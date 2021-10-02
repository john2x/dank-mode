;;; dank-auth.el --- Major mode for browsing Reddit

;; Copyright (C) 2021 John Louis Del Rosario

;; Author: John Louis Del Rosario <john2x@gmail.com>
;; Version: 0.1.5
;; Keywords: reddit, social

;;; Commentary:

;; This file defines functions for authenticating with Reddit.

;;; Code:

(require 'json)
(require 'plstore)
(require 'dank-url)
(require 'dank-utils)


(defcustom dank-auth-file (expand-file-name "~/.emacs.d/dank-mode/auth.json")
  "Path to a JSON file containing your Reddit auth info. DEPRECATED.
Refer to auth.example.json included in this package for example format."
  :type 'string
  :group 'dank-mode)

(defcustom dank-auth-plstore (expand-file-name "~/.emacs.d/dank-mode/auth.plist")
  "Path to a plist file containing your Reddit auth info."
  :type 'string
  :group 'dank-mode)

(defcustom dank-auth-user-agent "Emacs dank-mode /u/"
  "User agent value to use when requesting the Reddit API.
This can be set via `dank-auth-file'.  This variable is optional, and
by default will be a concatenation of the string \"Emacs dank-mode /u/\" plus your
username."
  :type 'string
  :group 'dank-mode)

(defvar dank-auth--token-expiry-threshold-seconds 300
  "The threshold in seconds to consider an access token as invalid.")

(defvar dank-auth--credentials nil)
(defvar dank-auth--token-storage nil)

(define-error 'dank-auth-error "dank-auth error" 'error)
(define-error 'dank-auth-token-refresh-error "Failed to refresh access token" 'dank-auth-error)

(defun dank-auth-read-from-disk ()
  "Read the auth secrets from disk.
If the plstore file is available, use that.  Otherwise, if the
legacy json file is available, use that and create a new plstore
file from it."
  (setq dank-auth--credentials
        (or (dank-auth--read-from-plstore (expand-file-name dank-auth-plstore))
            (if-let ((credentials (dank-auth--read-from-json (expand-file-name dank-auth-file))))
                (progn (dank-auth--write-to-plstore credentials dank-auth-plstore)  ;; migrate to new plstore file
                       (delete-file (expand-file-name dank-auth-file)) ;; clean up the legacy json file
                       credentials)))))

(defun dank-auth--read-from-plstore (path)
  "Read auth secrets from plstore in PATH.
If the plstore is encrypted, user will be prompted for the passphrase."
  (when (file-exists-p path)
    (let* ((store (plstore-open path))
           (data (plstore-get store "dank-mode")))
      (message "Using credentials in %s" path)
      (cdr data))))

(defun dank-auth--read-from-json (path)
  "Read auth secrets from the legacy JSON in PATH."
  (when (file-exists-p path)
    (let* ((json-object-type 'plist)
           (data (json-read-file path)))
      (message "Using credentials in %s" path)
      data)))

(defun dank-auth--write-to-plstore (credentials path &optional encrypt)
  "Write CREDENTIALS to plstore in PATH.
If ENCRYPT is non-nil, prompt the user for a passphrase."
  (let ((store (plstore-open (expand-file-name path))))
    (if encrypt
        (plstore-put store "dank-mode"
                     `(:oauthClientId ,(plist-get credentials :oauthClientId) :username ,(plist-get credentials :username))
                     `(:oauthClientSecret ,(plist-get credentials :oauthClientSecret) :password ,(plist-get credentials :password)))
      (plstore-put store "dank-mode" credentials nil))
    (plstore-save store)))

(defun dank-auth-prompt ()
  "Prompt for Reddit credentials.
Client id, client secret, username, and password.  These will be
stored in `dank-auth-plstore'.  Offers the option to encrypt the
secret and password with a passphrase when it is written to
disk."
  (interactive)
  (when (or (not (file-exists-p (expand-file-name dank-auth-plstore)))
            (yes-or-no-p (concat (expand-file-name dank-auth-plstore) " exists. Overwrite it?")))
    (let (client-id client-secret username password encrypt store)
      (setq client-id (read-string "Reddit API Client ID: "))
      (setq client-secret (read-passwd "Reddit API Client Secret: "))
      (setq username (read-string "Reddit username: "))
      (setq password (read-passwd "Reddit password: "))
      (setq encrypt (yes-or-no-p "Encrypt your Reddit secret and password when it's stored to disk (you will be prompted for a passphrase)? "))
      (setq store (plstore-open (expand-file-name dank-auth-plstore)))
      (dank-auth--write-to-plstore `(:oauthClientId ,client-id :oauthClientSecret ,client-secret :username ,username :password ,password)
                                   dank-auth-plstore encrypt))))

(defun dank-auth-token-refresh (&optional force-refresh)
  "Retrieve new token data and store it in dank-auth--token-storage.
When FORCE-REFRESH is non-nil, then force the refresh."
  (when (and dank-auth--credentials (or force-refresh (not (dank-auth--token-valid-p))))
    (let* ((authorization (base64-encode-string (concat (plist-get dank-auth--credentials :oauthClientId) ":"
                                                        (plist-get dank-auth--credentials :oauthClientSecret))))
           (full-url "https://www.reddit.com/api/v1/access_token")
           (url-user-agent (concat dank-auth-user-agent (plist-get dank-auth--credentials :username)))
           (url-request-method "POST")
           (url-request-extra-headers `(("Authorization" . ,(concat "Basic " authorization))
                                        ("Content-Type" . "application/x-www-form-urlencoded")))
           (url-request-data (dank-url-encode-alist `(("grant_type" . "password")
                                                      ("username" . ,(plist-get dank-auth--credentials :username))
                                                      ("password" . ,(plist-get dank-auth--credentials :password)))))
           (response-buf (url-retrieve-synchronously full-url)))
      ;; cleanup
      (setq url-request-method nil
            url-request-extra-headers nil
            url-request-data nil)
      (with-current-buffer response-buf
        (let* ((response-status-code (dank-url-response-status-code))
               (response-content-type (dank-url-response-header "content-type"))
               (response-content (dank-url-response-uncompress))
               (response-json (if (string-match-p "^application/json" response-content-type)
                                  (json-parse-string response-content :object-type 'plist :null-object nil))))
          (if (and (= response-status-code 200) (plist-get response-json :expires_in))
              (let ((expiry (+ (float-time) (plist-get response-json :expires_in))))
                (setq dank-auth--token-storage (plist-put response-json :expiry expiry))
                response-json)
            (signal 'dank-auth-token-refresh-error
                    `(,full-url ,url-request-method ,response-status-code ,response-content))))))))

(defun dank-auth-token ()
  "Return the access token stored in dank-auth--token-storage.
If the token is no longer valid, then attempt to retrieve a new token."
  (if (dank-auth--token-valid-p)
      (plist-get dank-auth--token-storage :access_token)
    (progn (dank-auth-token-refresh)
           (plist-get dank-auth--token-storage :access_token))))

(defun dank-auth--token-valid-p ()
  "Return t if the access token in `dank-auth--token-storage' is still valid.
It checks if the token expiry falls below
`dank-auth--token-expiry-threshold-seconds'.

If `ATTEMPT-REQUEST' is non-nil, then in addition this function will attempt
to do an actual request to Reddit's API using the current access token."
  (when dank-auth--token-storage
    (> (- (plist-get dank-auth--token-storage :expiry) (float-time))
       dank-auth--token-expiry-threshold-seconds)))

(provide 'dank-auth)

;;; dank-auth.el ends here
