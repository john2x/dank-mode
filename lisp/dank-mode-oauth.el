;;; dank-mode-oauth.el --- Major mode for browsing Reddit -*- lexical-binding: t -*-

;; Copyright (C) 2021 John Louis Del Rosario

;; Author: John Louis Del Rosario <john2x@gmail.com>
;; Version: 0.2.0
;; Keywords: reddit, social
;; Package-Requires: ((oauth2 "0.16") (web-server "0.1.2"))

;;; Commentary:

;; This file defines functions to perform the OAuth2 dance with Reddit
;; servers

;;; Code:

(require 'cl-lib)
(require 'oauth2)
(require 'web-server)
(require 'plstore)

(defcustom dank-mode-oauth-client-id "Q83RuROYKjRiQnYqsI1jVg"
  "The Reddit installed app client id. Not a secret.
You can change this to your own Reddit client id by going to
https://old.reddit.com/prefs/apps and creating an 'installed
app'."
  :type 'string
  :group 'dank-mode)

(defcustom dank-mode-oauth-token-file (expand-file-name "~/.emacs.d/dank-mode/oauth2.plstore")
  "File path where to store OAuth tokens."
  :type 'string
  :group 'dank-mode)

(defcustom dank-mode-oauth-redirect-port 36227
  "Port number where the web server to receive the OAuth2 redirect will run."
  :type 'number
  :group 'dank-mode)

(defcustom dank-mode-oauth-redirect-path "/dank-mode/oauth/redirect"
  "Path to receive the OAuth2 redirect."
  :type 'string
  :group 'dank-mode)

(defcustom dank-mode-oauth-wait-for-token-timeout 30
  "Number of seconds to wait for the access token to be set at the end of the OAuth2 dance."
  :type 'number
  :group 'dank-mode)

(defcustom dank-mode-oauth-encrypt-plstore nil
  "Encrypt the plstore file for the OAuth refresh token with a password.

Although it is recommended to keep this enabled, it is annoying
to have to retype the password every time the access token needs
to be refreshed."
  :type 'boolean
  :group 'dank-mode)

(defvar dank-mode-oauth--token-data nil)
(defvar dank-mode-oauth--auth-token nil)
(defvar dank-mode-oauth--state-nonce nil
  "Random string for the `state' parameter.")

(defvar dank-mode-oauth-redirect-server nil)
(defvar dank-mode-oauth-auth-url "https://www.reddit.com/api/v1/authorize")
(defvar dank-mode-oauth-token-url "https://www.reddit.com/api/v1/access_token")
(defvar dank-mode-oauth-scope "identity history mysubreddits read wikiread")
(defvar dank-mode-oauth-scope "identity history mysubreddits read wikiread vote edit save submit subscribe report")
(defvar dank-mode-oauth-redirect-url (concat "http://localhost:" (number-to-string dank-mode-oauth-redirect-port) dank-mode-oauth-redirect-path))

(defun dank-mode-oauth--random-string (n)
  "Return random alpha number string with length N."
  (if (= 0 n)
      (concat "-" (number-to-string (float-time)))
    (concat (dank-mode-oauth--random-alnum) (dank-mode-oauth--random-string (1- n)))))

(defun dank-mode-oauth--random-alnum ()
  "Return random single character alpha numeric string."
  (let* ((alnum "abcdefghijklmnopqrstuvwxyz0123456789")
         (i (% (abs (random)) (length alnum))))
    (substring alnum i (1+ i))))

(defun dank-mode-oauth-start ()
  "Start OAuth2 dance with Reddit using `dank-mode-oauth-client-id'."
  (interactive)
  ;; delete the existing token
  (when (file-exists-p (expand-file-name dank-mode-oauth-token-file))
    (delete-file (expand-file-name dank-mode-oauth-token-file)))

  (setq dank-mode-oauth--state-nonce (dank-mode-oauth--random-string 10)
        dank-mode-oauth--auth-token nil)

  ;; start the redirect server
  (dank-mode-oauth-stop-redirect-servers)
  (dank-mode-oauth-start-redirect-server)

  ;; start the oauth2 dance
  ;; and wait for the redirect server to receive the token
  (let ((oauth2-token-file dank-mode-oauth-token-file))
    ;; temporarily override these functions with our own
    (unwind-protect
        (progn
          ;; temporarily advice plstore-put so it understands dank-mode-oauth-encrypt-plstore
          (advice-add 'plstore-put :filter-args #'dank-mode-oauth--plstore-put-advice)
          ;; temporarily override these functions with our own
          (cl-letf (((symbol-function 'oauth2-make-access-request) 'dank-mode-oauth--make-access-request)
                    ((symbol-function 'oauth2-request-authorization) 'dank-mode-oauth--request-authorization))
            (oauth2-auth-and-store dank-mode-oauth-auth-url dank-mode-oauth-token-url
                                   dank-mode-oauth-scope dank-mode-oauth-client-id nil
                                   dank-mode-oauth-redirect-url dank-mode-oauth--state-nonce)))
      (advice-remove 'plstore-put #'dank-mode-oauth--plstore-put-advice))))


(defun dank-mode-oauth--plstore-put-advice (args)
  "Filter the arguments used for plstore-put. Combine KEYS and
SECRET-KEYS if `dank-mode-oauth-encrypt-plstore' is nil."
  (if dank-mode-oauth-encrypt-plstore
      args
    (list (car args) (cadr args) (append (caddr args) (cadddr args)) nil)))

(defun dank-mode-oauth--request-authorization (auth-url client-id &optional scope state redirect-uri)
  "Like `oauth2-request-authorization' but doesn't prompt for the code.
Instead, the code will be set by the redirect server."
  (browse-url (concat auth-url
                      (if (string-match-p "\?" auth-url) "&" "?")
                      "client_id=" (url-hexify-string client-id)
                      "&response_type=code"
                      "&redirect_uri=" (url-hexify-string (or redirect-uri "urn:ietf:wg:oauth:2.0:oob"))
                      "&duration=permanent"
                      (if scope (concat "&scope=" (url-hexify-string scope)) "")
                      (if state (concat "&state=" (url-hexify-string state)) "")))
  (dank-mode-oauth-wait-for-auth-token)
  (dank-mode-oauth-stop-redirect-servers)
  dank-mode-oauth--auth-token)

(defun dank-mode-oauth--make-access-request (url data)
  "Like `oauth2-make-access-request' but provides Authorization header."
  (let ((url-request-method "POST")
        (url-request-data data)
        (url-request-extra-headers
         `(("Authorization" . ,(concat "Basic " (base64-encode-string (concat dank-mode-oauth-client-id ":"))))
           ("Content-Type" . "application/x-www-form-urlencoded"))))
    (with-current-buffer (url-retrieve-synchronously url)
      (let ((data (oauth2-request-access-parse)))
        (kill-buffer (current-buffer))
        data))))

(defun dank-mode-oauth-wait-for-auth-token ()
  "Wait for `dank-mode-oauth--auth-token' to be set for up to `dank-mode-oauth-wait-for-token-timeout' seconds.
Once it's set, stop the `dank-mode-oauth-redirect-server'."
  (cl-loop repeat dank-mode-oauth-wait-for-token-timeout
           do (progn
                (when dank-mode-oauth--auth-token
                  (message "Auth token received.")
                  (cl-return))
                (message "Waiting for auth token to be set... Press C-g to stop waiting.")
                (sleep-for 1))
           finally (dank-mode-oauth-stop-redirect-servers))
  dank-mode-oauth--auth-token)

(defun dank-mode-oauth-read-from-disk ()
  "Read the oauth tokens from the plstore on disk."
  (when (file-exists-p (expand-file-name dank-mode-oauth-token-file))
    (let* ((store (plstore-open (expand-file-name dank-mode-oauth-token-file)))
           (id (oauth2-compute-id dank-mode-oauth-auth-url dank-mode-oauth-token-url dank-mode-oauth-scope))
           (token (cdr (plstore-get store id))))
      (when token
        (message "Reading %s" dank-mode-oauth-token-file)
        (setq dank-mode-oauth--token-data
              (make-oauth2-token :plstore store
                                 :plstore-id id
                                 :client-id dank-mode-oauth-client-id
                                 :client-secret ""
                                 :access-token (plist-get token :access-token)
                                 :refresh-token (plist-get token :refresh-token)
                                 :token-url dank-mode-oauth-token-url
                                 :access-response (plist-get token :access-response)))))))

(defun dank-mode-oauth-token ()
  "Get the access token. Refresh it if needed."
  (unless dank-mode-oauth--token-data
    (dank-mode-oauth-read-from-disk))
  (when dank-mode-oauth--token-data
    (dank-mode-oauth-token-refresh)
    (oauth2-token-access-token dank-mode-oauth--token-data)))

(defun dank-mode-oauth-token-needs-refresh-p (&optional duration)
  "Return non-nil if the token stored in `dank-mode-oauth-token-file' needs to be refreshed.
This is a hack. We check the the modification time of the plstore
file and see if it is more than 3300 or DURATION seconds old."
  (when (file-exists-p dank-mode-oauth-token-file)
    (let* ((update-time (time-convert (file-attribute-modification-time (file-attributes dank-mode-oauth-token-file)) 'integer))
           (expiry-time (+ update-time (or duration 3300))))
      (>= (float-time) expiry-time))))

(defun dank-mode-oauth-token-refresh (&optional force-refresh)
  "Refresh the access token in `dank-mode-oauth-token-file' when it is about to expire or if FORCE-REFRESH is non-nil."
  (when (or force-refresh (dank-mode-oauth-token-needs-refresh-p))
    (message "Refreshing oauth token...")
    (let ((oauth2-token-file dank-mode-oauth-token-file))
      (unwind-protect
          (progn
            ;; temporarily advice plstore-put so it understands dank-mode-oauth-encrypt-plstore
            (advice-add 'plstore-put :filter-args #'dank-mode-oauth--plstore-put-advice)
            ;; temporarily override these functions with our own
            (cl-letf (((symbol-function 'oauth2-make-access-request) 'dank-mode-oauth--make-access-request))
              (setq dank-mode-oauth--token-data (oauth2-refresh-access dank-mode-oauth--token-data))))
        (advice-remove 'plstore-put #'dank-mode-oauth--plstore-put-advice)))))

(defun dank-mode-oauth-start-redirect-server ()
  "Start the web server to receive the OAuth2 redirect request."
  (message "Starting dank-mode oauth redirect server on port %d..." dank-mode-oauth-redirect-port)
  (if dank-mode-oauth-redirect-server
      dank-mode-oauth-redirect-server
    (setq dank-mode-oauth-redirect-server
          (ws-start
           `(((:GET . ,dank-mode-oauth-redirect-path) .
              (lambda (request)
                (with-slots (process context headers) request
                  (ws-response-header process 200 '("Content-type" . "text/html"))
                  (let ((auth-token (assoc "code" (cdr headers)))
                        (state (assoc "state" (cdr headers))))
                    (if (and auth-token state (string-equal (cdr state) dank-mode-oauth--state-nonce))
                        (progn
                          (setq dank-mode-oauth--auth-token (cdr auth-token))
                          (process-send-string process "Received authorization token. You can now close this window and go back to Emacs."))
                      (progn
                        (setq dank-mode-oauth--auth-token nil)
                        (process-send-string process "Failed to receive valid authorization token. Please try again."))))))))
           dank-mode-oauth-redirect-port))))

(defun dank-mode-oauth-stop-redirect-servers ()
  "Stop all redirect web servers."
  (mapc (lambda (s)
          (when (= (port s) dank-mode-oauth-redirect-port)
            (message "Stopping dank-mode oauth redirect server...")
            (ws-stop s)))
        ws-servers)
  (setq dank-mode-oauth-redirect-server nil))

(provide 'dank-mode-oauth)

;;; dank-mode-oauth.el ends here
