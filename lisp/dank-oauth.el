;;; dank-oauth.el --- Major mode for browsing Reddit

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

(defcustom dank-oauth-client-id "Q83RuROYKjRiQnYqsI1jVg"
  "The Reddit installed app client id. Not a secret."
  :type 'string
  :group 'dank-mode)

(defcustom dank-oauth-token-file (expand-file-name "~/.emacs.d/dank-mode/oauth2.plstore")
  "File path where to store OAuth tokens."
  :type 'string
  :group 'dank-mode)

(defcustom dank-oauth-redirect-port 36227
  "Port number where the web server to receive the OAuth2 redirect will run."
  :type 'number
  :group 'dank-mode)

(defcustom dank-oauth-redirect-path "/dank-mode/oauth/redirect"
  "Path to receive the OAuth2 redirect."
  :type 'string
  :group 'dank-mode)

(defcustom dank-oauth-wait-for-token-timeout 30
  "Number of seconds to wait for the access token to be set at the end of the OAuth2 dance."
  :type 'number
  :group 'dank-mode)

(defvar dank-oauth--token-data nil)
(defvar dank-oauth--auth-token nil)
(defvar dank-oauth--state-nonce nil
  "Random string for the `state' parameter.")

(defvar dank-oauth-redirect-server nil)
(defvar dank-oauth-auth-url "https://www.reddit.com/api/v1/authorize")
(defvar dank-oauth-token-url "https://www.reddit.com/api/v1/access_token")
(defvar dank-oauth-scope "identity history mysubreddits read wikiread")
(defvar dank-oauth-redirect-url (concat "http://localhost:" (number-to-string dank-oauth-redirect-port) dank-oauth-redirect-path))

(defun dank-oauth--random-string (n)
  "Return random alpha number string with length N."
  (if (= 0 n)
      (concat "-" (number-to-string (float-time)))
    (concat (dank-oauth--random-alnum) (dank-oauth--random-string (1- n)))))

(defun dank-oauth--random-alnum ()
  "Return random single character alpha numeric string."
  (let* ((alnum "abcdefghijklmnopqrstuvwxyz0123456789")
         (i (% (abs (random)) (length alnum))))
    (substring alnum i (1+ i))))

(defun dank-oauth-start ()
  "Start OAuth2 dance with Reddit using `dank-oauth-client-id'."
  (interactive)
  ;; delete the existing token
  (when (file-exists-p (expand-file-name dank-oauth-token-file))
    (delete-file (expand-file-name dank-oauth-token-file)))

  (setq dank-oauth--state-nonce (dank-oauth--random-string 10)
        dank-oauth--auth-token nil)

  ;; start the redirect server
  (dank-oauth-stop-redirect-servers)
  (dank-oauth-start-redirect-server)

  ;; start the oauth2 dance
  ;; and wait for the redirect server to receive the token
  (let ((oauth2-token-file dank-oauth-token-file))
    ;; temporarily override these functions with our own
    (cl-letf (((symbol-function 'oauth2-make-access-request) 'dank-oauth--make-access-request)
              ((symbol-function 'oauth2-request-authorization) 'dank-oauth--request-authorization))
      (oauth2-auth-and-store dank-oauth-auth-url dank-oauth-token-url
                             dank-oauth-scope dank-oauth-client-id nil
                             dank-oauth-redirect-url dank-oauth--state-nonce))))

(defun dank-oauth--request-authorization (auth-url client-id &optional scope state redirect-uri)
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
  (dank-oauth-wait-for-auth-token)
  (dank-oauth-stop-redirect-servers)
  dank-oauth--auth-token)

(defun dank-oauth--make-access-request (url data)
  "Like `oauth2-make-access-request' but provides Authorization header."
  (let ((url-request-method "POST")
        (url-request-data data)
        (url-request-extra-headers
         `(("Authorization" . ,(concat "Basic " (base64-encode-string (concat dank-oauth-client-id ":"))))
           ("Content-Type" . "application/x-www-form-urlencoded"))))
    (with-current-buffer (url-retrieve-synchronously url)
      (let ((data (oauth2-request-access-parse)))
        (kill-buffer (current-buffer))
        data))))

(defun dank-oauth-wait-for-auth-token ()
  "Wait for `dank-oauth--auth-token' to be set for up to `dank-oauth-wait-for-token-timeout' seconds.
Once it's set, stop the `dank-oauth-redirect-server'."
  (cl-loop repeat dank-oauth-wait-for-token-timeout
           do (progn
                (when dank-oauth--auth-token
                  (message "Auth token received.")
                  (cl-return))
                (message "Waiting for auth token to be set... Press C-g to stop waiting.")
                (sleep-for 1))
           finally (dank-oauth-stop-redirect-servers))
  dank-oauth--auth-token)

(defun dank-oauth-read-from-disk ()
  "Read the oauth tokens from the plstore on disk."
  (when (file-exists-p (expand-file-name dank-oauth-token-file))
    (let* ((store (plstore-open (expand-file-name dank-oauth-token-file)))
           (id (oauth2-compute-id dank-oauth-auth-url dank-oauth-token-url dank-oauth-scope))
           (token (cdr (plstore-get store id))))
      (when token
        (message "Reading %s" dank-oauth-token-file)
        (setq dank-oauth--token-data
              (make-oauth2-token :plstore store
                                 :plstore-id id
                                 :client-id dank-oauth-client-id
                                 :client-secret ""
                                 :access-token (plist-get token :access-token)
                                 :refresh-token (plist-get token :refresh-token)
                                 :token-url dank-oauth-token-url
                                 :access-response (plist-get token :access-response)))))))

(defun dank-oauth-token ()
  "Get the access token. Refresh it if needed."
  (unless dank-oauth--token-data
    (dank-oauth-read-from-disk))
  (when dank-oauth--token-data
    (dank-oauth-token-refresh)
    (oauth2-token-access-token dank-oauth--token-data)))

(defun dank-oauth-token-needs-refresh-p (&optional duration)
  "Return non-nil if the token stored in `dank-oauth-token-file' needs to be refreshed.
This is a hack. We check the the modification time of the plstore
file and see if it is more than 3300 or DURATION seconds old."
  (when (file-exists-p dank-oauth-token-file)
    (let* ((update-time (time-convert (file-attribute-modification-time (file-attributes dank-oauth-token-file)) 'integer))
           (expiry-time (+ update-time (or duration 3300))))
      (>= (float-time) expiry-time))))

(defun dank-oauth-token-refresh (&optional force-refresh)
  "Refresh the access token in `dank-oauth-token-file' when it is about to expire or if FORCE-REFRESH is non-nil."
  (when (or force-refresh (dank-oauth-token-needs-refresh-p))
    (message "Refreshing oauth token...")
    (let ((oauth2-token-file dank-oauth-token-file))
      ;; temporarily override these functions with our own
      (cl-letf (((symbol-function 'oauth2-make-access-request) 'dank-oauth--make-access-request))
        (setq dank-oauth--token-data (oauth2-refresh-access dank-oauth--token-data))))))

(defun dank-oauth-start-redirect-server ()
  "Start the web server to receive the OAuth2 redirect request."
  (message "Starting dank-mode oauth redirect server on port %d..." dank-oauth-redirect-port)
  (if dank-oauth-redirect-server
      dank-oauth-redirect-server
    (setq dank-oauth-redirect-server
          (ws-start
           `(((:GET . ,dank-oauth-redirect-path) .
              (lambda (request)
                (with-slots (process context headers) request
                  (ws-response-header process 200 '("Content-type" . "text/html"))
                  (let ((auth-token (assoc "code" (cdr headers)))
                        (state (assoc "state" (cdr headers))))
                    (if (and auth-token state (string-equal (cdr state) dank-oauth--state-nonce))
                        (progn
                          (setq dank-oauth--auth-token (cdr auth-token))
                          (process-send-string process (format "Received authorization token %s. You can now close this window and go back to Emacs." (cdr auth-token))))
                      (progn
                        (setq dank-oauth--auth-token nil)
                        (process-send-string process "Failed to receive valid authorization token. Please try again."))))))))
           dank-oauth-redirect-port))))

(defun dank-oauth-stop-redirect-servers ()
  "Stop all redirect web servers."
  (mapc (lambda (s)
          (when (= (port s) dank-oauth-redirect-port)
            (message "Stopping dank-mode oauth redirect server...")
            (ws-stop s)))
        ws-servers)
  (setq dank-oauth-redirect-server nil))

(provide 'dank-oauth)

;;; dank-oauth.el ends here
