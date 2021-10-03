;;; dank-oauth.el --- Major mode for browsing Reddit

;; Copyright (C) 2021 John Louis Del Rosario

;; Author: John Louis Del Rosario <john2x@gmail.com>
;; Version: 0.1.5
;; Keywords: reddit, social

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

(defvar dank-oauth-auth-token nil)
(defvar dank-oauth-access-token nil)
(defvar dank-oauth-refresh-token nil)

(defvar dank-oauth-redirect-server nil)
(defvar dank-oauth-auth-url "https://www.reddit.com/api/v1/authorize")
(defvar dank-oauth-token-url "https://www.reddit.com/api/v1/access_token")
(defvar dank-oauth-scope "identity history mysubreddits read wikiread")
(defvar dank-oauth-redirect-url (concat "http://localhost:" (number-to-string dank-oauth-redirect-port) dank-oauth-redirect-path))
(defvar dank-oauth-state-nonce nil
  "Random string for the `state' parameter.")

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
  (let ((oauth2-token-file dank-oauth-token-file))
    (setq dank-oauth-state-nonce (dank-oauth--random-string 10)
          dank-oauth-auth-token nil
          dank-oauth-access-token nil
          dank-oauth-refresh-token nil)

    ;; start the redirect server
    (dank-oauth-stop-redirect-server)
    (dank-oauth-start-redirect-server)

    ;; replace the oauth2-request-authorization function with our own
    ;; so we don't have to prompt the user for the token
    (unless (advice-member-p #'dank-oauth-request-authorization 'oauth2-request-authorization)
      (advice-add 'oauth2-request-authorization :override #'dank-oauth-request-authorization))

    ;; start the oauth2 dance
    ;; and wait for the redirect server to receive the token
    (oauth2-auth-and-store dank-oauth-auth-url dank-oauth-token-url
                           dank-oauth-scope dank-oauth-client-id nil
                           dank-oauth-redirect-url dank-oauth-state-nonce)

    ;; clean up oauth2-request-authorization
    (when (advice-member-p #'dank-oauth-request-authorization 'oauth2-request-authorization)
      (advice-remove 'oauth2-request-authorization #'dank-oauth-request-authorization))))

(defun dank-oauth-request-authorization (auth-url client-id &optional scope state redirect-uri)
  "Like `oauth2-request-authorization' but doesn't prompt for the code.
Instead, the code will be set by the redirect server."
  (browse-url (concat auth-url
                      (if (string-match-p "\?" auth-url) "&" "?")
                      "client_id=" (url-hexify-string client-id)
                      "&response_type=token" ;; "implicit" auth flow
                      "&redirect_uri=" (url-hexify-string (or redirect-uri "urn:ietf:wg:oauth:2.0:oob"))
                      (if scope (concat "&scope=" (url-hexify-string scope)) "")
                      (if state (concat "&state=" (url-hexify-string state)) "")))
  (dank-oauth-wait-for-auth-token))

(defun dank-oauth-wait-for-auth-token ()
  "Wait for `dank-oauth-auth-token' to be set for up to `dank-oauth-wait-for-token-timeout' seconds.
Once it's set, stop the `dank-oauth-redirect-server'."
  (cl-loop repeat dank-oauth-wait-for-token-timeout
           do (progn
                (when dank-oauth-auth-token
                  (message "Auth token received.")
                  (cl-return))
                (message "Waiting for auth token to be set... Press C-g to stop waiting.")
                (sleep-for 1))
           finally (dank-oauth-stop-redirect-server))
  dank-oauth-auth-token)

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
                    (if (and auth-token state (string-equal (cdr state) dank-oauth-state-nonce))
                        (progn
                          (setq dank-oauth-auth-token (cdr auth-token))
                          (process-send-string process (format "Received authorization token %s. You can now close this window and go back to Emacs." (cdr auth-token))))
                      (progn
                        (setq dank-oauth-auth-token nil)
                        (process-send-string process "Failed to receive valid authorization token. Please try again."))))))))
           dank-oauth-redirect-port))))

(defun dank-oauth-stop-redirect-server ()
  "Stop the redirect web server."
  (message "Stopping dank-mode oauth redirect server...")
  (when dank-oauth-redirect-server
    (ws-stop dank-oauth-redirect-server))
  (setq dank-oauth-redirect-server nil))

(provide 'dank-oauth)

;;; dank-oauth.el ends here
