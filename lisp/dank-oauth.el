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

(defcustom dank-oauth-redirect-path "dank-mode/oauth/redirect"
  "Path to receive the OAuth2 redirect."
  :type 'string
  :group 'dank-mode)

(defcustom dank-oauth-wait-for-token-timeout 30
  "Number of seconds to wait for the access token to be set at the end of the OAuth2 dance."
  :type 'number
  :group 'dank-mode)

(defvar dank-oauth-token nil)
(defvar dank-oauth-refresh-token nil)
(defvar dank-oauth-redirect-server nil)
(defvar dank-oauth-auth-url "https://www.reddit.com/api/v1/authorize")
(defvar dank-oauth-token-url "https://www.reddit.com/api/v1/access_token")
(defvar dank-oauth-scope "identity history mysubreddits read wikiread")
(defvar dank-oauth-redirect-url (concat "http://localhost:" (number-to-sring dank-oauth-redirect-port) "/" dank-oauth-redirect-path))
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
    (dank-oauth-start-redirect-server)
    (setq dank-oauth-state-nonce (dank-oauth--random-string 10))
    (oauth2-auth-and-store dank-oauth-auth-url dank-oauth-token-url
                           dank-oauth-scope dank-oauth-client-id nil
                           dank-oauth-redirect-url dank-oauth-state-nonce)
    (dank-oauth-wait-for-token dank-oauth-redirect-server)))

(defun dank-oauth-auth-and-store ()
  "Perform the OAuth2 dance but don't prompt for the token.
Instead, wait for `dank-oauth-token' to be set by the redirect server.")

(defun dank-oauth-wait-for-token ()
  "Wait for `dank-oauth-token' to be set for up to `dank-oauth-wait-for-token-timeout' seconds.
Once it's set, stop the `dank-oauth-redirect-server'."
  (cl-loop repeat dank-oauth-wait-for-token-timeout
           do (progn
                (when dank-oauth-token
                  (message "OAuth token received.")
                  (ws-stop dank-oauth-redirect-server)
                  (setq dank-oauth-redirect-server nil)
                  cl-return)
                (message "Waiting for OAuth token to be set...")
                (sleep-for 3))))

(defun dank-oauth-start-redirect-server ()
  "Start the web server to receive the OAuth2 redirect request."
  (if dank-oauth-redirect-server
      dank-oauth-redirect-server
    (setq dank-oauth-redirect-server
          (ws-start
           `(((:GET . ,(concat "/" dank-oauth-redirect-path)) .
              (lambda (request)
                (with-slots (process headers) request
                  (ws-response-header process 200 '("Content-type" . "text/html"))
                  (process-send-string process
                                       (concat "URL Parameters:</br><table><tr>"
                                               (mapconcat (lambda (pair)
                                                            (format "<th>%s</th><td>%s</td>"
                                                                    (car pair) (cdr pair)))
                                                          (cl-remove-if-not (lambda (el) (stringp (car el)))
                                                                            headers)
                                                          "</tr><tr>")
                                               "</tr></table>"))))))
           dank-oauth-redirect-port))))

(provide 'dank-oauth)

;;; dank-oauth.el ends here
