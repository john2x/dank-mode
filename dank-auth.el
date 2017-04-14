(require 'request)
(require 'json)
(require 'dank-utils)


(defcustom dank-auth-file nil
  "Path to a JSON file containing your Reddit auth info.
Refer to auth.example.json included in this package for example format.")
(defcustom dank-auth-username nil
  "Your Reddit username.  This can be set via `dank-auth-file'.")
(defcustom dank-auth-password nil
  "Your Reddit password.  This can be set via `dank-auth-file'.")
(defcustom dank-auth-oauth-client-id nil
  "Your Reddit OAuth client id.  This can be set via `dank-auth-file'.
For instructions on how to retrieve the client id, refer to this package's
README file.")
(defcustom dank-auth-oauth-client-secret nil
  "Your Reddit OAuth client secret.
This can be set via `dank-auth-file'.
For instructions on how to retrieve the client secret, refer to this package's
README file.")
(defcustom dank-auth-user-agent nil
  "User agent value to use when requesting the Reddit API.
This can be set via `dank-auth-file'.  This variable is optional, and
by default will be a concatenation of the string \"dank-mode/\" plus your
username.")
(defcustom dank-auth--token-expiry-threshold-seconds 300
  "The threshold in seconds to consider an access token as invalid.")

(defvar dank-auth--token-storage nil)

(setq dank-auth-file "auth.json")


(defun dank-auth-load-auth-vars-from-file (path)
  "Read and set auth values from PATH."
  (let ((data (json-read-file path)))
    (setq dank-auth-username (alist-get 'username data))
    (setq dank-auth-password (alist-get 'password data))
    (setq dank-auth-oauth-client-id (alist-get 'oauthClientId data))
    (setq dank-auth-oauth-client-secret (alist-get 'oauthClientSecret data))
    (setq dank-auth-user-agent (or (alist-get 'userAgent data)
                                     (concat "dank-mode/" dank-auth-username)))))

(defun dank-auth--configured-p ()
  "Return t if the following auth vars are set.
- dank-auth-username
- dank-auth-password
- dank-auth-oauth-client-id
- dank-auth-oauth-client-secret"
  (and dank-auth-username dank-auth-password
       dank-auth-oauth-client-id dank-auth-oauth-client-secret))

(defun dank-auth-token-refresh ()
  "Retrieve new token data and store it in dank-auth--token-storage."
  (when (and (dank-auth--configured-p) (not (dank-auth--token-valid-p)))
    (let* ((authorization (base64-encode-string (concat dank-auth-oauth-client-id ":"
                                                        dank-auth-oauth-client-secret)))
           (resp (request
                  "https://www.reddit.com/api/v1/access_token"
                  :type "POST"
                  :data `(("grant_type" . "password")
                          ("username" . ,dank-auth-username)
                          ("password" . ,dank-auth-password))
                  :headers `(("Authorization" . ,(concat "Basic " authorization))
                             ("User-Agent" . ,dank-auth-user-agent))
                  :parser 'json-read
                  :sync t))
           (resp-data (request-response-data resp)))
      (if (request-response-error-thrown resp)
          (dank-warning 'dank-auth "failed to refresh Reddit token. Error %s" resp-data)
        (let ((expiry (+ (float-time) (cdr (assq 'expires_in resp-data)))))
          (setq dank-auth--token-storage (cons `(expiry . ,expiry) resp-data)))))))

(defun dank-auth-token ()
  "Return the access token stored in dank-auth--token-storage.
If the token is no longer valid, then attempt to retrieve a new token."
  (if (dank-auth--token-valid-p)
      (alist-get 'access_token dank-auth--token-storage)
    (progn (dank-auth-token-refresh)
           (alist-get 'access_token dank-auth--token-storage))))

(defun dank-auth--token-valid-p (&optional attempt-request)
  "Return t if the access token in `dank-auth--token-storage' is still valid.
It checks if the token expiry falls below
`dank-auth--token-expiry-threshold-seconds'.

If `ATTEMPT-REQUEST' is non-nil, then in addition this function will attempt
to do an actual request to Reddit's API using the current access token."
  (when dank-auth--token-storage
    (> (- (alist-get 'expiry dank-auth--token-storage) (float-time))
       dank-auth--token-expiry-threshold-seconds)))

(provide 'dank-auth)
