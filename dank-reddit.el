(require 'request)
(require 'json)

(defconst dank-reddit-host "https://oauth.reddit.com")

(defcustom dank-reddit-auth-file nil
  "Path to a JSON file containing your Reddit auth info.
Refer to auth.example.json included in this package for example format.")
(defcustom dank-reddit-username nil
  "Your Reddit username.  This can be set via `dank-reddit-auth-file'.")
(defcustom dank-reddit-password nil
  "Your Reddit password.  This can be set via `dank-reddit-auth-file'.")
(defcustom dank-reddit-oauth-client-id nil
  "Your Reddit OAuth client id.  This can be set via `dank-reddit-auth-file'.
For instructions on how to retrieve the client id, refer to this package's
README file.")
(defcustom dank-reddit-oauth-client-secret nil
  "Your Reddit OAuth client secret.
This can be set via `dank-reddit-auth-file'.
For instructions on how to retrieve the client secret, refer to this package's
README file.")
(defcustom dank-reddit-user-agent nil
  "User agent value to use when requesting the Reddit API.
This can be set via `dank-reddit-auth-file'.  This variable is optional, and
by default will be a concatenation of the string \"dank-mode/\" plus your
username.")
(defcustom dank-reddit--token-expiry-threshold-seconds 300
  "The threshold in seconds to consider an access token as invalid.")

(defvar dank-reddit--token-storage nil)

(setq dank-reddit-auth-file "auth.example.json")

(defun dank-reddit-load-auth-vars-from-file (path)
  "Read and set auth values from PATH."
  (let ((data (json-read-file path)))
    (setq dank-reddit-username (alist-get 'username data))
    (setq dank-reddit-password (alist-get 'password data))
    (setq dank-reddit-oauth-client-id (alist-get 'oauthClientId data))
    (setq dank-reddit-oauth-client-secret (alist-get 'oauthClientSecret data))
    (setq dank-reddit-user-agent (or (alist-get 'userAgent data)
                                     (concat "dank-mode/" dank-reddit-username)))))

(defun dank-reddit--auth-configured-p ()
  "Return t if the following auth vars are set.
- dank-reddit-username
- dank-reddit-password
- dank-reddit-oauth-client-id
- dank-reddit-oauth-client-secret"
  (and dank-reddit-username dank-reddit-password
       dank-reddit-oauth-client-id dank-reddit-oauth-client-secret))

(defun dank-reddit-token-refresh ()
  "Retrieve new token data and store it in dank-reddit--token-storage."
  (when (and (dank-reddit--auth-configured-p) (not (dank-reddit--token-valid-p)))
    (let* ((authorization (base64-encode-string (concat dank-reddit-oauth-client-id ":"
                                                        dank-reddit-oauth-client-secret)))
           (resp (request
                  "https://www.reddit.com/api/v1/access_token"
                  :type "POST"
                  :data `(("grant_type" . "password")
                          ("username" . ,dank-reddit-username)
                          ("password" . ,dank-reddit-password))
                  :headers `(("Authorization" . ,(concat "Basic " authorization))
                             ("User-Agent" . ,dank-reddit-user-agent))
                  :parser 'json-read
                  :sync t))
           (resp-data (request-response-data resp)))
      (if (request-response-error-thrown resp)
          (warn "dank-reddit failed to refresh Reddit token. Error %s" resp-data)
        (let ((expiry (+ (float-time) (cdr (assq 'expires_in resp-data)))))
          (setq dank-reddit--token-storage (cons `(expiry . ,expiry) resp-data)))))))

(defun dank-reddit-token ()
  "Return the access token stored in dank-reddit--token-storage.
If the token is no longer valid, then attempt to retrieve a new token."
  (if (dank-reddit--token-valid-p)
      (alist-get 'access_token dank-reddit--token-storage)
    (progn (dank-reddit-token-refresh)
           (alist-get 'access_token dank-reddit--token-storage))))

(defun dank-reddit--token-valid-p (&optional attempt-request)
  "Return t if the access token in `dank-reddit--token-storage' is still valid.
It checks if the token expiry falls below
`dank-reddit--token-expiry-threshold-seconds'.

If `ATTEMPT-REQUEST' is non-nil, then in addition this function will attempt
to do an actual request to Reddit's API using the current access token."
  (when dank-reddit--token-storage
    (> (- (alist-get 'expiry dank-reddit--token-storage) (float-time))
       dank-reddit--token-expiry-threshold-seconds)))

(defun dank-reddit-authenticated-request (&rest request-args)
  "Perform a synchronous `request' with REQUEST-ARGS with `dank-reddit-token' in the headers."
  (let* ((authorization (concat "Bearer " (dank-reddit-token)))
         (request-args (append request-args `(:headers (("Authorization" . ,authorization)
                                                        ("User-Agent" . ,dank-reddit-user-agent))
                                              :parser json-read
                                              :sync t)))
         (resp (apply 'request request-args))
         (resp-data (request-response-data resp))
         (resp-error (request-response-error-thrown resp)))
    (if resp-error
        (warn "dank-reddit-authenticated-request failed. %s" resp-data)
      resp-data)))


(defun dank-reddit-listings (listing &rest request-params)
  ())
