(require 'request)
(require 'json)
(require 'dank-utils)
(require 'dank-cache)
(require 'dank-auth)

(cl-defstruct dank-post id slug url title body score user date)
(cl-defstruct dank-comment id post-id url body score user date)

(defconst dank-backend-host "https://oauth.reddit.com")

(defvar-local dank-backend-buffer-history nil)
(defvar-local dank-backend-buffer-url nil)

(define-error 'dank-backend-error "dank-backend error" 'error)

(define-error 'dank-backend-request-error "Request failed" 'dank-backend-error)

(defun dank-backend--find-property (request-args property)
  "Find the PROPERTY and return its value from REQUEST-ARGS."
  (when request-args
    (if (eq (car request-args) property)
        (car (cdr request-args))
      (dank-backend--find-property (cdr request-args) property))))

(defun dank-backend--cache-key (request-args)
  "Return a cache key from REQUEST-ARGS."
  (let ((url (car request-args))
        (params (dank-backend--find-property request-args :params)))
    (if params
        (dank-cache-key (concat url (if (string-match-p "\\?" url) "&" "?")
                                (request--urlencode-alist params)))
      (dank-cache-key url))))

(defun dank-backend-authenticated-request (&rest request-args)
  "Perform a synchronous `request' with REQUEST-ARGS and `dank-auth-token'.
The first element in request-args (the _relative_ request url) will be prependend with `dank-backend-host'."
  (let ((key (dank-backend--cache-key request-args)))
    (if (dank-cache-key-exists key)
        (let ((json-object-type 'plist))
          (json-read-from-string (dank-cache-get key)))
      (let* ((full-url (concat dank-backend-host (car request-args)))
             (request-args (cons full-url (cdr request-args)))  ;; replace the relative url with the full-url
             (authorization (concat "Bearer " (dank-auth-token)))
             (request-args (append request-args `(:headers (("Authorization" . ,authorization)
                                                            ("User-Agent" . ,dank-auth-user-agent))
                                                           :parser buffer-string
                                                           :sync t)))
             (resp (apply 'request request-args))
             (resp-data (request-response-data resp))
             (resp-error (request-response-error-thrown resp)))
        (if resp-error
            (signal 'dank-backend-request-error
                    `(,(plist-get (cdr request-args) :type) ,full-url ,resp-data))
          (let ((json-object-type 'plist))
            (dank-cache-set key resp-data)
            (json-read-from-string resp-data)))))))


(defun dank-backend-post-listing (subreddit sorting &rest request-params)
  "Fetch authenticated user's SUBREDDIT posts sorted by SORTING.

SUBREDDIT can be nil to retrieve the user's front page listing.

SORTING must be a symbol of either 'hot, 'new, 'rising, 'top, or 'controversial.

REQUEST-PARAMS is plist of request parameters that Reddit's 'listing' API takes.
e.g. (:after \"xxx\" :limit 25)
Valid keywords are: :after (string), :before (string), :limit (integer).
If both :after and :before are provided, :after takes precedence and :before is ignored."
  (let ((after (plist-get request-params :after))
        (before (plist-get request-params :before))
        (limit (plist-get request-params :limit))
        (count (plist-get request-params :count))
        (url (concat (when subreddit (concat "/r/" subreddit)) "/" (symbol-name sorting))))
    (let* ((params (if before (cons `(before . ,before) '()) '()))
           (params (if limit (cons `(limit . ,limit) params) params))
           (params (if count (cons `(count . ,count) params) params))
           (params (if after (cons `(after . ,after) (assq-delete-all 'before params)) params))
           (resp (dank-backend-authenticated-request url :type "GET" :params params)))
      (plist-get (plist-get resp :data) :children))))

(defun dank-backend-my-subreddits-listing (&rest request-params)
  "Fetch authenticated user's subscribed subreddits.
REQUEST-PARAMS is a plist of request parameters that Reddit's 'listing' API takes."
  (let ((after (plist-get request-params :after))
        (before (plist-get request-params :before))
        (limit (plist-get request-params :limit)))
    (let* ((params (if before (cons `(before . ,before) '() '())))
           (params (if limit (cons `(limit . ,limit) params) params))
           (params (if after (cons `(after . , after) (assq-delete-all 'before params)) params))
           (resp (dank-backend-authenticated-request "/subreddits/mine/subscriber" :type "GET" :params params)))
      resp)))


(defun dank-backend-post-and-comments-listing (subreddit post-id sorting &rest request-params)
  "Fetch post details and comments of a post in SUBREDDIT with POST-ID.

SORTING must be a symbol of either 'hot, 'new, 'old, 'top, 'random, 'qa, 'confidence, 'live, or 'controversial.

REQUEST-PARAMS is plist of request parameters that Reddit's 'listing' API takes.
e.g. (:depth 10 :limit 25)
Valid keywords are: :depth (integer), :limit (integer)."
  (let ((depth (plist-get request-params :depth))
        (limit (plist-get request-params :limit))
        (showedits (plist-get request-params :showedits))
        (url (concat (when subreddit (concat "/r/" subreddit)) "/comments/" post-id)))
    (let* ((params (if depth (cons `(depth . ,depth) '()) '()))
           (params (if limit (cons `(limit . ,limit) params) params))
           (params (cons `(sorting . ,(symbol-name sorting)) params))
           (resp (dank-backend-authenticated-request url :type "GET" :params params))
           (post (aref (plist-get (plist-get (aref resp 0) :data) :children) 0))
           (comments (plist-get (plist-get (aref resp 1) :data) :children)))
      `(,post . ,comments))))

(provide 'dank-backend)
