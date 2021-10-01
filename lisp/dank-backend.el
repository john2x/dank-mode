;;; dank-backend.el --- Major mode for browsing Reddit

;; Copyright (C) 2021 John Louis Del Rosario

;; Author: John Louis Del Rosario <john2x@gmail.com>
;; Version: 0.1.0
;; Keywords: reddit, social

;;; Commentary:

;; This file defines functions for making requests to Reddit.

;;; Code:

(require 'url)
(require 'json)
(require 'dank-utils)
(require 'dank-auth)
(require 'dank-url)

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

(defun dank-backend-request (method path &optional url-params)
  "Perform a synchronous `request' with REQUEST-ARGS and `dank-auth-token'.
The first element in request-args (the _relative_ request url) will be prependend with `dank-backend-host'."
  (let* ((json-object-type 'plist)
         (encoded-url-params (dank-url-encode-alist url-params))
         (full-url (concat dank-backend-host path))
         (full-url (if encoded-url-params (concat full-url "?" encoded-url-params) full-url)))
      (let* ((token (dank-auth-token))
             (authorization (concat "Bearer " token))
             (url-user-agent dank-auth-user-agent)
             (url-request-extra-headers `(("Authorization" . ,authorization)
                                          ("Accept-Encoding" . "identity")))
             (response-buf (url-retrieve-synchronously full-url t t 60)))
        (with-current-buffer response-buf
          (if (= (dank-url-response-status-code) 200)
              (progn
                (json-parse-string (dank-url-response-uncompress) :object-type 'plist :null-object nil))
            (progn
              (signal 'dank-backend-request-error
                      `(,url-params ,full-url ,(dank-url-response-uncompress)))))))))

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
           (resp (dank-backend-request "GET" url params)))
      (message "%s" (plist-get resp :data))
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
           (resp (dank-backend-request "GET" "/subreddits/mine/subscriber" params)))
      resp)))


(defun dank-backend-post-and-comments-listing (subreddit post-id sorting &rest request-params)
  "Fetch post details and comments of a post in SUBREDDIT with POST-ID.

SORTING must be a symbol of either 'hot, 'new, 'old, 'top, 'random, 'qa, 'confidence, 'live, or 'controversial.

REQUEST-PARAMS is plist of request parameters that Reddit's 'listing' API takes.
e.g. (:depth 10 :limit 25)
Valid keywords are: :depth (integer), :limit (integer)."
  (let* ((depth (plist-get request-params :depth))
         (limit (plist-get request-params :limit))
         (comment (plist-get request-params :comment))
         (showedits (plist-get request-params :showedits))
         (url (concat "/r/" subreddit "/comments/" post-id "/" comment)))
    (let* ((params (if depth (cons `(depth . ,depth) '()) '()))
           (params (if limit (cons `(limit . ,limit) params) params))
           (params (if comment (cons `(comment . ,comment) params) params))
           (params (cons `(sort . ,(symbol-name sorting)) params))
           (resp (dank-backend-request "GET" url params))
           (post (aref (plist-get (plist-get (aref resp 0) :data) :children) 0))
           (comments (plist-get (plist-get (aref resp 1) :data) :children)))
      `(,post . ,comments))))

(defun dank-backend-more-children (link-id children-ids sorting &rest request-params)
  "Fetch more comments from LINK-ID.
CHILDREN-IDS is the list of children ids to fetch.

SORTING must be a symbol of either 'hot, 'new, 'top.

REQUEST-PARAMS is plist of request parameters that Reddit's 'morechildren' API takes."
  (let ((depth (plist-get request-params :depth))
        (limit-children (plist-get request-params :limit-children))
        (url "/api/morechildren"))
    (let* ((params (if depth (cons `(depth . ,depth) '()) '()))
           (params (if limit-children (cons `(limit_children . ,limit-children) params) params))
           (params (cons `(api_type . "json") params))
           (params (cons `(sorting . ,(symbol-name sorting)) params))
           (params (cons `(children . ,children-ids) params))
           (params (cons `(link_id . ,link-id) params))
           (params (cons `(limit_children . "False") params))
           (resp (dank-backend-request "GET" url params))
           (things (plist-get (plist-get (plist-get resp :json) :data) :things)))
      things)))

(defun dank-backend-subreddits (&optional where &rest request-params)
  "Get list of subreddits.
Optional WHERE parameter is a symbol for the type of subscription (e.g. 'mine).
REQUEST-PARAMS is plist of request parameters that Reddit's 'subreddits' API takes."
  (let* ((url (concat "/subreddits/mine/" (or (and where (symbol-name where)) "subscriber")))
         (params `((limit . 100) (sr_detail . nil)))
         (resp (dank-backend-request "GET" url params)))
    (plist-get (plist-get resp :data) :children)))

(provide 'dank-backend)

;;; dank-backend.el ends here
