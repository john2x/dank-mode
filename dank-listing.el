(require 'dank-auth)
(require 'dank-backend)
(require 'dank-utils)
(require 's)

(cl-defstruct dank-post
  id title link text age date author subreddit score num_comments
  domain post_type nsfw spoiler link_flair author_flair
  gilded stickied locked)

(defvar dank-listing-buffer nil)

(defvar dank-listing-post-template
  "${title}\n  submitted ${age} by ${author}${author_flair} to /r/${subreddit}\n  ${score} points | ${num_comments} comments | ${nsfw}${spoiler}${post_type}${domain}${link_flair}")

(defvar dank-listing-page-items-count 25)

(defcustom dank-listing-default-subreddit nil "")
(defcustom dank-listing-default-sorting 'hot "")

(defvar-local dank-listing-current-subreddit dank-listing-default-subreddit)
(defvar-local dank-listing-current-sorting dank-listing-default-sorting)
(defvar-local dank-listing-current-after nil)
(defvar-local dank-listing-current-page-posts nil)

;; create new buffer
;; fetch subreddits
;; push to history
;; render
;; open subreddit
;; push to history
;; render

(define-derived-mode dank-listing-mode special-mode "dank-listing-mode"
  (message "Welcome to your front page!"))

(defun dank-listing-mode-init ()
  "Initialize dank-listing-mode buffer."
  (message "init listing...")
  (unless (and dank-listing-buffer (buffer-live-p dank-listing-buffer))
    (setq dank-listing-buffer (get-buffer "*dank-mode*")))
  (dank-listing-set-page-posts dank-listing-default-subreddit
                               dank-listing-default-sorting
                               dank-listing-page-items-count)
  (dank-listing-render-current-page t))

(defun dank-listing-set-page-posts (subreddit sorting &optional limit after)
  "Get a page of posts from reddit.
Store the results in `dank-listing-current-page-posts'."
  (let* ((posts (dank-backend-post-listing subreddit sorting :limit limit :after after))
         (posts (mapcar (lambda (el) (plist-get el :data)) posts))
         (posts (mapcar #'dank-listing-make-post posts)))
    (setq-local dank-listing-current-page-posts posts)
    (setq-local dank-listing-current-subreddit subreddit)
    (setq-local dank-listing-current-sorting sorting)
    (setq-local dank-listing-current-after after)
    (dank-listing-set-header-line)))

(defun dank-listing-render-current-page (&optional clear)
  "Render contents of `dank-listing-current-page-posts' into `dank-listing-buffer'.
If CLEAR is non-nil, clear the listing buffer before rendering the current page."
  (mapc #'dank-listing-append-post dank-listing-current-page-posts))

(defun dank-listing-render-post (post)
  "Render POST as string using `dank-listing-post-template'."
  (let* ((title (dank-post-title post))
         (age (dank-post-age post))
         (author (dank-post-author post))
         (author_flair (if (dank-post-author_flair post)
                           (concat " [" (dank-post-author_flair post) "]") ""))
         (subreddit (dank-post-subreddit post))
         (score (format "%s" (dank-post-score post)))
         (num_comments (format "%s" (dank-post-num_comments post)))
         (nsfw (if (dank-post-nsfw post) "NSFW " ""))
         (spoiler (if (dank-post-spoiler post) "SPOILERS " ""))
         (post_type (if (string= (dank-post-post_type post) "self")
                        "self-text" (dank-post-post_type post)))
         (domain (if (string= post_type "self-text") "" (concat " from " (dank-post-domain post))))
         (link_flair (if (dank-post-link_flair post)
                         (concat " [" (dank-post-link_flair post) "]") ""))
         (format-context `(title ,title age ,age author ,author author_flair ,author_flair
                                 subreddit ,subreddit score ,score num_comments ,num_comments
                                 nsfw ,nsfw spoiler ,spoiler domain ,domain post_type ,post_type
                                 link_flair ,link_flair)))
    (dank-utils-format-plist dank-listing-post-template format-context)))

(defun dank-listing-make-post (post)
  "Parse POST into a `dank-post'."
  (make-dank-post :id (plist-get post :id)
                  :title (plist-get post :title)
                  :link (plist-get post :url)
                  :text (plist-get post :selftext)
                  :age "TODO hours ago"
                  :date (plist-get post :created_utc)
                  :score (plist-get post :score)
                  :author (plist-get post :author)
                  :subreddit (plist-get post :subreddit)
                  :num_comments (plist-get post :num_comments)
                  :domain (plist-get post :domain)
                  :post_type (or (plist-get post :post_hint) (when (eq (plist-get post :is_self) t) "self"))
                  :nsfw (not (eq (plist-get post :over_18) :json-false))
                  :spoiler (not (eq (plist-get post :spoiler) :json-false))
                  :link_flair (plist-get post :link_flair_text)
                  :author_flair (plist-get post :author_flair_text)
                  :gilded (plist-get post :gilded)
                  :stickied (not (eq (plist-get post :stickied) :json-false))
                  :locked (not (eq (plist-get post :locked) :json-false))))

(defun dank-listing-append-post (post)
  "Append POST into `dank-listing-buffer'."
  (when (buffer-live-p dank-listing-buffer)
    (with-current-buffer dank-listing-buffer
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-max))
          (insert (concat (dank-listing-render-post post) "\n")))))))


;;;;;;;;;;;;;;;;;;;;;;;
;; header-line stuff ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defvar dank-listing-header-line-format-template
  "${subreddit} - ${sorting}")

(defun dank-listing-set-header-line ()
  (when (buffer-live-p dank-listing-buffer)
    (with-current-buffer dank-listing-buffer
      (setq header-line-format (dank-utils-format-plist
                                dank-listing-header-line-format-template
                                `(subreddit ,(or dank-listing-current-subreddit "Frontpage")
                                            sorting ,(symbol-name dank-listing-current-sorting)
                                            position ,(if dank-listing-current-after (format " - " dank-listing-current-after)
                                                        "")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; navigation functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interaction functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'dank-listing)
