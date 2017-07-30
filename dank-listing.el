(require 'dank-auth)
(require 'dank-backend)
(require 'dank-utils)
(require 'dank-post)
(require 's)

(defvar dank-listing-buffer nil)

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
  (dank-listing-render-current-page))

(defun dank-listing-set-page-posts (subreddit sorting &optional limit after)
  "Get a page of posts from reddit.
Store the results in `dank-listing-current-page-posts'."
  (let* ((posts (dank-backend-post-listing subreddit sorting :limit limit :after after))
         (posts (mapcar (lambda (el) (plist-get el :data)) posts))
         (posts (mapcar #'dank-post-parse posts)))
    (setq-local dank-listing-current-page-posts posts)
    (setq-local dank-listing-current-subreddit subreddit)
    (setq-local dank-listing-current-sorting sorting)
    (setq-local dank-listing-current-after after)
    (dank-listing-set-header-line)))

(defun dank-listing-render-current-page ()
  "Render contents of `dank-listing-current-page-posts' into `dank-listing-buffer'.
Clears `dank-listing-buffer' before rendering."
  (when (buffer-live-p dank-listing-buffer)
    (with-current-buffer dank-listing-buffer
      (erase-buffer)))
  (mapc #'dank-listing-append-post dank-listing-current-page-posts))

(defun dank-listing-append-post (post)
  "Append POST into `dank-listing-buffer'."
  (when (buffer-live-p dank-listing-buffer)
    (with-current-buffer dank-listing-buffer
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-max))
          (insert (concat (dank-post-render post) "\n")))))))


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

(defun dank-listing-goto-next-page ()
  )

(defun dank-listing-goto-next-page ()
  )

(defun dank-listing-goto-subreddit ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interaction functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'dank-listing)
