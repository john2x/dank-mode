(require 'dank-auth)
(require 'dank-backend)
(require 'dank-utils)
(require 'dank-post)
(require 'dank-faces)
(require 's)
(require 'dash)

(defvar dank-comments-buffer nil)

(defvar-local dank-comments-current-post nil)
(defvar-local dank-comments-current-comments nil)


(define-derived-mode dank-comments-mode special-mode "dank-comments-mode"
  (switch-to-buffer "*dank-comments*"))

(defun dank-comments-init-post-comments (post-id)
  "Initialize dank-comments-buffer with POST-ID."
  (message "init post comments...")
  (unless (and dank-comments-buffer (buffer-live-p dank-comments-buffer))
    (setq dank-comments-buffer (get-buffer "*dank-comments*")))
  (switch-to-buffer "*dank-comments*"))

(defun dank-comments-set-current-post-and-comments (subreddit post-id sorting)
  (let* ((post-comments (dank-backend-post-and-comments-listing subreddit post-id sorting))
         (post (dank-post-parse (car post-comments)))
         (comments (mapcar #'dank-comment-parse (cdr post-comments))))
    (setq-local dank-comments-current-post post)
    (setq-local dank-comments-current-comments comments)))

(defun dank-comments--insert-comment-tree (parent children)
  )

(provide 'dank-comments)
