(require 'dank-auth)
(require 'dank-backend)
(require 'dank-utils)
(require 'dank-post)
(require 'dank-faces)
(require 'dank-comment)
(require 's)
(require 'dash)

(defvar-local dank-comments-buffer nil)
(defvar-local dank-comments-post-id nil)
(defvar-local dank-comments-subreddit nil)
(defvar-local dank-comments-sorting nil)

(defvar-local dank-comments-current-post nil)
(defvar-local dank-comments-current-comments nil)


(defvar dank-comments-global-var nil)
(define-derived-mode dank-comments-mode special-mode "dank-comments-mode")

(defun dank-comments-init-post-comments (subreddit post-id permalink &optional sorting)
  "Initialize dank-comments-buffer with POST-ID."
  (let ((buf (concat "*dank-comments* " permalink)))
    (message "init post comments %s..." buf)
    (switch-to-buffer buf)
    (dank-comments-mode)
    (setq dank-comments-buffer (current-buffer))
    (setq dank-comments-subreddit subreddit)
    (setq dank-comments-post-id post-id)
    (setq dank-comments-sorting sorting)
    (condition-case err
        (dank-comments-set-current-post-and-comments subreddit post-id sorting)
      (dank-backend-error (progn (dank-comments-render-error err)
                                 (signal (car err) (cdr err)))))
    (dank-comments-render-current-comments t)))

(defun dank-comments-set-current-post-and-comments (subreddit post-id &optional sorting)
  (let* ((post-comments (dank-backend-post-and-comments-listing subreddit post-id sorting))
         (post (dank-post-parse (car post-comments)))
         (comments (mapcar #'dank-comment-parse (cdr post-comments))))
    (message "%s" post)
    (setq dank-comments-current-post post)
    (setq dank-comments-current-comments comments)))


(defun dank-comments--insert-comment-tree (parent children)
  )

(dank-defrender dank-comments-render-current-post dank-comments-buffer ()
  (let* ((inhibit-read-only t)
         (rendered-post (concat (dank-post-render dank-comments-current-post) "\n")))
    (dank-post-propertize rendered-post dank-comments-current-post 1)
    (save-excursion
      (goto-char (point-max))
      (insert rendered-post)
      (dank-comments--insert-current-post-self-text))))

(defun dank-comments--insert-current-post-self-text ()
  ""
  (insert (propertize "\n" 'font-lock-face 'dank-faces-separator))
  (if (string= (dank-post-post_type dank-comments-current-post) "self-text")
      (insert (dank-post-text dank-comments-current-post))
    (insert (dank-post-link dank-comments-current-post)))
  (insert "\n")
  (insert (propertize "\n" 'font-lock-face 'dank-faces-separator)))

(dank-defrender dank-comments-render-current-comments dank-comments-buffer (&optional clear-buffer)
  (when clear-buffer
    (let ((inhibit-read-only t))
      (erase-buffer)))
  (dank-comments-render-current-post)
  ;(mapc dank-comments-insert-post dank-comments-current-comments)
  )

(dank-defrender dank-comments-render-error dank-comments-buffer (err)
  "Render the ERR message in the current buffer and show recommended actions."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (format "%s\n" err))
    (insert "TODO: show recommended actions (either [q]uit or retry)")))

(provide 'dank-comments)
