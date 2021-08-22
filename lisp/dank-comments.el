(require 'dank-auth)
(require 'dank-backend)
(require 'dank-utils)
(require 'dank-post)
(require 'dank-faces)
(require 'dank-comment)
(require 's)
(require 'dash)

(defcustom dank-comments-default-depth 5
  "Default depth of the comment tree to initially fetch.")
(defcustom dank-comments-body-fill-width 120
  "Fill width for rendering the comment body.")

(defvar-local dank-comments-buffer nil)
(defvar-local dank-comments-current-post-id nil)
(defvar-local dank-comments-current-subreddit nil)
(defvar-local dank-comments-current-sorting nil)
(defvar-local dank-comments-current-post nil)
(defvar-local dank-comments-current-comments nil)

(define-derived-mode dank-comments-mode special-mode "dank-comments-mode"
  "Major mode for reading reddit post comments."
  (setq show-trailing-whitespace nil))

(defun dank-comments-init (subreddit post-id permalink &optional sorting)
  "Initialize dank-comments-buffer with POST-ID."
  (let ((buf (concat "*dank-comments* " permalink)))
    (if (get-buffer buf)
        (progn
          (message "Switched to existing dank-comments-mode buffer %s" permalink)
          (switch-to-buffer buf))
      (progn
        (message "Initializing post comments buffer %s..." buf)
        (switch-to-buffer buf)
        (dank-comments-mode)
        (setq dank-comments-buffer (current-buffer)
              dank-comments-current-subreddit subreddit
              dank-comments-current-post-id post-id
              dank-comments-current-sorting sorting)
        (condition-case err
            (dank-comments-reset-state)
          (dank-backend-error (progn (dank-comments-render-error err)
                                     (signal (car err) (cdr err)))))
        (dank-comments-render-current-post dank-comments-current-post t)
        (dank-comments-render-current-comments dank-comments-current-comments dank-comments-current-post)))))

(defun dank-comments-reset-state ()
  "Reset state of the current dank-posts buffer."
  (setq dank-comments-current-comments nil
        dank-comments-current-post nil)
  (dank-comments-set-current-post-and-comments dank-comments-current-subreddit dank-comments-current-post-id dank-comments-current-sorting))

(defun dank-comments-set-current-post-and-comments (subreddit post-id &optional sorting)
  (let* ((post-comments (dank-backend-post-and-comments-listing subreddit post-id sorting '(:depth ,dank-comments-default-depth)))
         (post (dank-post-parse (car post-comments)))
         (comments (mapcar #'dank-comment-parse (cdr post-comments))))
    (setq dank-comments-current-post post
          dank-comments-current-comments comments)))


(defun dank-comments--insert-comment-tree (parent children)
  )

(dank-defrender dank-comments-render-current-post dank-comments-buffer (post &optional clear-buffer)
  "Render the post contents in the current buffer."
  (let* ((inhibit-read-only t)
         (formatted-post (concat (dank-post-format dank-comments-current-post 1) "\n"))
         (formatted-content (dank-comment-format-post-content dank-comments-current-post dank-comments-body-fill-width)))
    (when clear-buffer
      (erase-buffer))
    (save-excursion
      (goto-char (point-max))
      (insert formatted-post)
      (insert formatted-content))))

(dank-defrender dank-comments-render-current-comments dank-comments-buffer (comments post &optional clear-buffer)
  (let ((inhibit-read-only t))
    (when clear-buffer
      (erase-buffer))
    (goto-char (point-max))
    (insert ;; insert comments into a temp buffer and insert that into the real buffer
     (with-temp-buffer
       (dank-comments--insert-comments-in-current-buffer comments (dank-post-author post))
       (buffer-string)))))

(defun dank-comments--insert-comments-in-current-buffer (comments post-author)
  "Insert a COMMENT-TREE into the current buffer (preferably a temp buffer)."
  (when comments
    (let* ((comment (car comments)))
      (dank-comments--insert-comment-in-current-buffer comment post-author)
      (when (eq (type-of comment) 'dank-comment)
        (dank-comments--insert-comments-in-current-buffer (dank-comment-replies comment) post-author))
      (dank-comments--insert-comments-in-current-buffer (cdr comments) post-author))))

(defun dank-comments--insert-comment-in-current-buffer (comment post-author &optional point)
  "Insert COMMENT into the current temporary buffer at optional POINT."
  (if (eq (type-of comment) 'dank-comment)
      (let* ((formatted-comment-metadata (concat (dank-comment-format-metadata comment post-author) "\n"))
             (formatted-comment-body (concat (dank-comment-format-body comment dank-comments-body-fill-width) "\n")))
        (goto-char (or point (point-max)))
        (insert formatted-comment-metadata)
        (insert formatted-comment-body))
    (let ((formatted-load-more-placeholder (concat (dank-comment-format-load-more-placeholder comment) "\n")))
      (goto-char (or point (point-max)))
      (insert formatted-load-more-placeholder))))

(dank-defrender dank-comments-render-error dank-comments-buffer (err)
  "Render the ERR message in the current buffer and show recommended actions."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (format "%s\n" err))
    (insert "TODO: show recommended actions (either [q]uit or retry)")))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; navigation functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dank-comments-refresh ()
  (interactive)
  (dank-comments-reset-state)
  (dank-comments-render-current-post dank-comments-current-post t)
  (dank-comments-render-current-comments dank-comments-current-comments dank-comments-current-post))

(provide 'dank-comments)
