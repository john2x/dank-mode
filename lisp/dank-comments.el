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
(defcustom dank-comments-highlight-under-point-enabled 't "")

(defvar-local dank-comments-buffer nil)
(defvar-local dank-comments-current-post-id nil)
(defvar-local dank-comments-current-subreddit nil)
(defvar-local dank-comments-current-sorting nil)
(defvar-local dank-comments-current-post nil)
(defvar-local dank-comments-current-comments nil)

(defvar dank-comments-mode-map
  (let ((map (make-sparse-keymap)))
    ;(define-key map "n" 'dank-posts-navigate-next-post)
    ;(define-key map "p" 'dank-posts-navigate-prev-post)
    ;(define-key map (kbd "C-c C-v") 'dank-posts-fetch-next-page)
    (define-key map (kbd "C-c C-r") 'dank-comments-refresh)
    ;(define-key map (kbd "C-c C-c") 'dank-posts-goto-post-comments-at-point)
    ;(define-key map (kbd "C-c C-/") 'dank-posts-goto-subreddit-at-point)
    (define-key map (kbd "C-c q") 'kill-current-buffer)
    map))

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
        (dank-comments-render-current-comments dank-comments-current-comments dank-comments-current-post)
        (goto-char 0)))))

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
          dank-comments-current-comments comments)
    (dank-comments-set-header-line)))


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

(defun dank-comments-set-header-line ()
  "Set the header line of a dank-comments buffer."
  (when (buffer-live-p dank-comments-buffer)
    (with-current-buffer dank-comments-buffer
      (setq header-line-format (dank-utils-format-plist
                                dank-comments-header-line-format-template
                                `(subreddit ,dank-comments-current-subreddit
                                            sorting ,(symbol-name dank-posts-current-sorting)))))))

;; this highlighting logic is copied from ledger-mode
;; an overlay needs to be set once in the buffer and moved around
(defvar-local dank-comments-highlight-overlay (list))
(defun dank-comments-highlight-under-point ()
  "Highlight comment under point."
  (when dank-comments-highlight-under-point-enabled
    (unless dank-comments-highlight-overlay
      (setq dank-comments-highlight-overlay (dank-utils-make-highlight-overlay)))
    (let ((exts (dank-comments--find-comment-extents (point))))
      (let ((b (car exts))
            (e (cadr exts))
            (p (point)))
        (message "%s %s %s" b e p)
        (if (and (> (- e b) 1)
                 (<= p e))
            (move-overlay dank-comments-highlight-overlay b (+ 1 e))
          (move-overlay dank-comments-highlight-overlay 1 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; navigation functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dank-comments--navigate-beginning-of-comment ()
  "Move point to the beginning of the current comment."
  (interactive)
  (beginning-of-line)
  (if (looking-at " *[-+] /u/")
      ; When point is behind the start of a comment, just move to the start
      (beginning-of-line-text)
    (let ((sreg "[-+] /u/"))
      (unless (looking-at sreg)
        (re-search-backward sreg nil t))))
  (beginning-of-line-text)
  (backward-char 2)
  (point))

(defun dank-comments--navigate-end-of-comment ()
  "Move point to the end of the current comment."
  (interactive)
  (let ((sreg " *[-+] /u/"))
    ; When point is already behind the start of a comment, move down first
    (when (looking-at sreg)
      (next-line)))
  (let ((sreg "[-+] /u/"))
    ; Look for the start of the next comment then move up
    (unless (looking-at sreg)
      (re-search-forward sreg nil t)))
  (previous-line)
  (end-of-line)
  (point))

(defun dank-comments--find-comment-extents (pos)
  "Return list containing point for beginning and end of comment containing POS."
  ;; find the header then do beginning-of-line-text
  (interactive "d")
  (save-excursion
    (goto-char pos)
    (list (dank-comments--navigate-beginning-of-comment)
          (dank-comments--navigate-end-of-comment))))

(defun dank-comments-refresh ()
  (interactive)
  (dank-comments-reset-state)
  (dank-comments-render-current-post dank-comments-current-post t)
  (dank-comments-render-current-comments dank-comments-current-comments dank-comments-current-post)
  (goto-char 0))

(provide 'dank-comments)
