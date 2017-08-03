(require 'dank-auth)
(require 'dank-backend)
(require 'dank-utils)
(require 'dank-post)
(require 'dank-faces)
(require 's)
(require 'dash)

(defvar dank-listing-buffer nil)

(defvar dank-listing-page-items-count 25)

(defcustom dank-listing-default-subreddit nil "")
(defcustom dank-listing-default-sorting 'hot "")

(defvar-local dank-listing-current-subreddit dank-listing-default-subreddit)
(defvar-local dank-listing-current-sorting dank-listing-default-sorting)
(defvar-local dank-listing-current-count 0)
(defvar-local dank-listing-current-after nil)
(defvar-local dank-listing-current-before nil)
(defvar-local dank-listing-current-page-posts nil)


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
  (let ((ordinals (number-sequence dank-listing-current-count (+ dank-listing-current-count dank-listing-page-items-count)))))
  (mapc #'dank-listing-append-post dank-listing-current-page-posts))

(defun dank-listing-append-post (post)
  "Append POST into `dank-listing-buffer'.
POST-INDEX is the ordinal of the post."
  (when (buffer-live-p dank-listing-buffer)
    (with-current-buffer dank-listing-buffer
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-max))
          (insert (concat (dank-post-render post) "\n")))))))


;;;;;;;;;;;;;;;;;;;
;; display stuff ;;
;;;;;;;;;;;;;;;;;;;

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

;; this highlighting logic is copied from ledger-mode
(defvar-local dank-listing-post-highlight-overlay (list))

(defun dank-listing--make-highlight-overlay ()
  (let ((ovl (make-overlay 1 1)))
    (overlay-put ovl 'font-lock-face 'dank-faces-highlight)
    (overlay-put ovl 'priority '(nil . 99))
    ovl))

(defun dank-listing-highlight-post-under-point ()
  "Highlight post under point."
  (unless dank-listing-post-highlight-overlay
    (setq dank-listing-post-highlight-overlay (dank-listing--make-highlight-overlay)))
  (let ((exts (dank-listing--find-post-extents (point))))
    (let ((b (car exts))
          (e (cadr exts))
          (p (point)))
      (if (and (> (- e b) 1)
               (<= p e) (>= p b))
          (move-overlay dank-listing-post-highlight-overlay b (+ 1 e))
        (move-overlay dank-listing-post-highlight-overlay 1 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; navigation functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dank-listing--navigate-beginning-of-post ()
  "Move point to the beginning of the current post."
  (interactive)
  (beginning-of-line)
  (let ((sreg "^[^ ]"))
    (unless (looking-at sreg)
      (re-search-backward sreg nil t)
      (beginning-of-line)))
  (point))

(defun dank-listing--navigate-end-of-post ()
  "Move point to the end of the current post."
  (interactive)
  ;; Go to beginning of post and go down 2 lines :-P
  (dank-listing--navigate-beginning-of-post)
  (end-of-line)  ;; need to go to end-of-line first to workaround linewraps
  (next-line)
  (end-of-line)
  (next-line)
  (end-of-line)
  (point))

(defun dank-listing--find-post-extents (pos)
  "Return list containing point for beginning and end of post containing POS."
  (interactive "d")
  (save-excursion
    (goto-char pos)
    (list (dank-listing--navigate-beginning-of-post)
          (dank-listing--navigate-end-of-post))))


(defun dank-listing-navigate-prev-post ()
  "Move point to the beginning of previous post."
  (interactive)
  (dank-listing--navigate-beginning-of-post)
  (previous-line)
  (dank-listing--navigate-beginning-of-post)
  (point))

(defun dank-listing-navigate-next-post ()
  "Move point to the beginning of next post."
  (interactive)
  (dank-listing--navigate-end-of-post)
  (next-line)
  (beginning-of-line)
  (point))

(defun dank-listing-goto-next-page ()
  )

(defun dank-listing-goto-next-page ()
  )

(defun dank-listing-goto-subreddit ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interaction functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; misc

(defun dank-listing--find-post-extents (pos)
  "Return list containing point for beginning and end of a post containing POS.")

(provide 'dank-listing)
