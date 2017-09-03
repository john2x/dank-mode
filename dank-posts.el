(require 'dank-auth)
(require 'dank-backend)
(require 'dank-utils)
(require 'dank-post)
(require 'dank-faces)
(require 's)
(require 'dash)

(defvar dank-posts-buffer nil)

(defvar dank-posts-page-items-limit 25)

(defcustom dank-posts-default-subreddit nil "")
(defcustom dank-posts-default-sorting 'hot "")

(defvar-local dank-posts-current-subreddit dank-posts-default-subreddit)
(defvar-local dank-posts-current-sorting dank-posts-default-sorting)
(defvar-local dank-posts-current-start-count 0)
(defvar-local dank-posts-current-end-count 0)
(defvar-local dank-posts-current-after nil)
(defvar-local dank-posts-current-before nil)
(defvar-local dank-posts-current-page-posts nil)


(define-derived-mode dank-posts-mode special-mode "dank-posts-mode"
  (switch-to-buffer "*dank-posts*")
  (message "Welcome to your front page!"))

(defun dank-posts-init-front-page ()
  "Initialize dank-posts-mode buffer."
  (message "init front page...")
  (unless (and dank-posts-buffer (buffer-live-p dank-posts-buffer))
    (setq dank-posts-buffer (get-buffer "*dank-posts*")))
  (dank-posts-reset-state dank-posts-default-subreddit dank-posts-default-sorting
                            dank-posts-page-items-limit)
  (dank-posts-render-current-page))


(defun dank-posts-reset-state (subreddit sorting limit)
  (setq-local dank-posts-current-subreddit subreddit)
  (setq-local dank-posts-current-sorting sorting)
  (setq-local dank-posts-current-start-count 0)
  (setq-local dank-posts-current-end-count 0)
  (setq-local dank-posts-current-after nil)
  (setq-local dank-posts-current-before nil)
  (setq-local dank-posts-current-page-posts nil)
  (setq-local dank-posts-current-all-posts nil)
  (dank-posts-set-page-posts subreddit sorting limit))


(defun dank-posts-set-page-posts (subreddit sorting limit &optional count after before)
  "Get a page of posts from reddit.
Store the results in `dank-posts-current-page-posts'."
  (let* ((posts (dank-backend-post-listing subreddit sorting
                                           :limit limit :after after :before before :count count))
         (posts (mapcar #'dank-post-parse posts)))
    (setq-local dank-posts-current-page-posts posts)
    (setq-local dank-posts-current-all-posts (append dank-posts-current-page-posts posts))
    (setq-local dank-posts-current-subreddit subreddit)
    (setq-local dank-posts-current-sorting sorting)
    ;; update navigation references
    (if (not before)
        (progn
          (setq-local dank-posts-current-start-count (+ 1 dank-posts-current-end-count))
          (setq-local dank-posts-current-end-count (+ dank-posts-current-end-count
                                                        dank-posts-page-items-limit)))
      (progn
        (setq-local dank-posts-current-end-count (- dank-posts-current-start-count 1))
        (setq-local dank-posts-current-start-count (- dank-posts-current-end-count
                                                        dank-posts-page-items-limit))))
    (if (<= dank-posts-current-start-count 0)
        (setq dank-posts-current-start-count 1))
    (setq-local dank-posts-current-after (dank-post-name (car (last posts))))
    (setq-local dank-posts-current-before (dank-post-name (car posts)))
    (dank-posts-set-header-line)))

(defun dank-posts-render-current-page (&optional clear-buffer)
  "Render contents of `dank-posts-current-page-posts' into `dank-posts-buffer'.
Clears `dank-posts-buffer' before rendering."
  (when (and (buffer-live-p dank-posts-buffer) clear-buffer)
    (with-current-buffer dank-posts-buffer
      (let ((inhibit-read-only t))
        (erase-buffer))))
  (let* ((ordinals (number-sequence dank-posts-current-start-count dank-posts-current-end-count))
         ;; merge ordinals and posts lists into one list of pairs '(ord post)
         (ords-posts (mapcar* #'list ordinals dank-posts-current-page-posts)))
    (mapc (lambda (ord-post) (dank-posts-append-post (car ord-post) (cadr ord-post)))
          ords-posts)))

(defun dank-posts-append-post (ord post)
  "Append POST into `dank-posts-buffer'.
POST-INDEX is the ordinal of the post."
  (when (buffer-live-p dank-posts-buffer)
    (with-current-buffer dank-posts-buffer
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-max))
          (insert (concat (cond ((< ord 10) "  ")
                                ((< ord 100) " ")
                                (t ""))
                          (int-to-string ord) " "
                          (dank-post-render post) "\n")))))))


;;;;;;;;;;;;;;;;;;;
;; display stuff ;;
;;;;;;;;;;;;;;;;;;;

(defvar dank-posts-header-line-format-template
  "${subreddit} - ${sorting} (${count})")

(defun dank-posts-set-header-line ()
  (when (buffer-live-p dank-posts-buffer)
    (with-current-buffer dank-posts-buffer
      (setq header-line-format (dank-utils-format-plist
                                dank-posts-header-line-format-template
                                `(subreddit ,(or dank-posts-current-subreddit "Frontpage")
                                            sorting ,(symbol-name dank-posts-current-sorting)
                                            count ,dank-posts-current-end-count))))))

;; this highlighting logic is copied from ledger-mode
(defvar-local dank-posts-post-highlight-overlay (list))

(defun dank-posts--make-highlight-overlay ()
  (let ((ovl (make-overlay 1 1)))
    (overlay-put ovl 'font-lock-face 'dank-faces-highlight)
    (overlay-put ovl 'priority '(nil . 99))
    ovl))

(defun dank-posts-highlight-post-under-point ()
  "Highlight post under point."
  (unless dank-posts-post-highlight-overlay
    (setq dank-posts-post-highlight-overlay (dank-posts--make-highlight-overlay)))
  (let ((exts (dank-posts--find-post-extents (point))))
    (let ((b (car exts))
          (e (cadr exts))
          (p (point)))
      (if (and (> (- e b) 1)
               (<= p e) (>= p b))
          (move-overlay dank-posts-post-highlight-overlay b (+ 1 e))
        (move-overlay dank-posts-post-highlight-overlay 1 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; navigation functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dank-posts--navigate-beginning-of-post ()
  "Move point to the beginning of the current post."
  (interactive)
  (beginning-of-line)
  (let ((sreg "^[^ ]"))
    (unless (looking-at sreg)
      (re-search-backward sreg nil t)
      (beginning-of-line)))
  (point))

(defun dank-posts--navigate-end-of-post ()
  "Move point to the end of the current post."
  (interactive)
  ;; Go to beginning of post and go down 2 lines :-P
  (dank-posts--navigate-beginning-of-post)
  (end-of-line)  ;; need to go to end-of-line first to workaround linewraps
  (next-line)
  (end-of-line)
  (next-line)
  (end-of-line)
  (point))

(defun dank-posts--find-post-extents (pos)
  "Return list containing point for beginning and end of post containing POS."
  (interactive "d")
  (save-excursion
    (goto-char pos)
    (list (dank-posts--navigate-beginning-of-post)
          (dank-posts--navigate-end-of-post))))


(defun dank-posts-navigate-prev-post ()
  "Move point to the beginning of previous post."
  (interactive)
  (dank-posts--navigate-beginning-of-post)
  (previous-line)
  (dank-posts--navigate-beginning-of-post)
  (point))

(defun dank-posts-navigate-next-post ()
  "Move point to the beginning of next post."
  (interactive)
  (dank-posts--navigate-end-of-post)
  (next-line)
  (beginning-of-line)
  (point))

(defun dank-posts-load-next-page ()
  (interactive)
  (dank-posts-set-page-posts dank-posts-current-subreddit
                               dank-posts-current-sorting
                               dank-posts-page-items-limit
                               dank-posts-current-end-count
                               dank-posts-current-after
                               nil)
  (goto-char (point-max))
  (dank-posts-render-current-page))

(defun dank-posts-refresh ()
  (interactive)
  (dank-posts-reset-state dank-posts-current-subreddit dank-posts-current-sorting
                            dank-posts-page-items-limit)
  (dank-posts-render-current-page t))

(defun dank-posts-goto-subreddit ())

(defun dank-posts-goto-post-comments (post-id))

(defun dank-posts-goto-post-comments-at-point ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interaction functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'dank-posts)
