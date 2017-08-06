(require 'dank-auth)
(require 'dank-backend)
(require 'dank-utils)
(require 'dank-post)
(require 'dank-faces)
(require 's)
(require 'dash)

(defvar dank-listing-buffer nil)

(defvar dank-listing-page-items-limit 25)

(defcustom dank-listing-default-subreddit nil "")
(defcustom dank-listing-default-sorting 'hot "")

(defvar-local dank-listing-current-subreddit dank-listing-default-subreddit)
(defvar-local dank-listing-current-sorting dank-listing-default-sorting)
(defvar-local dank-listing-current-start-count 0)
(defvar-local dank-listing-current-end-count 0)
(defvar-local dank-listing-current-after nil)
(defvar-local dank-listing-current-before nil)
(defvar-local dank-listing-current-page-posts nil)


(define-derived-mode dank-listing-mode special-mode "dank-listing-mode"
  (message "Welcome to your front page!"))

(defun dank-listing-init-front-page ()
  "Initialize dank-listing-mode buffer."
  (message "init front page...")
  (unless (and dank-listing-buffer (buffer-live-p dank-listing-buffer))
    (setq dank-listing-buffer (get-buffer "*dank-mode*")))
  (dank-listing-reset-state dank-listing-default-subreddit dank-listing-default-sorting
                            dank-listing-page-items-limit)
  (dank-listing-render-current-page))


(defun dank-listing-reset-state (subreddit sorting limit)
  (setq-local dank-listing-current-subreddit subreddit)
  (setq-local dank-listing-current-sorting sorting)
  (setq-local dank-listing-current-start-count 0)
  (setq-local dank-listing-current-end-count 0)
  (setq-local dank-listing-current-after nil)
  (setq-local dank-listing-current-before nil)
  (setq-local dank-listing-current-page-posts nil)
  (setq-local dank-listing-current-all-posts nil)
  (dank-listing-set-page-posts subreddit sorting limit))


(defun dank-listing-set-page-posts (subreddit sorting limit &optional count after before)
  "Get a page of posts from reddit.
Store the results in `dank-listing-current-page-posts'."
  (let* ((posts (dank-backend-post-listing subreddit sorting
                                           :limit limit :after after :before before :count count))
         (posts (mapcar (lambda (el) (plist-get el :data)) posts))
         (posts (mapcar #'dank-post-parse posts)))
    (setq-local dank-listing-current-page-posts posts)
    (setq-local dank-listing-current-all-posts (append dank-listing-current-page-posts posts))
    (setq-local dank-listing-current-subreddit subreddit)
    (setq-local dank-listing-current-sorting sorting)
    ;; update navigation references
    (if (not before)
        (progn
          (setq-local dank-listing-current-start-count (+ 1 dank-listing-current-end-count))
          (setq-local dank-listing-current-end-count (+ dank-listing-current-end-count
                                                        dank-listing-page-items-limit)))
      (progn
        (setq-local dank-listing-current-end-count (- dank-listing-current-start-count 1))
        (setq-local dank-listing-current-start-count (- dank-listing-current-end-count
                                                        dank-listing-page-items-limit))))
    (if (<= dank-listing-current-start-count 0)
        (setq dank-listing-current-start-count 1))
    (setq-local dank-listing-current-after (dank-post-name (car (last posts))))
    (setq-local dank-listing-current-before (dank-post-name (car posts)))
    (dank-listing-set-header-line)))

(defun dank-listing-render-current-page (&optional clear-buffer)
  "Render contents of `dank-listing-current-page-posts' into `dank-listing-buffer'.
Clears `dank-listing-buffer' before rendering."
  (when (and (buffer-live-p dank-listing-buffer) clear-buffer)
    (with-current-buffer dank-listing-buffer
      (let ((inhibit-read-only t))
        (erase-buffer))))
  (let* ((ordinals (number-sequence dank-listing-current-start-count dank-listing-current-end-count))
         ;; merge ordinals and posts lists into one list of pairs '(ord post)
         (ords-posts (mapcar* #'list ordinals dank-listing-current-page-posts)))
    (mapc (lambda (ord-post) (dank-listing-append-post (car ord-post) (cadr ord-post)))
          ords-posts)))

(defun dank-listing-append-post (ord post)
  "Append POST into `dank-listing-buffer'.
POST-INDEX is the ordinal of the post."
  (when (buffer-live-p dank-listing-buffer)
    (with-current-buffer dank-listing-buffer
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-max))
          (insert (concat (int-to-string ord) ". " (dank-post-render post) "\n")))))))


;;;;;;;;;;;;;;;;;;;
;; display stuff ;;
;;;;;;;;;;;;;;;;;;;

(defvar dank-listing-header-line-format-template
  "${subreddit} - ${sorting} (${count})")

(defun dank-listing-set-header-line ()
  (when (buffer-live-p dank-listing-buffer)
    (with-current-buffer dank-listing-buffer
      (setq header-line-format (dank-utils-format-plist
                                dank-listing-header-line-format-template
                                `(subreddit ,(or dank-listing-current-subreddit "Frontpage")
                                            sorting ,(symbol-name dank-listing-current-sorting)
                                            count ,dank-listing-current-end-count))))))

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
  (interactive)
  (dank-listing-set-page-posts dank-listing-current-subreddit
                               dank-listing-current-sorting
                               dank-listing-page-items-limit
                               dank-listing-current-end-count
                               dank-listing-current-after
                               nil)
  (goto-char (point-max))
  (dank-listing-render-current-page))

(defun dank-listing-refresh ()
  (interactive)
  (dank-listing-reset-state dank-listing-current-subreddit dank-listing-current-sorting
                            dank-listing-page-items-limit)
  (dank-listing-render-current-page t))

(defun dank-listing-goto-subreddit ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interaction functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'dank-listing)
