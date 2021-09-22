(require 'dank-auth)
(require 'dank-backend)
(require 'dank-utils)
(require 'dank-post)
(require 'dank-faces)
(require 'dank-comments)
(require 's)
(require 'dash)
(require 'browse-url)


(defvar dank-posts-page-items-limit 25)
(defvar dank-posts-subscribed-subreddits nil)

(when (not dank-posts-subscribed-subreddits)
  (setq dank-posts-subscribed-subreddits (dank-posts--get-subscribed-subreddits-names)))

(defcustom dank-posts-known-subreddits dank-posts-subscribed-subreddits "")
(defcustom dank-posts-default-subreddit nil "")
(defcustom dank-posts-default-sorting 'hot "")
(defcustom dank-posts-highlight-under-point-enabled 't "")

(defvar-local dank-posts-buffer nil)
(defvar-local dank-posts-current-subreddit dank-posts-default-subreddit)
(defvar-local dank-posts-current-sorting dank-posts-default-sorting)
(defvar-local dank-posts-current-start-count 0)
(defvar-local dank-posts-current-end-count 0)
(defvar-local dank-posts-current-after nil)
(defvar-local dank-posts-current-before nil)
(defvar-local dank-posts-current-page-posts nil)

(defvar dank-posts-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'dank-posts-navigate-next-post)
    (define-key map "p" 'dank-posts-navigate-prev-post)
    (define-key map (kbd "C-x C-v") 'dank-posts-fetch-next-page)
    (define-key map (kbd "C-x C-r") 'dank-posts-refresh)
    (define-key map (kbd "C-x C-o") 'dank-posts-goto-post-comments-at-point)
    (define-key map (kbd "C-x C-/") 'dank-posts-goto-subreddit-at-point)
    (define-key map (kbd "C-x C-f") 'dank-posts-goto-subreddit)
    (define-key map (kbd "C-x b") 'dank-posts-browse-post-link-at-point)
    (define-key map (kbd "C-x o") 'dank-posts-browse-post-comments-at-point)
    (define-key map (kbd "C-x q") 'kill-current-buffer)
    map))

(define-derived-mode dank-posts-mode special-mode "dank-posts-mode"
  "Major mode for browsing reddit posts."
  (setq show-trailing-whitespace nil))

(defun dank-posts-init (&optional subreddit)
  "Initialize dank-posts-mode buffer to SUBREDDIT.
If SUBREDDIT is nil, a frontpage buffer is initialized.
If a buffer already exists, switch to that buffer."
  (let ((buf (concat "*dank-posts* " (if subreddit (concat "/r/" subreddit) "frontpage"))))
    (if (get-buffer buf)
        (progn
          (message "Switched to existing dank-posts-mode buffer for %s..." (or subreddit "frontpage"))
          (switch-to-buffer buf))
      (progn
        (message "Initializing dank-posts-mode buffer for %s..." (or subreddit "frontpage"))
        (switch-to-buffer buf)
        (dank-posts-mode)
        (setq dank-posts-buffer (current-buffer))
        (condition-case err
            (dank-posts-reset-state subreddit dank-posts-default-sorting dank-posts-page-items-limit)
          (dank-backend-error (progn (dank-posts-render-error err)
                                     (signal (car err) (cdr err)))))
        (dank-posts-render-current-page dank-posts-current-page-posts)
        (dank-posts-highlight-under-point)))))


(defun dank-posts-reset-state (subreddit sorting limit)
  "Reset state of the current dank-posts buffer."
  (setq dank-posts-current-subreddit subreddit
        dank-posts-current-sorting sorting
        dank-posts-current-start-count 0
        dank-posts-current-end-count 0
        dank-posts-current-after nil
        dank-posts-current-before nil
        dank-posts-current-page-posts nil
        dank-posts-current-all-posts nil)
  (dank-posts-set-page-posts subreddit sorting limit))


(defun dank-posts-set-page-posts (subreddit sorting limit &optional count after before)
  "Get a page of posts from reddit.
Store the results in `dank-posts-current-page-posts'."
  (let* ((posts (dank-backend-post-listing subreddit sorting
                                           :limit limit :after after :before before :count count))
         (posts (mapcar #'dank-post-parse posts)))
    (setq dank-posts-current-page-posts posts
          dank-posts-current-all-posts (append dank-posts-current-page-posts posts)
          dank-posts-current-subreddit subreddit
          dank-posts-current-sorting sorting)
    ;; update navigation references
    (if (not before)
        (progn
          (setq dank-posts-current-start-count (+ 1 dank-posts-current-end-count)
                dank-posts-current-end-count (+ dank-posts-current-end-count
                                                            dank-posts-page-items-limit)))
      (progn
        (setq dank-posts-current-end-count (- dank-posts-current-start-count 1)
              dank-posts-current-start-count (- dank-posts-current-end-count
                                                            dank-posts-page-items-limit))))
    (if (<= dank-posts-current-start-count 0)
        (setq dank-posts-current-start-count 1))
    (setq dank-posts-current-after (dank-post-name (car (last posts)))
          dank-posts-current-before (dank-post-name (car posts)))
    (dank-posts-set-header-line)))


(dank-defrender dank-posts-render-current-page dank-posts-buffer (posts &optional clear-buffer)
  "Render contents of POSTS into `dank-posts-buffer'.
Clears `dank-posts-buffer' before rendering."
  (when clear-buffer
    (let ((inhibit-read-only t))
      (erase-buffer)))
  (let* ((ordinals (number-sequence dank-posts-current-start-count dank-posts-current-end-count))
         ;; merge ordinals and posts lists into one list of pairs '(ord post)
         (ords-posts (mapcar* #'list ordinals dank-posts-current-page-posts)))
    (mapc (lambda (ord-post) (dank-posts-append-post-to-buffer dank-posts-buffer (car ord-post) (cadr ord-post)))
          ords-posts)))


(defun dank-posts-append-post-to-buffer (buf post-index post)
  "Append POST into BUF.
POST-INDEX is the number (\"position\") of the post."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (let* ((inhibit-read-only t)
             (formatted-post (concat (dank-post-format post post-index) "\n")))
        (save-excursion
          (goto-char (point-max))
          (insert formatted-post))))))

(dank-defrender dank-posts-render-error dank-posts-buffer (err)
  "Render contents of ERR into `dank-posts-buffer'."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (format "%s\n" err))
    (insert "TODO: show recommended actions (either [q]uit or retry)")))

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
;; an overlay needs to be set once in the buffer and moved around
(defvar-local dank-posts-highlight-overlay (list))
(defun dank-posts-highlight-under-point ()
  "Highlight post under point."
  (when dank-posts-highlight-under-point-enabled
    (unless dank-posts-highlight-overlay
      (setq dank-posts-highlight-overlay (dank-utils-make-highlight-overlay)))
    (let ((exts (dank-posts--find-post-extents (point))))
      (let ((b (car exts))
            (e (cadr exts))
            (p (point)))
        (if (and (> (- e b) 1)
                 (<= p e) (>= p b))
            (move-overlay dank-posts-highlight-overlay b (+ 1 e))
          (move-overlay dank-posts-highlight-overlay 1 1))))))

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
  (point)
  (dank-posts-highlight-under-point))

(defun dank-posts-navigate-next-post ()
  "Move point to the beginning of next post."
  (interactive)
  (dank-posts--navigate-end-of-post)
  (next-line)
  (beginning-of-line)
  (point)
  (dank-posts-highlight-under-point))

(defun dank-posts-fetch-next-page ()
  (interactive)
  (dank-posts-set-page-posts dank-posts-current-subreddit
                             dank-posts-current-sorting
                             dank-posts-page-items-limit
                             dank-posts-current-end-count
                             dank-posts-current-after
                             nil)
  (goto-char (point-max))
  (dank-posts-render-current-page dank-posts-current-page-posts)
  (dank-posts-highlight-under-point))

(defun dank-posts-refresh ()
  (interactive)
  (dank-posts-reset-state dank-posts-current-subreddit dank-posts-current-sorting
                          dank-posts-page-items-limit)
  (dank-posts-render-current-page dank-posts-current-page-posts t)
  (dank-posts-highlight-under-point))

(defun dank-posts-goto-subreddit-at-point (point)
  "Navigate to a dank-posts-mode buffer for a post's subreddit under pointer."
  (interactive "d")
  (let* ((subreddit (dank-utils-get-prop point 'dank-post-subreddit)))
    (dank-posts-init subreddit)))

(defun dank-posts-goto-subreddit (subreddit)
  "Navigate to a dank-posts-mode for a specific subreddit."
  (interactive (list (completing-read "Go to subreddit: " dank-posts-known-subreddits)))
  (if (string-equal (substring subreddit 0 3) "/r/")
      (dank-posts-init (substring subreddit 3 (- (string-width subreddit) 1)))
    (dank-posts-init subreddit)))

(defun dank-posts-goto-post-comments (subreddit post-id permalink &optional sorting)
  (dank-comments-init subreddit post-id permalink (current-buffer) sorting))

(defun dank-posts-goto-post-comments-at-point (point)
  (interactive "d")
  (let* ((post-props (text-properties-at point))
         (post-id (plist-get post-props 'dank-post-id))
         (subreddit (plist-get post-props 'dank-post-subreddit))
         (permalink (plist-get post-props 'dank-post-permalink))
         (title (plist-get post-props 'dank-post-title)))
    (dank-posts-goto-post-comments subreddit post-id permalink
                                   dank-posts-current-sorting)))

(defun dank-posts--get-subscribed-subreddits-names ()
  "Get the authenticated user's list of subscribed subreddits."
  (sort (mapcar (lambda (s) (dank-subreddit-url s)) (mapcar #'dank-post-subreddit-parse (dank-backend-subreddits))) 'string<))

(defun dank-posts-browse-post-link-at-point (point)
  "Open the post link at POINT in a browser."
  (interactive "d")
  (let* ((post-link (dank-utils-get-prop point 'dank-post-link)))
    (browse-url post-link)))

(defun dank-posts-browse-post-comments-at-point (point)
  "Open the post comments at POINT in a browser."
  (interactive "d")
  (let ((post-permalink (dank-utils-get-prop point 'dank-post-permalink)))
    (browse-url (concat "https://old.reddit.com" post-permalink))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interaction functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'dank-posts)
