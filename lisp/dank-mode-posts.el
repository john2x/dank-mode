;;; dank-mode-posts.el --- Major mode for browsing Reddit

;; Copyright (C) 2021 John Louis Del Rosario

;; Author: John Louis Del Rosario <john2x@gmail.com>
;; Version: 0.2.0
;; Keywords: reddit, social

;;; Commentary:

;; This file defines the dank-mode-posts major mode for browsing Reddit
;; posts.
;; To start this mode, start a dank-mode-posts buffer via `M-x dank-mode-posts`.

;;; Code:

(require 'dank-mode-backend)
(require 'dank-mode-utils)
(require 'dank-mode-post)
(require 'dank-mode-faces)
(require 'dank-mode-comments)
(require 'browse-url)


(defvar dank-mode-posts-page-items-limit 25)
(defvar dank-mode-posts-subscribed-subreddits nil)
(defvar dank-mode-posts-sorting-options '(hot best new rising controversial top))


(defcustom dank-mode-posts-known-subreddits dank-mode-posts-subscribed-subreddits
  "List of known subreddits. Defaults to the authenticated user's subscribed subreddits.
Set a custom value if you want a different list."
  :type '(list string)
  :group 'dank-mode)

(defcustom dank-mode-posts-default-subreddit nil
  "Default subreddit to load when first invoking `M-x dank-mode'.
When nil, defaults to the frontpage."
  :type 'string
  :group 'dank-mode)

(defcustom dank-mode-posts-default-sorting 'hot
  "Default sorting when loading posts."
  :type 'symbol
  :group 'dank-mode
  :options '(hot new best))

(defcustom dank-mode-posts-highlight-under-point-enabled t
  "Highlight post under point."
  :type 'boolean
  :group 'dank-mode)

(defvar-local dank-mode-posts-buffer nil)
(defvar-local dank-mode-posts-current-subreddit dank-mode-posts-default-subreddit)
(defvar-local dank-mode-posts-current-sorting dank-mode-posts-default-sorting)
(defvar-local dank-mode-posts-current-start-count 0)
(defvar-local dank-mode-posts-current-end-count 0)
(defvar-local dank-mode-posts-current-after nil)
(defvar-local dank-mode-posts-current-before nil)
(defvar-local dank-mode-posts-current-page-posts nil)
(defvar-local dank-mode-posts-current-ewoc nil)

(defvar dank-mode-posts-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'dank-mode-posts-navigate-next-post)
    (define-key map "p" 'dank-mode-posts-navigate-prev-post)
    (define-key map (kbd "C-x C-v") 'dank-mode-posts-fetch-next-page)
    (define-key map (kbd "C-x C-r") 'dank-mode-posts-refresh)
    (define-key map (kbd "C-x C-s") 'dank-mode-posts-change-sorting)
    (define-key map (kbd "C-x C-o") 'dank-mode-posts-goto-post-comments-at-point)
    (define-key map (kbd "C-x C-/") 'dank-mode-posts-goto-subreddit-at-point)
    (define-key map (kbd "C-x C-f") 'dank-mode-posts-goto-subreddit)
    (define-key map (kbd "C-x l l") (lambda (point) (interactive "d") (dank-mode-posts-browse-post-link-at-point point t)))
    (define-key map (kbd "C-x l b") 'dank-mode-posts-browse-post-link-at-point)
    (define-key map (kbd "C-x l o") 'dank-mode-posts-browse-post-comments-at-point)
    (define-key map (kbd "C-x q") 'kill-current-buffer)
    map))

(define-derived-mode dank-mode-posts-mode special-mode "dank-mode-posts-mode"
  "Major mode for browsing reddit posts."
  (setq show-trailing-whitespace nil)
  (when (not dank-mode-posts-subscribed-subreddits)
    (setq dank-mode-posts-subscribed-subreddits (dank-mode-posts--get-subscribed-subreddits-names)))
  (when (not dank-mode-posts-known-subreddits)
    (setq dank-mode-posts-known-subreddits dank-mode-posts-subscribed-subreddits)))

(defun dank-mode-posts-init (&optional subreddit)
  "Initialize dank-mode-posts-mode buffer to SUBREDDIT.
If SUBREDDIT is nil, a frontpage buffer is initialized.
If a buffer already exists, switch to that buffer."
  (let ((buf (concat "*dank-mode-posts* " (if subreddit (concat "/r/" subreddit) "frontpage"))))
    (if (get-buffer buf)
        (progn
          (message "Switched to existing dank-mode-posts-mode buffer for %s..." (or subreddit "frontpage"))
          (switch-to-buffer buf))
      (progn
        (message "Initializing dank-mode-posts-mode buffer for %s..." (or subreddit "frontpage"))
        (switch-to-buffer buf)
        (dank-mode-posts-mode)
        (setq dank-mode-posts-buffer (current-buffer))
        (condition-case err
            (dank-mode-posts-reset-state subreddit dank-mode-posts-default-sorting dank-mode-posts-page-items-limit)
          (dank-mode-backend-error (progn (dank-mode-posts-render-error err)
                                          (signal (car err) (cdr err)))))
        (dank-mode-posts-render-current-page-ewoc dank-mode-posts-current-page-posts)
        (dank-mode-posts-highlight-under-point)))))


(defun dank-mode-posts-reset-state (subreddit sorting limit)
  "Reset state of the current dank-mode-posts buffer."
  (setq dank-mode-posts-current-subreddit subreddit
        dank-mode-posts-current-sorting sorting
        dank-mode-posts-current-start-count 0
        dank-mode-posts-current-end-count 0
        dank-mode-posts-current-after nil
        dank-mode-posts-current-before nil
        dank-mode-posts-current-page-posts nil
        dank-mode-posts-current-all-posts nil)
  (dank-mode-posts-set-page-posts subreddit sorting limit))


(defun dank-mode-posts-set-page-posts (subreddit sorting limit &optional count after before)
  "Get a page of posts from reddit.
Store the results in `dank-mode-posts-current-page-posts'."
  (let* ((posts (dank-mode-backend-post-listing subreddit sorting
                                                :limit limit :after after :before before :count count))
         (posts (mapcar #'dank-mode-post-parse posts)))
    (setq dank-mode-posts-current-page-posts posts
          dank-mode-posts-current-all-posts (append dank-mode-posts-current-page-posts posts)
          dank-mode-posts-current-subreddit subreddit
          dank-mode-posts-current-sorting sorting)
    ;; update navigation references
    (if (not before)
        (progn
          (setq dank-mode-posts-current-start-count (+ 1 dank-mode-posts-current-end-count)
                dank-mode-posts-current-end-count (+ dank-mode-posts-current-end-count
                                                dank-mode-posts-page-items-limit)))
      (progn
        (setq dank-mode-posts-current-end-count (- dank-mode-posts-current-start-count 1)
              dank-mode-posts-current-start-count (- dank-mode-posts-current-end-count
                                                dank-mode-posts-page-items-limit))))
    (if (<= dank-mode-posts-current-start-count 0)
        (setq dank-mode-posts-current-start-count 1))
    (setq dank-mode-posts-current-after (dank-mode-post-name (car (last posts)))
          dank-mode-posts-current-before (dank-mode-post-name (car posts)))
    (dank-mode-posts-set-header-line)))


(defun dank-mode-posts-render-current-page-ewoc (posts &optional refresh-ewoc)
  "Set `dank-mode-posts-current-ewoc' with POSTS and insert it into the current buffer.
Uses `dank-mode-post--ewoc-pp' as the ewoc pretty-printer.
REFRESH-EWOC creates a new ewoc."
  (when (and refresh-ewoc dank-mode-posts-current-ewoc)
    (ewoc-filter dank-mode-posts-current-ewoc (lambda (n) nil)))
  (let ((inhibit-read-only t))
    (delete-blank-lines))
  (when (or refresh-ewoc (not dank-mode-posts-current-ewoc))
    (setq dank-mode-posts-current-ewoc (ewoc-create #'dank-mode-post--ewoc-pp)))
  (dank-mode-posts--set-posts-ewoc dank-mode-posts-current-ewoc posts)
  (let ((inhibit-read-only t))
    (delete-blank-lines)))

(defun dank-mode-posts--set-posts-ewoc (ewoc posts)
  "Populate the EWOC with POSTS."
  (mapc (lambda (p) (ewoc-enter-last ewoc p)) posts))

(defun dank-mode-posts-append-post-to-buffer (buf post-index post)
  "Append POST into BUF.
POST-INDEX is the number (\"position\") of the post."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (let* ((inhibit-read-only t)
             (formatted-post (concat (dank-mode-post-format post post-index) "\n")))
        (save-excursion
          (goto-char (point-max))
          (insert formatted-post))))))

(defun dank-mode-posts-render-error (err)
  "Render contents of ERR into the current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "Uh oh! Something went wrong.\n")
    (insert (format "%s\n" err))
    (insert "Try killing this buffer with `C-x q` or `C-x k <buffer name> RET` and try again.")))

;;;;;;;;;;;;;;;;;;;
;; display stuff ;;
;;;;;;;;;;;;;;;;;;;

(defvar dank-mode-posts-header-line-format-template
  "${subreddit} - ${sorting} (${count})")

(defun dank-mode-posts-set-header-line ()
  (when (buffer-live-p dank-mode-posts-buffer)
    (with-current-buffer dank-mode-posts-buffer
      (setq header-line-format (dank-mode-utils-format-plist
                                dank-mode-posts-header-line-format-template
                                `(subreddit ,(or dank-mode-posts-current-subreddit "Frontpage")
                                            sorting ,(symbol-name dank-mode-posts-current-sorting)
                                            count ,dank-mode-posts-current-end-count))))))

;; this highlighting logic is copied from ledger-mode
;; an overlay needs to be set once in the buffer and moved around
(defvar-local dank-mode-posts-highlight-overlay (list))
(defun dank-mode-posts-highlight-under-point ()
  "Highlight post under point."
  (when dank-mode-posts-highlight-under-point-enabled
    (unless dank-mode-posts-highlight-overlay
      (setq dank-mode-posts-highlight-overlay (dank-mode-utils-make-highlight-overlay)))
    (let ((exts (dank-mode-posts--find-post-extents (point))))
      (let ((b (car exts))
            (e (cadr exts))
            (p (point)))
        (if (and (> (- e b) 1)
                 (<= p e) (>= p b))
            (move-overlay dank-mode-posts-highlight-overlay b (+ 1 e))
          (move-overlay dank-mode-posts-highlight-overlay 1 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; navigation functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dank-mode-posts--navigate-beginning-of-post (pos)
  "Move point to the beginning of the current post."
  (interactive "d")
  (let* ((node (ewoc-locate dank-mode-posts-current-ewoc pos)))
    (ewoc-goto-node dank-mode-posts-current-ewoc node))
  (beginning-of-line-text)
  (point))

(defun dank-mode-posts--navigate-end-of-post (pos)
  "Move point to the end of the current post."
  (interactive "d")
  (let* ((next-node (ewoc-goto-next dank-mode-posts-current-ewoc 1)))
    (if next-node
        (progn
          (previous-line)
          (end-of-line))
      (end-of-buffer)))
  (point))

(defun dank-mode-posts--find-post-extents (pos)
  "Return list containing point for beginning and end of post containing POS."
  (interactive "d")
  (save-excursion
    (goto-char pos)
    (list (dank-mode-posts--navigate-beginning-of-post pos)
          (dank-mode-posts--navigate-end-of-post pos))))


(defun dank-mode-posts-navigate-prev-post (pos)
  "Move point to the beginning of previous post."
  (interactive "d")
  (ewoc-goto-prev dank-mode-posts-current-ewoc 1)
  (dank-mode-posts-highlight-under-point))

(defun dank-mode-posts-navigate-next-post (pos)
  "Move point to the beginning of next post."
  (interactive "d")
  (ewoc-goto-next dank-mode-posts-current-ewoc 1)
  (dank-mode-posts-highlight-under-point))

(defun dank-mode-posts-fetch-next-page ()
  "Fetch the next page."
  (interactive)
  (dank-mode-posts-set-page-posts dank-mode-posts-current-subreddit
                             dank-mode-posts-current-sorting
                             dank-mode-posts-page-items-limit
                             dank-mode-posts-current-end-count
                             dank-mode-posts-current-after
                             nil)
  (goto-char (point-max))
  (dank-mode-posts-render-current-page-ewoc dank-mode-posts-current-page-posts)
  (dank-mode-posts-highlight-under-point))

(defun dank-mode-posts-refresh ()
  "Refresh the current dank-mode-posts buffer."
  (interactive)
  (dank-mode-posts-reset-state dank-mode-posts-current-subreddit dank-mode-posts-current-sorting
                          dank-mode-posts-page-items-limit)
  (dank-mode-posts-render-current-page-ewoc dank-mode-posts-current-page-posts t)
  (dank-mode-posts-highlight-under-point))

(defun dank-mode-posts-change-sorting (sorting)
  "Refresh the current dank-mode-posts buffer with a different SORTING."
  (interactive (list (completing-read "Sorting: " dank-mode-posts-sorting-options)))
  (dank-mode-posts-reset-state dank-mode-posts-current-subreddit (intern sorting)
                          dank-mode-posts-page-items-limit)
  (dank-mode-posts-render-current-page-ewoc dank-mode-posts-current-page-posts t)
  (dank-mode-posts-highlight-under-point))

(defun dank-mode-posts-goto-subreddit-at-point (pos)
  "Navigate to a dank-mode-posts-mode buffer for a post's subreddit under POINT."
  (interactive "d")
  (let* ((subreddit (dank-mode-post-subreddit (dank-mode-utils-ewoc-data dank-mode-posts-current-ewoc pos))))
    (dank-mode-posts-init subreddit)))

(defun dank-mode-posts-goto-subreddit (subreddit)
  "Navigate to a dank-mode-posts-mode for a specific SUBREDDIT."
  (interactive (list (completing-read "Go to subreddit: " dank-mode-posts-known-subreddits)))
  (if (string-equal (substring subreddit 0 3) "/r/")
      (dank-mode-posts-init (substring subreddit 3 (- (string-width subreddit) 1)))
    (dank-mode-posts-init subreddit)))

(defun dank-mode-posts-goto-post-comments (subreddit post-id permalink &optional sorting)
  "Open a dank-mode-comments buffer for SUBREDDIT, POST-ID, and PERMALINK.
Optional SORTING is the sort order for the comments."
  (dank-mode-comments-init subreddit post-id permalink (current-buffer) sorting))

(defun dank-mode-posts-goto-post-comments-at-point (pos)
  "Open a dank-mode-comments buffer for the post at POS."
  (interactive "d")
  (let* ((post (dank-mode-utils-ewoc-data dank-mode-posts-current-ewoc pos))
         (post-id (dank-mode-post-id post))
         (subreddit (dank-mode-post-subreddit post))
         (permalink (dank-mode-post-permalink post))
         (title (dank-mode-post-title post)))
    (dank-mode-posts-goto-post-comments subreddit post-id permalink
                                   dank-mode-posts-current-sorting)))

(defun dank-mode-posts--get-subscribed-subreddits-names ()
  "Get the authenticated user's list of subscribed subreddits."
  (sort (mapcar (lambda (s) (dank-subreddit-url s)) (mapcar #'dank-mode-post-subreddit-parse (dank-mode-backend-subreddits))) 'string<))

(defun dank-mode-posts-browse-post-link-at-point (pos &optional eww)
  "Open the post link at POS in a browser.
If EWW is non-nil, browse in eww instead of the browser."
  (interactive "d")
  (let* ((post-link (dank-mode-post-link (dank-mode-utils-ewoc-data dank-mode-posts-current-ewoc pos)))
         (browse-url-browser-function (if eww 'eww-browse-url 'browse-url-default-browser)))
    (browse-url post-link)))

(defun dank-mode-posts-browse-post-comments-at-point (pos &optional eww)
  "Open the post comments at POS in a browser.
If EWW is non-nil, browse in eww instead of the browser."
  (interactive "d")
  (let ((post-permalink (dank-mode-post-permalink (dank-mode-utils-ewoc-data dank-mode-posts-current-ewoc point)))
        (browse-url-browser-function (if eww 'eww-browse-url 'browse-url-default-browser)))
    (browse-url (concat "https://old.reddit.com" post-permalink))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interaction functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'dank-mode-posts)

;;; dank-mode-posts.el ends here
