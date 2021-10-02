;;; dank-posts.el --- Major mode for browsing Reddit

;; Copyright (C) 2021 John Louis Del Rosario

;; Author: John Louis Del Rosario <john2x@gmail.com>
;; Version: 0.1.5
;; Keywords: reddit, social

;;; Commentary:

;; This file defines the dank-posts major mode for browsing Reddit
;; posts.
;; To start this mode, start a dank-posts buffer via `M-x dank-posts`.

;;; Code:

(require 'dank-auth)
(require 'dank-backend)
(require 'dank-utils)
(require 'dank-post)
(require 'dank-faces)
(require 'dank-comments)
(require 'browse-url)


(defvar dank-posts-page-items-limit 25)
(defvar dank-posts-subscribed-subreddits nil)
(defvar dank-posts-sorting-options '(hot best new rising controversial top))


(defcustom dank-posts-known-subreddits dank-posts-subscribed-subreddits
  "List of known subreddits. Defaults to the authenticated user's subscribed subreddits.
Set a custom value if you want a different list."
  :type '(list string)
  :group 'dank-mode)

(defcustom dank-posts-default-subreddit nil
  "Default subreddit to load when first invoking `M-x dank-mode'.
When nil, defaults to the frontpage."
  :type 'string
  :group 'dank-mode)

(defcustom dank-posts-default-sorting 'hot
  "Default sorting when loading posts."
  :type 'symbol
  :group 'dank-mode
  :options '(hot new best))

(defcustom dank-posts-highlight-under-point-enabled t
  "Highlight post under point."
  :type 'boolean
  :group 'dank-mode)

(defvar-local dank-posts-buffer nil)
(defvar-local dank-posts-current-subreddit dank-posts-default-subreddit)
(defvar-local dank-posts-current-sorting dank-posts-default-sorting)
(defvar-local dank-posts-current-start-count 0)
(defvar-local dank-posts-current-end-count 0)
(defvar-local dank-posts-current-after nil)
(defvar-local dank-posts-current-before nil)
(defvar-local dank-posts-current-page-posts nil)
(defvar-local dank-posts-current-ewoc nil)

(defvar dank-posts-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'dank-posts-navigate-next-post)
    (define-key map "p" 'dank-posts-navigate-prev-post)
    (define-key map (kbd "C-x C-v") 'dank-posts-fetch-next-page)
    (define-key map (kbd "C-x C-r") 'dank-posts-refresh)
    (define-key map (kbd "C-x C-s") 'dank-posts-change-sorting)
    (define-key map (kbd "C-x C-o") 'dank-posts-goto-post-comments-at-point)
    (define-key map (kbd "C-x C-/") 'dank-posts-goto-subreddit-at-point)
    (define-key map (kbd "C-x C-f") 'dank-posts-goto-subreddit)
    (define-key map (kbd "C-x l l") (lambda (point) (interactive "d") (dank-posts-browse-post-link-at-point point t)))
    (define-key map (kbd "C-x l b") 'dank-posts-browse-post-link-at-point)
    (define-key map (kbd "C-x l o") 'dank-posts-browse-post-comments-at-point)
    (define-key map (kbd "C-x q") 'kill-current-buffer)
    map))

(define-derived-mode dank-posts-mode special-mode "dank-posts-mode"
  "Major mode for browsing reddit posts."
  (setq show-trailing-whitespace nil)
  (when (not dank-posts-subscribed-subreddits)
    (setq dank-posts-subscribed-subreddits (dank-posts--get-subscribed-subreddits-names)))
  (when (not dank-posts-known-subreddits)
    (setq dank-posts-known-subreddits dank-posts-subscribed-subreddits)))

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
        (dank-posts-render-current-page-ewoc dank-posts-current-page-posts)
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


(defun dank-posts-render-current-page-ewoc (posts &optional refresh-ewoc)
  "Set `dank-posts-current-ewoc' with POSTS and insert it into the current buffer.
Uses `dank-post--ewoc-pp' as the ewoc pretty-printer.
REFRESH-EWOC creates a new ewoc."
  (when (and refresh-ewoc dank-posts-current-ewoc)
    (ewoc-filter dank-posts-current-ewoc (lambda (n) nil)))
  (let ((inhibit-read-only t))
    (delete-blank-lines))
  (when (or refresh-ewoc (not dank-posts-current-ewoc))
    (setq dank-posts-current-ewoc (ewoc-create #'dank-post--ewoc-pp)))
  (dank-posts--set-posts-ewoc dank-posts-current-ewoc posts)
  (let ((inhibit-read-only t))
    (delete-blank-lines)))

(defun dank-posts--set-posts-ewoc (ewoc posts)
  "Populate the EWOC with POSTS."
  (mapc (lambda (p) (ewoc-enter-last ewoc p)) posts))

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

(defun dank-posts-render-error (err)
  "Render contents of ERR into the current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "Uh oh! Something went wrong.")
    (insert (format "%s\n" err))
    (insert "Try killing this buffer with `C-x q` or `C-x k <buffer name> RET` and try again.")))

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

(defun dank-posts--navigate-beginning-of-post (pos)
  "Move point to the beginning of the current post."
  (interactive "d")
  (let* ((node (ewoc-locate dank-posts-current-ewoc pos)))
    (ewoc-goto-node dank-posts-current-ewoc node))
  (beginning-of-line-text)
  (point))

(defun dank-posts--navigate-end-of-post (pos)
  "Move point to the end of the current post."
  (interactive "d")
  (let* ((next-node (ewoc-goto-next dank-posts-current-ewoc 1)))
    (if next-node
        (progn
          (previous-line)
          (end-of-line))
      (end-of-buffer)))
  (point))

(defun dank-posts--find-post-extents (pos)
  "Return list containing point for beginning and end of post containing POS."
  (interactive "d")
  (save-excursion
    (goto-char pos)
    (list (dank-posts--navigate-beginning-of-post pos)
          (dank-posts--navigate-end-of-post pos))))


(defun dank-posts-navigate-prev-post (pos)
  "Move point to the beginning of previous post."
  (interactive "d")
  (ewoc-goto-prev dank-posts-current-ewoc 1)
  (dank-posts-highlight-under-point))

(defun dank-posts-navigate-next-post (pos)
  "Move point to the beginning of next post."
  (interactive "d")
  (ewoc-goto-next dank-posts-current-ewoc 1)
  (dank-posts-highlight-under-point))

(defun dank-posts-fetch-next-page ()
  "Fetch the next page."
  (interactive)
  (dank-posts-set-page-posts dank-posts-current-subreddit
                             dank-posts-current-sorting
                             dank-posts-page-items-limit
                             dank-posts-current-end-count
                             dank-posts-current-after
                             nil)
  (goto-char (point-max))
  (dank-posts-render-current-page-ewoc dank-posts-current-page-posts)
  (dank-posts-highlight-under-point))

(defun dank-posts-refresh ()
  "Refresh the current dank-posts buffer."
  (interactive)
  (dank-posts-reset-state dank-posts-current-subreddit dank-posts-current-sorting
                          dank-posts-page-items-limit)
  (dank-posts-render-current-page-ewoc dank-posts-current-page-posts t)
  (dank-posts-highlight-under-point))

(defun dank-posts-change-sorting (sorting)
  "Refresh the current dank-posts buffer with a different SORTING."
  (interactive (list (completing-read "Sorting: " dank-posts-sorting-options)))
  (dank-posts-reset-state dank-posts-current-subreddit (intern sorting)
                          dank-posts-page-items-limit)
  (dank-posts-render-current-page-ewoc dank-posts-current-page-posts t)
  (dank-posts-highlight-under-point))

(defun dank-posts-goto-subreddit-at-point (pos)
  "Navigate to a dank-posts-mode buffer for a post's subreddit under POINT."
  (interactive "d")
  (let* ((subreddit (dank-post-subreddit (dank-utils-ewoc-data dank-posts-current-ewoc pos))))
    (dank-posts-init subreddit)))

(defun dank-posts-goto-subreddit (subreddit)
  "Navigate to a dank-posts-mode for a specific SUBREDDIT."
  (interactive (list (completing-read "Go to subreddit: " dank-posts-known-subreddits)))
  (if (string-equal (substring subreddit 0 3) "/r/")
      (dank-posts-init (substring subreddit 3 (- (string-width subreddit) 1)))
    (dank-posts-init subreddit)))

(defun dank-posts-goto-post-comments (subreddit post-id permalink &optional sorting)
  "Open a dank-comments buffer for SUBREDDIT, POST-ID, and PERMALINK.
Optional SORTING is the sort order for the comments."
  (dank-comments-init subreddit post-id permalink (current-buffer) sorting))

(defun dank-posts-goto-post-comments-at-point (pos)
  "Open a dank-comments buffer for the post at POS."
  (interactive "d")
  (let* ((post (dank-utils-ewoc-data dank-posts-current-ewoc pos))
         (post-id (dank-post-id post))
         (subreddit (dank-post-subreddit post))
         (permalink (dank-post-permalink post))
         (title (dank-post-title post)))
    (dank-posts-goto-post-comments subreddit post-id permalink
                                   dank-posts-current-sorting)))

(defun dank-posts--get-subscribed-subreddits-names ()
  "Get the authenticated user's list of subscribed subreddits."
  (sort (mapcar (lambda (s) (dank-subreddit-url s)) (mapcar #'dank-post-subreddit-parse (dank-backend-subreddits))) 'string<))

(defun dank-posts-browse-post-link-at-point (pos &optional eww)
  "Open the post link at POS in a browser.
If EWW is non-nil, browse in eww instead of the browser."
  (interactive "d")
  (let* ((post-link (dank-post-link (dank-utils-ewoc-data dank-posts-current-ewoc pos)))
         (browse-url-browser-function (if eww 'eww-browse-url 'browse-url-default-browser)))
    (browse-url post-link)))

(defun dank-posts-browse-post-comments-at-point (pos &optional eww)
  "Open the post comments at POS in a browser.
If EWW is non-nil, browse in eww instead of the browser."
  (interactive "d")
  (let ((post-permalink (dank-post-permalink (dank-utils-ewoc-data dank-posts-current-ewoc point)))
        (browse-url-browser-function (if eww 'eww-browse-url 'browse-url-default-browser)))
    (browse-url (concat "https://old.reddit.com" post-permalink))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interaction functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'dank-posts)

;;; dank-posts.el ends here
