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
(defvar-local dank-comments-tree-fold-overlays '())

(defvar dank-comments-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'dank-comments-navigate-next-comment)
    (define-key map "p" 'dank-comments-navigate-prev-comment)
    (define-key map "P" 'dank-comments-navigate-to-parent)
    (define-key map (kbd "M-n") 'dank-comments-navigate-next-sibling)
    (define-key map (kbd "M-p") 'dank-comments-navigate-prev-sibling)
    ;(define-key map (kbd "C-c C-v") 'dank-posts-fetch-next-page)
    (define-key map (kbd "C-x C-r") 'dank-comments-refresh)
    ;(define-key map (kbd "C-c C-c") 'dank-posts-goto-post-comments-at-point)
    ;(define-key map (kbd "C-c C-/") 'dank-posts-goto-subreddit-at-point)
    (define-key map (kbd "TAB") 'dank-comments-toggle-comment-tree-fold)
    (define-key map (kbd "C-x q") 'kill-current-buffer)
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
        dank-comments-current-post nil
        dank-comments-tree-fold-overlays '())
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
      (let ((start (car exts))
            (end (cadr exts))
            (p (point)))
        (if (and (> (- end start) 1)
                 (<= p end))
            (move-overlay dank-comments-highlight-overlay start (+ 1 end))
          (move-overlay dank-comments-highlight-overlay 1 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; navigation functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dank-comments--navigate-beginning-of-comment ()
  "Move point to the beginning of the current comment."
  (interactive)
  (beginning-of-line)
  ;; TODO: maybe change this to look at text properties instead of regex
  (if (looking-at " *[-+] \\(/u/\\|\\[More\\)")
      ;; When point is behind the start of a comment, just move to the start
      (beginning-of-line-text)
    (let ((sreg "[-+] \\(/u/\\|\\[More\\)"))
      (unless (looking-at sreg)
        (re-search-backward sreg nil t))))
  (beginning-of-line-text)
  (when (string-equal (char-to-string (char-after)) "/")
    (backward-char 2))
  (point))

(defun dank-comments--navigate-end-of-comment ()
  "Move point to the end of the current comment."
  (interactive)
  (let ((comment-id (dank-utils-get-prop (point) 'dank-comment-id)))
    (if (looking-at " *\\+ \\[More comments\\]")
        (progn
          (end-of-line)
          (point))
      (progn
        ;; TODO: maybe change this to look at text properties instead of regex
        (let ((sreg " *[-+] \\(/u/\\|\\[More\\)"))
          ;; When point is already behind the start of a comment, move down first
          (when (looking-at sreg)
            (next-logical-line)))
        (let ((sreg "[-+] \\(/u/\\|\\[More\\)"))
          ;; Look for the start of the next comment then move up
          (unless (looking-at sreg)
            (re-search-forward sreg nil t)))
        ;; if we did not find a next comment, we are at the end of the buffer
        (if (string-equal comment-id (dank-utils-get-prop (point) 'dank-comment-id))
            (end-of-buffer))
        (previous-logical-line)
        (end-of-line)
        (point)))))

(defun dank-comments--find-comment-extents (pos)
  "Return list containing point for beginning and end of comment containing POS."
  ;; find the header then do beginning-of-line-text
  (interactive "d")
  (save-excursion
    (goto-char pos)
    (list (dank-comments--navigate-beginning-of-comment)
          (dank-comments--navigate-end-of-comment))))

(defun dank-comments--find-comment-tree-extents (pos &optional start-at-end-of-first-header)
  "Return a list containing point for beginning and end of a comment tree at POS.
When START-AT-END-OF-FIRST-HEADER is non-nil, exclude the body of the header of the
top comment from the extent range."
  (interactive "d")
  (save-excursion
    (goto-char pos)
    (let ((current-id (dank-utils-get-prop (point) 'dank-comment-id)))
      (list (progn (dank-comments--navigate-beginning-of-comment)
                   (when start-at-end-of-first-header
                     (end-of-line)
                     (backward-char)
                     (point)))
            (progn (dank-comments-navigate-next-root)
                   (unless (string-equal current-id (dank-utils-get-prop (point) 'dank-comment-id))
                       (dank-comments-navigate-prev-comment))
                   (dank-comments--navigate-end-of-comment))))))

(defun dank-comments-navigate-prev-comment ()
  "Move point to the beginning of the previous comment directly above."
  (interactive)
  (dank-comments--navigate-beginning-of-comment)
  (previous-logical-line)
  (dank-comments--navigate-beginning-of-comment)
  (point)
  (dank-comments-highlight-under-point))

(defun dank-comments-navigate-next-comment ()
  "Move point to the beginning of the next comment directly below."
  (interactive)
  (dank-comments--navigate-end-of-comment)
  (next-logical-line)
  (beginning-of-line-text)
  (dank-comments--navigate-beginning-of-comment)
  (point)
  (dank-comments-highlight-under-point))

(defun dank-comments-navigate-to-parent ()
  "Move point to the parent of the current comment."
  (interactive)
  (let* ((parent-id (dank-utils-get-prop (point) 'dank-comment-parent-id)))
    (when (and parent-id (string-prefix-p "t1_" parent-id))
      (previous-logical-line)
      (while (not (string-equal (substring parent-id 3) (dank-utils-get-prop (point) 'dank-comment-id)))
        (previous-logical-line))
      (dank-comments--navigate-beginning-of-comment)
      (dank-comments-highlight-under-point))))

(defun dank-comments-navigate-next-sibling ()
  "Move point to the beginning of the next sibling comment."
  (interactive)
  (let* ((current-point (point))
         (depth (dank-utils-get-prop (point) 'dank-comment-depth))
         (comment-id (dank-utils-get-prop (point) 'dank-comment-id))
         (parent-id (dank-utils-get-prop (point) 'dank-comment-parent-id)))
    (end-of-line)
    (backward-char)
    ;; keep moving down when we are not at the end of the buffer and
    ;; still on the same comment or until we are no longer under the same parent and a lower depth
    (while (and (not (eobp))
                (or (string-equal (dank-utils-get-prop (point) 'dank-comment-id) comment-id)
                    (and (not (string-equal (dank-utils-get-prop (point) 'dank-comment-parent-id) parent-id))
                         (>= (dank-utils-get-prop (point) 'dank-comment-depth) depth))))
      (next-logical-line)
      (beginning-of-line))
    ;; when we are no longer under the parent of where we started from, go back to where we started from
    (when (not (string-equal (dank-utils-get-prop (point) 'dank-comment-parent-id) parent-id))
      (backward-char (- (point) current-point)))
    (dank-comments--navigate-beginning-of-comment)
    (dank-comments-highlight-under-point)))

(defun dank-comments-navigate-prev-sibling ()
  "Move point to the beginning of the prev sibling comment."
  (interactive)
  (let* ((current-point (point))
         (depth (dank-utils-get-prop (point) 'dank-comment-depth))
         (comment-id (dank-utils-get-prop (point) 'dank-comment-id))
         (parent-id (dank-utils-get-prop (point) 'dank-comment-parent-id)))
    (end-of-line)
    (backward-char)
    ;; keep moving up when we are still on the same comment, or
    ;; until we are no longer under the same parent and a lower depth
    (while (or (string-equal (dank-utils-get-prop (point) 'dank-comment-id) comment-id)
               (and (not (string-equal (dank-utils-get-prop (point) 'dank-comment-parent-id) parent-id))
                    (>= (dank-utils-get-prop (point) 'dank-comment-depth) depth)))
      (previous-logical-line)
      (beginning-of-line))
    ;; when we are no longer under the parent of where we started from, go back to where we started from
    (when (not (string-equal (dank-utils-get-prop (point) 'dank-comment-parent-id) parent-id))
      (forward-char (- current-point (point))))
    (dank-comments--navigate-beginning-of-comment)
    (dank-comments-highlight-under-point)))

(defun dank-comments-navigate-next-root ()
  "Move point to the beginning of the next root comment.
The next root comment is either the next sibling, or if there is
no next sibling, the next comment that has a lower depth."
  (interactive)
  (let* ((current-point (point))
         (depth (dank-utils-get-prop (point) 'dank-comment-depth))
         (comment-id (dank-utils-get-prop (point) 'dank-comment-id))
         (parent-id (dank-utils-get-prop (point) 'dank-comment-parent-id)))
    (end-of-line)
    (backward-char)
    ;; keep moving down when we are not at the end of the buffer and
    ;; still on the same comment or until we are no longer under the same parent and a lower depth
    (while (and (not (eobp))
                (or (string-equal (dank-utils-get-prop (point) 'dank-comment-id) comment-id)
                    (> (dank-utils-get-prop (point) 'dank-comment-depth) depth)))
      (next-logical-line)
      (beginning-of-line))
    (dank-comments--navigate-beginning-of-comment)
    (dank-comments-highlight-under-point)))

(defun dank-comments-collapse-comment-tree ()
  "Collapse the comment tree at point."
  (interactive)
  ;; TODO: change the '-' to a '+'? or add ellipses at the end?
  ;; TODO: change style
  (let* ((exts (dank-comments--find-comment-tree-extents (point) t))
         (comment-id (dank-utils-get-prop (point) 'dank-comment-id))
         (start (car exts))
         (end (cadr exts))
         (existing-ovl (cdr (assoc comment-id dank-comments-tree-fold-overlays)))
         (ovl (if existing-ovl (move-overlay existing-ovl start end) (make-overlay start end))))
    (when comment-id
      (overlay-put ovl 'category 'dank-comments-tree)
      (overlay-put ovl 'dank-comments-tree-state 'collapsed)
      (overlay-put ovl 'dank-comments-tree-id comment-id)
      (overlay-put ovl 'after-string "...")
      (overlay-put ovl 'invisible t)
      (add-to-list 'dank-comments-tree-fold-overlays `(,comment-id . ,ovl))))
  (dank-comments-highlight-under-point))

(defun dank-comments-expand-comment-tree ()
  "Expand the collapsed comment tree at point."
  (interactive)
  ;; point might be at the end of the overlay with no text properties or overlay

  (move-beginning-of-line 1)
  (move-beginning-of-line 1)
  (let* ((comment-id (dank-utils-get-prop (point) 'dank-comment-id))
         (ovl (cdr (assoc comment-id dank-comments-tree-fold-overlays))))
    (when ovl
      (overlay-put ovl 'after-string "")
      (overlay-put ovl 'dank-comments-tree-state 'expanded)
      (overlay-put ovl 'invisible nil)
      (delete-overlay ovl))
    (dank-comments-highlight-under-point)))

(defun dank-comments-toggle-comment-tree-fold ()
  "Collapse or expand comment tree at point."
  (interactive)
  (let* ((comment-id (dank-utils-get-prop (point) 'dank-comment-id))
         (existing-ovl (cdr (assoc comment-id dank-comments-tree-fold-overlays))))
    (if (and existing-ovl (overlay-get existing-ovl 'invisible))
        (dank-comments-expand-comment-tree)
      (dank-comments-collapse-comment-tree))))

(defun dank-comments--point-on-last-sibling (pos)
  "Return non-nil if the comment under POS is the last sibling of the comment tree."
  (interactive "d")
  (save-excursion
    (let ((dank-comments-highlight-under-point-enabled nil)
          (current-id (dank-utils-get-prop (point) 'dank-comment-id)))
      (dank-comments-navigate-next-sibling)
      (string-equal current-id (dank-utils-get-prop (point) 'dank-comment-id)))))

(defun dank-comments-refresh ()
  (interactive)
  (dank-comments-reset-state)
  (dank-comments-render-current-post dank-comments-current-post t)
  (dank-comments-render-current-comments dank-comments-current-comments dank-comments-current-post)
  (goto-char 0))

(provide 'dank-comments)
