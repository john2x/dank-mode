;;; dank-comments.el --- Major mode for browsing Reddit

;; Copyright (C) 2021 John Louis Del Rosario

;; Author: John Louis Del Rosario <john2x@gmail.com>
;; Version: 0.1.0
;; Keywords: reddit, social

;;; Commentary:

;; This file defines the dank-comments major mode for reading Reddit
;; comments.
;; To start this mode, first start a dank-posts buffer via `M-x dank-posts`
;; then open a post's comments via `M-x dank-posts-goto-post-comments-at-point`.

;;; Code:

(require 'dank-auth)
(require 'dank-backend)
(require 'dank-utils)
(require 'dank-post)
(require 'dank-faces)
(require 'dank-comment)
(require 'ewoc)
(require 's)

(defvar dank-comments-sorting-options '(top best new controversial old qa))

(defcustom dank-comments-default-depth 10
  "Default depth of the comment tree to initially fetch."
  :type 'integer
  :group 'dank-mode)

(defcustom dank-comments-highlight-under-point-enabled t
  "Highlight the comment under point."
  :type 'boolean
  :group 'dank-mode)

(defvar-local dank-comments-buffer nil)
(defvar-local dank-comments-current-permalink nil)
(defvar-local dank-comments-current-post-id nil)
(defvar-local dank-comments-current-subreddit nil)
(defvar-local dank-comments-current-sorting 'top)
(defvar-local dank-comments-current-post nil)
(defvar-local dank-comments-current-comments nil)
(defvar-local dank-comments-current-source-buffer nil)
(defvar-local dank-comments-current-starting-comment-id nil)
(defvar-local dank-comments-tree-fold-overlays '())
(defvar-local dank-comments-current-ewoc nil)

(defvar dank-comments-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'dank-comments-navigate-next-comment)
    (define-key map "p" 'dank-comments-navigate-prev-comment)
    (define-key map "P" 'dank-comments-navigate-to-parent)
    (define-key map (kbd "M-n") 'dank-comments-navigate-next-sibling)
    (define-key map (kbd "M-p") 'dank-comments-navigate-prev-sibling)
    (define-key map (kbd "C-x C-r") 'dank-comments-refresh)
    (define-key map (kbd "C-x C-o") 'dank-comments-open-more-comments-at-point)
    (define-key map (kbd "C-x l l") (lambda () (interactive) (dank-comments-browse-post-link t)))
    (define-key map (kbd "C-x l o") 'dank-comments-browse-post-link)
    (define-key map (kbd "C-x l o") 'dank-comments-browse-post-comments)
    (define-key map (kbd "TAB") 'dank-comments-toggle-comment-tree-fold-at-point)
    (define-key map (kbd "C-x q") 'dank-comments-kill-current-buffer)
    map))

(define-derived-mode dank-comments-mode special-mode "dank-comments-mode"
  "Major mode for reading reddit post comments."
  (setq show-trailing-whitespace nil))

(defun dank-comments-init (subreddit post-id permalink source-buffer &optional sorting starting-comment-id)
  "Initialize a dank-comments buffer with SUBREDDIT, POST-ID, and PERMALINK.
SOURCE-BUFFER is the dank-posts buffer we came from.
Optional SORTING, when non-nil, will sort the comments by that order.
Optional STARTING-COMMENT-ID will start the comment tree at the comment (instead of the full comment tree)."
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
              dank-comments-current-permalink permalink
              dank-comments-current-starting-comment-id starting-comment-id
              dank-comments-current-sorting (or sorting 'top)
              dank-comments-current-source-buffer source-buffer)
        (condition-case err
            (dank-comments-reset-state sorting)
          (dank-backend-error (progn (dank-comments-render-error err)
                                     (signal (car err) (cdr err)))))
        (dank-comments-render-current-post-and-comments-ewoc dank-comments-current-post dank-comments-current-comments)))))

(defun dank-comments-reset-state (sorting)
  "Reset state of the current dank-posts buffer."
  (mapcar (lambda (c)
            (delete-overlay (cdr c)))
          dank-comments-tree-fold-overlays)
  (setq dank-comments-current-comments nil
        dank-comments-current-post nil
        dank-comments-current-sorting sorting)
  (dank-comments-set-current-post-and-comments dank-comments-current-subreddit
                                               dank-comments-current-post-id
                                               dank-comments-current-sorting
                                               dank-comments-current-starting-comment-id))

(defun dank-comments-set-current-post-and-comments (subreddit post-id &optional sorting starting-comment-id)
  "Set the buffer local variables for the post and comments contents."
  (let* ((post-comments (dank-backend-post-and-comments-listing subreddit post-id sorting
                                                                :depth dank-comments-default-depth
                                                                :comment starting-comment-id))
         (post (dank-post-parse (car post-comments)))
         (post-author (dank-post-author post))
         (comments (mapcar (lambda (c) (dank-comment-parse c post-author)) (cdr post-comments))))
    (setq dank-comments-current-post post
          dank-comments-current-comments comments)
    (dank-comments-set-header-line)))

(defun dank-comments--set-comments-ewoc (ewoc comments)
  "Populate the EWOC with COMMENTS."
  (let* ((flattened-comments (flatten-tree comments)))
    (mapc (lambda (c) (ewoc-enter-last ewoc c)) flattened-comments)
    ewoc))

(defun dank-comments--insert-comments-at-pos-ewoc (ewoc comments pos)
  "Insert COMMENTS into EWOC at POS.
This deletes the ewoc node at POS before inserting COMMENTS."
  (let* ((inhibit-read-only t)
         (node (ewoc-locate ewoc pos))
         (delete-node node)
         (flattened-comments (flatten-tree comments)))
    (while (consp flattened-comments)
      (setq node (ewoc-enter-after ewoc node (pop flattened-comments))))
    (ewoc-delete ewoc delete-node)))

(defun dank-comments-insert-more-comments-at-point (point)
  "Fetch more comments for the placeholder at POINT and insert the contents in its place."
  (interactive "d")
  (when (and (eq (dank-utils-get-prop point 'dank-comment-type) 'more)
             (> (dank-utils-get-prop point 'dank-comment-count) 0))
    (let* ((post-id (concat "t3_" dank-comments-current-post-id))
           (post-author (dank-post-author dank-comments-current-post))
           (current-depth (dank-utils-get-prop point 'dank-comment-depth))
           (children-ids (string-join (dank-utils-get-prop point 'dank-comment-children-ids) ","))
           (comments-raw (dank-backend-more-children post-id children-ids dank-comments-current-sorting))
           (comments (mapcar (lambda (c) (dank-comment-parse c post-author)) comments-raw)))
      (save-excursion
        (dank-comments--insert-comments-at-pos-ewoc dank-comments-current-ewoc
                                                    comments point))
      (beginning-of-line-text)
      (dank-comments-highlight-under-point))))

(defun dank-comments-continue-thread-at-point (point)
  "Fetch even more comments for the placeholder at POINT and open a new buffer for the tree."
  (interactive "d")
  (when (and (eq (dank-utils-get-prop point 'dank-comment-type) 'more)
             (= (dank-utils-get-prop point 'dank-comment-count) 0))
    (let* ((starting-comment-id (substring (dank-utils-get-prop point 'dank-comment-parent-id) 3))
           (permalink (concat dank-comments-current-permalink starting-comment-id)))
      (dank-comments-init dank-comments-current-subreddit dank-comments-current-post-id
                          permalink (current-buffer)
                          dank-comments-current-sorting starting-comment-id))))

(defun dank-comments-open-more-comments-at-point (point)
  "Open more comments at POINT.
If it's a short tree, insert it at POINT.
If it's a long tree, open a new buffer for it."
  (interactive "d")
  (when (eq (dank-utils-get-prop point 'dank-comment-type) 'more)
    (if (> (dank-utils-get-prop point 'dank-comment-count) 0)
        (dank-comments-insert-more-comments-at-point point)
      (dank-comments-continue-thread-at-point point))))

(defun dank-comments-render-current-post-and-comments-ewoc (post comments &optional refresh-ewoc)
  "Set `dank-comments-current-ewoc' with POST and COMMENTS and insert it into the current buffer.
Uses `dank-comment--ewoc-pp' as the ewoc pretty-printer.
REFRESH-EWOC creates a new ewoc."

  (when (and refresh-ewoc dank-comments-current-ewoc)
    (ewoc-filter dank-comments-current-ewoc (lambda (n) nil)))

  (when (or refresh-ewoc (not dank-comments-current-ewoc))
    (setq dank-comments-current-ewoc
          (ewoc-create #'dank-comment--ewoc-pp)))

  (ewoc-enter-first dank-comments-current-ewoc post)
  (let ((inhibit-read-only t))
    (delete-blank-lines))
  (dank-comments--set-comments-ewoc dank-comments-current-ewoc comments))

(defun dank-comments-render-error (err)
  "Render the ERR message in the current buffer and show recommended actions."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "Uh oh! Something went wrong.")
    (insert (format "%s\n" err))
    (insert "Try killing this buffer with `C-x q` or `C-x k <buffer name> RET` and try again.")))

(defun dank-comments-set-header-line ()
  "Set the header line of a dank-comments buffer."
  (when (buffer-live-p dank-comments-buffer)
    (with-current-buffer dank-comments-buffer
      (setq header-line-format (dank-utils-format-plist
                                dank-comments-header-line-format-template
                                `(subreddit ,dank-comments-current-subreddit
                                            starting-comment-id ,(or dank-comments-current-starting-comment-id "")
                                            sorting ,(symbol-name dank-comments-current-sorting)))))))

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

(defun dank-comments--navigate-beginning-of-comment (pos)
  "Move point to the beginning of the current comment."
  (interactive "d")
  (let* ((ewoc-node (ewoc-locate dank-comments-current-ewoc pos)))
    (ewoc-goto-node dank-comments-current-ewoc ewoc-node))
  (beginning-of-line-text)
  (point))

(defun dank-comments--navigate-end-of-comment (pos)
  "Move point to the end of the current comment."
  (interactive "d")
  (let* ((next-node (ewoc-goto-next dank-comments-current-ewoc 1)))
    (if next-node
        (progn
          (previous-line)
          (end-of-line))
      (end-of-buffer)))
  (point))

(defun dank-comments--find-comment-extents (pos)
  "Return list containing point for beginning and end of comment containing POS."
  ;; find the header then do beginning-of-line-text
  (interactive "d")
  (save-excursion
    (goto-char pos)
    (list (dank-comments--navigate-beginning-of-comment pos)
          (dank-comments--navigate-end-of-comment pos))))

(defun dank-comments--find-comment-tree-extents (pos &optional start-at-end-of-first-header)
  "Return a list containing point for beginning and end of a comment tree at POS.
When START-AT-END-OF-FIRST-HEADER is non-nil, exclude the body of the header of the
top comment from the extent range. This is useful for folding the comment body only."
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

(defun dank-comments-navigate-prev-comment (pos)
  "Move point to the beginning of the previous comment directly above."
  (interactive "d")
  (ewoc-goto-prev dank-comments-current-ewoc 1)
  (point)
  (dank-comments-highlight-under-point))

(defun dank-comments-navigate-next-comment (pos)
  "Move point to the beginning of the next comment directly below."
  (interactive "d")
  (ewoc-goto-next dank-comments-current-ewoc 1)
  (point)
  (dank-comments-highlight-under-point))

(defun dank-comments-navigate-to-parent (pos)
  "Move point to the parent of the current comment."
  (interactive "d")
  (let* ((node (ewoc-locate dank-comments-current-ewoc pos))
         (comment (ewoc-data node))
         (parent-id (substring (dank-comment--parent-id comment) 3)))
    (ewoc-goto-node
     dank-comments-current-ewoc
     (dank-utils-ewoc-next-match-node dank-comments-current-ewoc node
       (lambda (d)
         (when (dank-comment-p d)
           (string-equal parent-id (dank-comment-id d))))
       #'ewoc-prev)))
  (beginning-of-line-text)
  (dank-comments-highlight-under-point))

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

(defun dank-comments-collapse-comment-tree-at-point (point)
  "Collapse the comment tree at POINT."
  (interactive "d")
  ;; TODO: change the '-' to a '+'? or add ellipses at the end?
  ;; TODO: change style
  (let* ((exts (dank-comments--find-comment-tree-extents point t))
         (comment-id (dank-utils-get-prop point 'dank-comment-id))
         (comment-type (dank-utils-get-prop point 'dank-comment-type))
         (start (car exts))
         (end (cadr exts))
         (existing-ovl (cdr (assoc comment-id dank-comments-tree-fold-overlays)))
         (ovl (if existing-ovl (move-overlay existing-ovl start end) (make-overlay start end))))
    (when (and comment-id (eq comment-type 'comment))
      (overlay-put ovl 'category 'dank-comments-tree)
      (overlay-put ovl 'dank-comments-tree-state 'collapsed)
      (overlay-put ovl 'dank-comments-tree-id comment-id)
      (overlay-put ovl 'after-string "...")
      (overlay-put ovl 'invisible t)
      (add-to-list 'dank-comments-tree-fold-overlays `(,comment-id . ,ovl))))
  (dank-comments-highlight-under-point))

(defun dank-comments-expand-comment-tree-at-point (point)
  "Expand the collapsed comment tree at POINT."
  (interactive "d")
  ;; point might be at the end of the overlay with no text properties or overlay
  (move-beginning-of-line 1)
  (move-beginning-of-line 1)
  (let* ((comment-id (dank-utils-get-prop point 'dank-comment-id))
         (comment-type (dank-utils-get-prop point 'dank-comment-type))
         (ovl (cdr (assoc comment-id dank-comments-tree-fold-overlays))))
    (when (and comment-id ovl (eq comment-type 'comment))
      (overlay-put ovl 'after-string "")
      (overlay-put ovl 'dank-comments-tree-state 'expanded)
      (overlay-put ovl 'invisible nil)
      (delete-overlay ovl))
    (dank-comments-highlight-under-point)))

(defun dank-comments-toggle-comment-tree-fold-at-point (point)
  "Collapse or expand comment tree at POINT."
  (interactive "d")
  (let* ((comment-id (dank-utils-get-prop point 'dank-comment-id))
         (existing-ovl (cdr (assoc comment-id dank-comments-tree-fold-overlays))))
    (if (and existing-ovl (overlay-get existing-ovl 'invisible))
        (dank-comments-expand-comment-tree-at-point point)
      (dank-comments-collapse-comment-tree-at-point point))))

(defun dank-comments--point-on-last-sibling (pos)
  "Return non-nil if the comment under POS is the last sibling of the comment tree."
  (interactive "d")
  (save-excursion
    (let ((dank-comments-highlight-under-point-enabled nil)
          (current-id (dank-utils-get-prop (point) 'dank-comment-id)))
      (dank-comments-navigate-next-sibling)
      (string-equal current-id (dank-utils-get-prop (point) 'dank-comment-id)))))

(defun dank-comments-refresh ()
  "Refresh the comments of the current buffer."
  (interactive)
  (dank-comments-reset-state dank-comments-current-sorting)
  (dank-comments-render-current-post-and-comments-ewoc dank-comments-current-post dank-comments-current-comments t))

(defun dank-comments-change-sorting (sorting)
  "Refresh the comments of the current buffer with a different SORTING."
  (interactive (list (completing-read "Sorting: " dank-comments-sorting-options)))
  (dank-comments-reset-state (intern sorting))
  (dank-comments-render-current-post-and-comments-ewoc dank-comments-current-post dank-comments-current-comments t))

(defun dank-comments-kill-current-buffer ()
  "Kill the current dank-comments buffer and switch back to the source buffer."
  (interactive)
  (let ((current-buffer (current-buffer)))
    (when dank-comments-current-source-buffer
      (switch-to-buffer dank-comments-current-source-buffer))
    (kill-buffer current-buffer)))

(defun dank-comments-browse-post-link (&optional eww)
  "Open the current post link in a browser.
If EWW is non-nil, browse in eww instead of the browser."
  (interactive)
  (let* ((post-link (dank-post-link dank-comments-current-post))
         (browse-url-browser-function (if eww 'eww-browse-url 'browse-url-default-browser)))
    (browse-url post-link)))

(defun dank-comments-browse-post-comments (&optional eww)
  "Open the current comments in a browser.
If EWW is non-nil, browse in eww instead of the browser."
  (interactive)
  (let ((browse-url-browser-function (if eww 'eww-browse-url 'browse-url-default-browser)))
    (browse-url (concat "https://old.reddit.com" dank-comments-current-permalink))))

(provide 'dank-comments)
;;; dank-comments.el ends here
