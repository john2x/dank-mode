;;; dank-mode-comments.el --- Major mode for browsing Reddit -*- lexical-binding: t -*-

;; Copyright (C) 2021 John Louis Del Rosario

;; Author: John Louis Del Rosario <john2x@gmail.com>
;; Version: 0.2.0
;; Keywords: reddit, social

;;; Commentary:

;; This file defines the dank-mode-comments major mode for reading Reddit
;; comments.
;; To start this mode, first start a dank-mode-posts buffer via `M-x dank-mode-posts`
;; then open a post's comments via `M-x dank-mode-posts-goto-post-comments-at-point`.

;;; Code:

(require 'dank-mode-backend)
(require 'dank-mode-utils)
(require 'dank-mode-post)
(require 'dank-mode-faces)
(require 'dank-mode-comment)
(require 'ewoc)

(defvar dank-mode-comments-sorting-options '(top best new controversial old qa))

(defcustom dank-mode-comments-default-depth 10
  "Default depth of the comment tree to initially fetch."
  :type 'integer
  :group 'dank-mode)

(defcustom dank-mode-comments-highlight-under-point-enabled t
  "Highlight the comment under point."
  :type 'boolean
  :group 'dank-mode)

(defvar-local dank-mode-comments-buffer nil)
(defvar-local dank-mode-comments-current-permalink nil)
(defvar-local dank-mode-comments-current-post-id nil)
(defvar-local dank-mode-comments-current-subreddit nil)
(defvar-local dank-mode-comments-current-sorting 'top)
(defvar-local dank-mode-comments-current-post nil)
(defvar-local dank-mode-comments-current-comments nil)
(defvar-local dank-mode-comments-current-source-buffer nil)
(defvar-local dank-mode-comments-current-starting-comment-id nil)
(defvar-local dank-mode-comments-tree-fold-overlays '())
(defvar-local dank-mode-comments-current-ewoc nil)

(defvar dank-mode-comments-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'dank-mode-comments-navigate-next-comment)
    (define-key map "p" 'dank-mode-comments-navigate-prev-comment)
    (define-key map "P" 'dank-mode-comments-navigate-to-parent)
    (define-key map (kbd "M-n") 'dank-mode-comments-navigate-next-sibling)
    (define-key map (kbd "M-p") 'dank-mode-comments-navigate-prev-sibling)
    (define-key map (kbd "C-x C-r") 'dank-mode-comments-refresh)
    (define-key map (kbd "C-x C-o") 'dank-mode-comments-open-more-comments-at-point)
    (define-key map (kbd "C-x l l") (lambda () (interactive) (dank-mode-comments-browse-post-link t)))
    (define-key map (kbd "C-x l o") 'dank-mode-comments-browse-post-link)
    (define-key map (kbd "C-x l o") 'dank-mode-comments-browse-post-comments)
    (define-key map (kbd "TAB") 'dank-mode-comments-toggle-comment-tree-fold-at-point)
    (define-key map (kbd "C-x q") 'dank-mode-comments-kill-current-buffer)
    map))

(define-derived-mode dank-mode-comments-mode special-mode "dank-mode-comments-mode"
  "Major mode for reading reddit post comments."
  (setq show-trailing-whitespace nil))

(defun dank-mode-comments-init (subreddit post-id permalink source-buffer &optional sorting starting-comment-id)
  "Initialize a dank-mode-comments buffer with SUBREDDIT, POST-ID, and PERMALINK.
SOURCE-BUFFER is the dank-mode-posts buffer we came from.
Optional SORTING, when non-nil, will sort the comments by that order.
Optional STARTING-COMMENT-ID will start the comment tree at the comment (instead of the full comment tree)."
  (let ((buf (concat "*dank-mode-comments* " permalink)))
    (if (get-buffer buf)
        (progn
          (message "Switched to existing dank-mode-comments-mode buffer %s" permalink)
          (switch-to-buffer buf))
      (progn
        (message "Initializing post comments buffer %s..." buf)
        (switch-to-buffer buf)
        (dank-mode-comments-mode)
        (setq dank-mode-comments-buffer (current-buffer)
              dank-mode-comments-current-subreddit subreddit
              dank-mode-comments-current-post-id post-id
              dank-mode-comments-current-permalink permalink
              dank-mode-comments-current-starting-comment-id starting-comment-id
              dank-mode-comments-current-sorting (or sorting 'top)
              dank-mode-comments-current-source-buffer source-buffer)
        (condition-case err
            (dank-mode-comments-reset-state sorting)
          (dank-mode-backend-error (progn (dank-mode-comments-render-error err)
                                          (signal (car err) (cdr err)))))
        (dank-mode-comments-render-current-post-and-comments-ewoc dank-mode-comments-current-post dank-mode-comments-current-comments)))))

(defun dank-mode-comments-reset-state (sorting)
  "Reset state of the current dank-mode-posts buffer."
  (mapcar (lambda (c)
            (delete-overlay (cdr c)))
          dank-mode-comments-tree-fold-overlays)
  (setq dank-mode-comments-current-comments nil
        dank-mode-comments-current-post nil
        dank-mode-comments-current-sorting sorting)
  (dank-mode-comments-set-current-post-and-comments dank-mode-comments-current-subreddit
                                                    dank-mode-comments-current-post-id
                                                    dank-mode-comments-current-sorting
                                                    dank-mode-comments-current-starting-comment-id))

(defun dank-mode-comments-set-current-post-and-comments (subreddit post-id &optional sorting starting-comment-id)
  "Set the buffer local variables for the post and comments contents."
  (let* ((post-comments (dank-mode-backend-post-and-comments-listing subreddit post-id sorting
                                                                     :depth dank-mode-comments-default-depth
                                                                     :comment starting-comment-id))
         (post (dank-mode-post-parse (car post-comments)))
         (post-author (dank-mode-post-author post))
         (comments (mapcar (lambda (c) (dank-mode-comment-parse c post-author)) (cdr post-comments))))
    (setq dank-mode-comments-current-post post
          dank-mode-comments-current-comments comments)
    (dank-mode-comments-set-header-line)))

(defun dank-mode-comments--set-comments-ewoc (ewoc comments)
  "Populate the EWOC with COMMENTS."
  (let* ((flattened-comments (flatten-tree comments)))
    (mapc (lambda (c) (ewoc-enter-last ewoc c)) flattened-comments)
    ewoc))

(defun dank-mode-comments--insert-comments-at-pos-ewoc (ewoc comments pos)
  "Insert COMMENTS into EWOC at POS.
This deletes the ewoc node at POS before inserting COMMENTS."
  (let* ((inhibit-read-only t)
         (node (ewoc-locate ewoc pos))
         (delete-node node)
         (flattened-comments (flatten-tree comments)))
    (while (consp flattened-comments)
      (setq node (ewoc-enter-after ewoc node (pop flattened-comments))))
    (ewoc-delete ewoc delete-node)))

(defun dank-mode-comments-insert-more-comments-at-point (pos)
  "Fetch more comments for the placeholder at POS and insert the contents in its place."
  (interactive "d")
  (when (and (eq (dank-mode-comment-type (dank-mode-utils-ewoc-data dank-mode-comments-current-ewoc pos)) 'more)
             (> (dank-mode-comment-more_count (dank-mode-utils-ewoc-data dank-mode-comments-current-ewoc pos)) 0))
    (let* ((ewoc dank-mode-comments-current-ewoc)
           (post-id (concat "t3_" dank-mode-comments-current-post-id))
           (post-author (dank-mode-post-author dank-mode-comments-current-post))
           (current-depth (dank-mode-comment-depth (dank-mode-utils-ewoc-data ewoc pos)))
           (children-ids (string-join (dank-mode-comment-children_ids (dank-mode-utils-ewoc-data ewoc pos)) ","))
           (comments-raw (dank-mode-backend-more-children post-id children-ids dank-mode-comments-current-sorting))
           (comments (mapcar (lambda (c) (dank-mode-comment-parse c post-author)) comments-raw)))
      (save-excursion
        (dank-mode-comments--insert-comments-at-pos-ewoc dank-mode-comments-current-ewoc
                                                         comments pos))
      (beginning-of-line-text)
      (dank-mode-comments-highlight-under-point))))

(defun dank-mode-comments-continue-thread-at-point (pos)
  "Fetch even more comments for the placeholder at POS and open a new buffer for the tree."
  (interactive "d")
  (when (and (eq (dank-mode-comment-type (dank-mode-utils-ewoc-data dank-mode-comments-current-ewoc pos)) 'more)
             (= (dank-mode-comment-more_count (dank-mode-utils-ewoc-data dank-mode-comments-current-ewoc pos)) 0))
    (let* ((starting-comment-id (substring (dank-mode-comment-parent_id (dank-mode-utils-ewoc-data dank-mode-comments-current-ewoc pos)) 3))
           (permalink (concat dank-mode-comments-current-permalink starting-comment-id)))
      (dank-mode-comments-init dank-mode-comments-current-subreddit dank-mode-comments-current-post-id
                          permalink (current-buffer)
                          dank-mode-comments-current-sorting starting-comment-id))))

(defun dank-mode-comments-open-more-comments-at-point (pos)
  "Open more comments at POS.
If it's a short tree, insert it at POS.
If it's a long tree, open a new buffer for it."
  (interactive "d")
  (when (eq (dank-mode-comment-type (dank-mode-utils-ewoc-data dank-mode-comments-current-ewoc pos)) 'more)
    (if (> (dank-mode-comment-more_count (dank-mode-utils-ewoc-data dank-mode-comments-current-ewoc pos)) 0)
        (dank-mode-comments-insert-more-comments-at-point pos)
      (dank-mode-comments-continue-thread-at-point pos))))

(defun dank-mode-comments-render-current-post-and-comments-ewoc (post comments &optional refresh-ewoc)
  "Set `dank-mode-comments-current-ewoc' with POST and COMMENTS and insert it into the current buffer.
Uses `dank-mode-comment--ewoc-pp' as the ewoc pretty-printer.
REFRESH-EWOC creates a new ewoc."

  (when (and refresh-ewoc dank-mode-comments-current-ewoc)
    (ewoc-filter dank-mode-comments-current-ewoc (lambda (n) nil)))

  (when (or refresh-ewoc (not dank-mode-comments-current-ewoc))
    (setq dank-mode-comments-current-ewoc
          (ewoc-create #'dank-mode-comment--ewoc-pp)))

  (ewoc-enter-first dank-mode-comments-current-ewoc post)
  (let ((inhibit-read-only t))
    (delete-blank-lines))
  (dank-mode-comments--set-comments-ewoc dank-mode-comments-current-ewoc comments))

(defun dank-mode-comments-render-error (err)
  "Render the ERR message in the current buffer and show recommended actions."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "Uh oh! Something went wrong.\n")
    (insert (format "%s\n" err))
    (insert "Try killing this buffer with `C-x q` or `C-x k <buffer name> RET` and try again.")))

(defun dank-mode-comments-set-header-line ()
  "Set the header line of a dank-mode-comments buffer."
  (when (buffer-live-p dank-mode-comments-buffer)
    (with-current-buffer dank-mode-comments-buffer
      (setq header-line-format (dank-mode-utils-format-plist
                                dank-mode-comments-header-line-format-template
                                `(subreddit ,dank-mode-comments-current-subreddit
                                            starting-comment-id ,(or dank-mode-comments-current-starting-comment-id "")
                                            sorting ,(symbol-name dank-mode-comments-current-sorting)))))))

;; this highlighting logic is copied from ledger-mode
;; an overlay needs to be set once in the buffer and moved around
(defvar-local dank-mode-comments-highlight-overlay (list))
(defun dank-mode-comments-highlight-under-point ()
  "Highlight comment under point."
  (when dank-mode-comments-highlight-under-point-enabled
    (unless dank-mode-comments-highlight-overlay
      (setq dank-mode-comments-highlight-overlay (dank-mode-utils-make-highlight-overlay)))
    (let ((exts (dank-mode-comments--find-comment-extents (point))))
      (let ((start (car exts))
            (end (cadr exts))
            (p (point)))
        (if (and (> (- end start) 1)
                 (<= p end))
            (move-overlay dank-mode-comments-highlight-overlay start (+ 1 end))
          (move-overlay dank-mode-comments-highlight-overlay 1 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; navigation functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dank-mode-comments--navigate-beginning-of-comment (pos)
  "Move point to the beginning of the current comment."
  (interactive "d")
  (let* ((ewoc-node (ewoc-locate dank-mode-comments-current-ewoc pos)))
    (ewoc-goto-node dank-mode-comments-current-ewoc ewoc-node))
  (beginning-of-line-text)
  (point))

(defun dank-mode-comments--navigate-end-of-comment (pos)
  "Move point to the end of the current comment."
  (interactive "d")
  (let* ((next-node (ewoc-goto-next dank-mode-comments-current-ewoc 1)))
    (if next-node
        (progn
          (previous-line)
          (end-of-line))
      (end-of-buffer)))
  (point))

(defun dank-mode-comments--find-comment-extents (pos)
  "Return list containing point for beginning and end of comment containing POS."
  ;; find the header then do beginning-of-line-text
  (interactive "d")
  (save-excursion
    (goto-char pos)
    (list (dank-mode-comments--navigate-beginning-of-comment pos)
          (dank-mode-comments--navigate-end-of-comment pos))))

(defun dank-mode-comments--find-comment-tree-extents (pos &optional start-at-end-of-first-header)
  "Return a list containing point for beginning and end of a comment tree at POS.
When START-AT-END-OF-FIRST-HEADER is non-nil, exclude the body of the header of the
top comment from the extent range. This is useful for folding the comment body only."
  (interactive "d")
  (save-excursion
    (goto-char pos)
    (let ((current-id (dank-mode-comment-id (ewoc-data (ewoc-locate dank-mode-comments-current-ewoc pos))))
          (dank-mode-comments-highlight-under-point-enabled nil))
      (list (progn (dank-mode-comments--navigate-beginning-of-comment (point))
                   (when start-at-end-of-first-header
                     (end-of-line)
                     (backward-char)
                     (point)))
            (progn (dank-mode-comments-navigate-next-root (point))
                   (unless (string-equal current-id (dank-mode-comment-id (ewoc-data (ewoc-locate dank-mode-comments-current-ewoc (point)))))
                       (dank-mode-comments-navigate-prev-comment (point)))
                   (dank-mode-comments--navigate-end-of-comment (point))
                   (point))))))

(defun dank-mode-comments-navigate-prev-comment (pos)
  "Move point to the beginning of the previous comment directly above."
  (interactive "d")
  (ewoc-goto-prev dank-mode-comments-current-ewoc 1)
  (point)
  (dank-mode-comments-highlight-under-point))

(defun dank-mode-comments-navigate-next-comment (pos)
  "Move point to the beginning of the next comment directly below."
  (interactive "d")
  (ewoc-goto-next dank-mode-comments-current-ewoc 1)
  (point)
  (dank-mode-comments-highlight-under-point))

(defun dank-mode-comments-navigate-to-parent (pos)
  "Move point to the parent of the current comment."
  (interactive "d")
  (let* ((node (ewoc-locate dank-mode-comments-current-ewoc pos))
         (comment (ewoc-data node))
         (parent-id (substring (dank-mode-comment-parent_id comment) 3))
         (parent-node (dank-mode-comment--ewoc-parent-node dank-mode-comments-current-ewoc node)))
    (ewoc-goto-node dank-mode-comments-current-ewoc (or parent-node node)))
  (beginning-of-line-text)
  (dank-mode-comments-highlight-under-point))

(defun dank-mode-comments-navigate-next-sibling (pos)
  "Move point to the beginning of the next sibling comment."
  (interactive "d")
  (let* ((node (ewoc-locate dank-mode-comments-current-ewoc pos))
         (comment (ewoc-data node))
         (comment-id (dank-mode-comment-id comment))
         (parent-id (dank-mode-comment-parent_id comment)))
    (ewoc-goto-node
     dank-mode-comments-current-ewoc
     (or (dank-mode-utils-ewoc-next-match-node dank-mode-comments-current-ewoc node
                                               (lambda (d)
                                                 (string-equal parent-id (dank-mode-comment-parent_id d))))
         node)))
  (dank-mode-comments-highlight-under-point))

(defun dank-mode-comments-navigate-prev-sibling (pos)
  "Move point to the beginning of the previous sibling comment."
  (interactive "d")
  (let* ((node (ewoc-locate dank-mode-comments-current-ewoc pos))
         (comment (ewoc-data node))
         (comment-id (dank-mode-comment-id comment))
         (parent-id (dank-mode-comment-parent_id comment)))
    (ewoc-goto-node
     dank-mode-comments-current-ewoc
     (or (dank-mode-utils-ewoc-next-match-node dank-mode-comments-current-ewoc node
                                               (lambda (d)
                                                 (string-equal parent-id (dank-mode-comment-parent_id d)))
                                               #'ewoc-prev)
         node)))
  (dank-mode-comments-highlight-under-point))

(defun dank-mode-comments-navigate-next-root (pos)
  "Move point to the beginning of the next root comment.
The next root comment is either the next sibling, or if there is
no next sibling, the next comment that has a lower depth.
If there is no next root, then navigate to the end of the comment."
  (interactive "d")
  (let* ((node (ewoc-locate dank-mode-comments-current-ewoc pos))
         (comment (ewoc-data node))
         (comment-id (dank-mode-comment-id comment)))
    (dank-mode-comments-navigate-next-sibling pos)  ;; try moving to the next sibling first

    (when (string-equal comment-id (dank-mode-comment-id (ewoc-data (ewoc-locate dank-mode-comments-current-ewoc (point)))))
      ;; if we're still at the same comment,
      ;; move back to parent then move to its next sibling
      (dank-mode-comments-navigate-to-parent pos)
      (dank-mode-comments-navigate-next-sibling (point)))

    (when (string-equal comment-id (dank-mode-comment-id (ewoc-data (ewoc-locate dank-mode-comments-current-ewoc (point)))))
      ;; if we're still at the same comment,
      ;; move to the end of the comment
      (dank-mode-comments--navigate-end-of-comment pos)))
  (dank-mode-comments-highlight-under-point))

(defun dank-mode-comments-collapse-comment-tree-at-point (pos)
  "Collapse the comment tree at POS."
  (interactive "d")
  ;; TODO: change the '-' to a '+'? or add ellipses at the end?
  ;; TODO: change style
  (let* ((exts (dank-mode-comments--find-comment-tree-extents pos t))
         (comment-id (dank-mode-comment-id (dank-mode-utils-ewoc-data dank-mode-comments-current-ewoc pos)))
         (comment-type (dank-mode-comment-type (dank-mode-utils-ewoc-data dank-mode-comments-current-ewoc pos)))
         (start (car exts))
         (end (cadr exts))
         (existing-ovl (cdr (assoc comment-id dank-mode-comments-tree-fold-overlays)))
         (ovl (if existing-ovl (move-overlay existing-ovl start end) (make-overlay start end))))
    (when (and comment-id (eq comment-type 'comment))
      (overlay-put ovl 'category 'dank-mode-comments-tree)
      (overlay-put ovl 'dank-mode-comments-tree-state 'collapsed)
      (overlay-put ovl 'dank-mode-comments-tree-id comment-id)
      (overlay-put ovl 'after-string "...")
      (overlay-put ovl 'invisible t)
      (add-to-list 'dank-mode-comments-tree-fold-overlays `(,comment-id . ,ovl))))
  (dank-mode-comments-highlight-under-point))

(defun dank-mode-comments-expand-comment-tree-at-point (pos)
  "Expand the collapsed comment tree at POS."
  (interactive "d")
  ;; point might be at the end of the overlay with no text properties or overlay
  (move-beginning-of-line 1)
  (move-beginning-of-line 1)
  (let* ((comment-id (dank-mode-comment-id (dank-mode-utils-ewoc-data dank-mode-comments-current-ewoc pos)))
         (comment-type (dank-mode-comment-type (dank-mode-utils-ewoc-data dank-mode-comments-current-ewoc pos)))
         (ovl (cdr (assoc comment-id dank-mode-comments-tree-fold-overlays))))
    (when (and comment-id ovl (eq comment-type 'comment))
      (overlay-put ovl 'after-string "")
      (overlay-put ovl 'dank-mode-comments-tree-state 'expanded)
      (overlay-put ovl 'invisible nil)
      (delete-overlay ovl))
    (dank-mode-comments-highlight-under-point)))

(defun dank-mode-comments-toggle-comment-tree-fold-at-point (pos)
  "Collapse or expand comment tree at POS."
  (interactive "d")
  (let* ((comment-id (dank-mode-comment-id (dank-mode-utils-ewoc-data dank-mode-comments-current-ewoc pos)))
         (existing-ovl (cdr (assoc comment-id dank-mode-comments-tree-fold-overlays))))
    (if (and existing-ovl (overlay-get existing-ovl 'invisible))
        (dank-mode-comments-expand-comment-tree-at-point pos)
      (dank-mode-comments-collapse-comment-tree-at-point pos))))

(defun dank-mode-comments-refresh ()
  "Refresh the comments of the current buffer."
  (interactive)
  (dank-mode-comments-reset-state dank-mode-comments-current-sorting)
  (dank-mode-comments-render-current-post-and-comments-ewoc dank-mode-comments-current-post dank-mode-comments-current-comments t))

(defun dank-mode-comments-change-sorting (sorting)
  "Refresh the comments of the current buffer with a different SORTING."
  (interactive (list (completing-read "Sorting: " dank-mode-comments-sorting-options)))
  (dank-mode-comments-reset-state (intern sorting))
  (dank-mode-comments-render-current-post-and-comments-ewoc dank-mode-comments-current-post dank-mode-comments-current-comments t))

(defun dank-mode-comments-kill-current-buffer ()
  "Kill the current dank-mode-comments buffer and switch back to the source buffer."
  (interactive)
  (let ((current-buffer (current-buffer)))
    (when dank-mode-comments-current-source-buffer
      (switch-to-buffer dank-mode-comments-current-source-buffer))
    (kill-buffer current-buffer)))

(defun dank-mode-comments-browse-post-link (&optional eww)
  "Open the current post link in a browser.
If EWW is non-nil, browse in eww instead of the browser."
  (interactive "P")
  (let* ((post-link (dank-mode-post-link dank-mode-comments-current-post))
         (browse-url-browser-function (if eww 'eww-browse-url 'browse-url-default-browser)))
    (browse-url post-link)))

(defun dank-mode-comments-browse-post-comments (&optional eww)
  "Open the current comments in a browser.
If EWW is non-nil, browse in eww instead of the browser."
  (interactive "P")
  (let ((browse-url-browser-function (if eww 'eww-browse-url 'browse-url-default-browser)))
    (browse-url (concat (dank-mode-utils-reddit-url) dank-mode-comments-current-permalink))))

(provide 'dank-mode-comments)
;;; dank-mode-comments.el ends here
