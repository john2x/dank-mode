;;; dank-comment.el --- Major mode for browsing Reddit

;; Copyright (C) 2021 John Louis Del Rosario

;; Author: John Louis Del Rosario <john2x@gmail.com>
;; Version: 0.1.0
;; Keywords: reddit, social

;;; Commentary:

;; This file defines structs and utility functions for the
;; dank-comments major mode.

;;; Code:

(require 'dank-backend)
(require 'dank-utils)
(require 'dank-post)

(cl-defstruct dank-comment
  name id body edited text age date author subreddit score
  author_flair gilded replies depth parent_id post_author_p
  type more_count children_ids)

(defcustom dank-comments-body-fill-width 120
  "Fill width for rendering the comment body."
  :type 'integer
  :group 'dank-mode)

(defvar dank-comment-metadata-template
  "- ${author} (${score} points | ${age})${edited} ")

(defvar dank-comment-more-template
  "+ [${count} more comments]")

(defvar dank-comment-continue-template
  "+ [Continue thread in new buffer...]")

(defvar dank-comments-header-line-format-template
  "/r/${subreddit} - ${sorting} ${starting-comment-id}")

(defun dank-comment-parse (comment post-author)
  "Parse COMMENT into a `dank-comment'.
POST-AUTHOR is used to set a boolean field to differentiate the
comment author as the post author.

If COMMENT has replies, return a cons cell where the cdr is the
list of children, also parsed."
  (let* ((kind (plist-get comment :kind))
         (comment (plist-get comment :data))
         (depth (plist-get comment :depth))
         (replies (plist-get comment :replies))
         (children (plist-get (plist-get replies :data) :children)))
    (if (string= kind "more")
        (make-dank-comment :id (plist-get comment :id)
                           :type 'more
                           :parent_id (plist-get comment :parent_id)
                           :more_count (plist-get comment :count)
                           :depth depth
                           :children_ids (plist-get comment :children))
      (let ((parsed-comment (make-dank-comment
                             :type 'comment
                             :id (plist-get comment :id)
                             :name (plist-get comment :name)
                             :depth depth
                             :body (s-trim (or (plist-get comment :body) ""))
                             :age (dank-utils-timestamp-ago (plist-get comment :created_utc))
                             :date (plist-get comment :created_utc)
                             :score (plist-get comment :score)
                             :author (plist-get comment :author)
                             :subreddit (plist-get comment :subreddit)
                             :author_flair (plist-get comment :author_flair_text)
                             :gilded (plist-get comment :gilded)
                             :parent_id (plist-get comment :parent_id)
                             :post_author_p (string-equal (plist-get comment :author) post-author))))
        (if (stringp replies)
            parsed-comment
          `(,parsed-comment . (,(mapcar (lambda (c) (dank-comment-parse c post-author)) children))))))))

(defun dank-comment-format-metadata (comment)
  "Format COMMENT metadata.
Also applies font-lock properties.
The comment body will need to be formatted separately, since it's
formatting/indentation will depend on its position."
  (let* ((author-face (if (dank-comment-post_author_p comment) 'dank-faces-comment-author-op 'dank-faces-comment-author))
         (author (concat "/u/" (dank-comment-author comment)))
         (score (number-to-string (dank-comment-score comment)))
         (age (dank-comment-age comment))
         (edited (or nil ""))
         (gilded (number-to-string (dank-comment-gilded comment)))
         (depth (dank-comment-depth comment))
         (format-context `(author (,author . ,author-face) age ,age score ,score edited ,edited gilded ,gilded depth ,depth))
         (formatted-metadata (dank-utils-format-plist dank-comment-metadata-template format-context 'dank-faces-comment-metadata))
         (formatted-metadata (concat (s-repeat depth "  ") formatted-metadata)))
    (dank-comment--propertize-comment-with-metadata formatted-metadata comment)))

(defun dank-comment-format-body (comment)
  "Format COMMENT body.
The body is filled up to `dank-comments-body-fill-width'."
  (let* ((body (dank-comment-body comment))
         (depth (dank-comment-depth comment))
         (filled-body (dank-utils-markdown-fill-paragraph-and-indent body depth dank-comments-body-fill-width)) ;; fill the body
         )
    (dank-comment--propertize-comment-with-metadata filled-body comment)))

(defun dank-comment-format-more (more)
  "Format MORE."
  (let* ((count (dank-comment-more_count more))
         (format-context `(count ,(number-to-string count)))
         (template (if (> count 0) dank-comment-more-template dank-comment-continue-template))
         (formatted (dank-utils-format-plist template format-context 'dank-faces-comment-more))
         (formatted (concat (s-repeat (dank-comment-depth more) "  ") formatted)))
    (dank-comment--propertize-more-with-metadata formatted more)))

(defun dank-comment--propertize-comment-with-metadata (formatted-comment source-comment)
  "Assign FORMATTED-COMMENT with metadata from SOURCE-COMMENT."
  (add-text-properties 0 (length formatted-comment)
                       `(dank-comment-id ,(dank-comment-id source-comment)
                                         dank-comment-type comment
                                         dank-comment-parent-id ,(dank-comment-parent_id source-comment)
                                         dank-comment-depth ,(dank-comment-depth source-comment))
                       formatted-comment)
  formatted-comment)

(defun dank-comment--propertize-more-with-metadata (formatted-placeholder source-placeholder)
  "Assign FORMATTED-PLACEHOLDER with metadata from SOURCE-PLACEHOLDER."
  (add-text-properties 0 (length formatted-placeholder)
                       `(dank-comment-parent-id ,(dank-comment-parent_id source-placeholder)
                                                dank-comment-type more
                                                dank-comment-id ,(dank-comment-id source-placeholder)
                                                dank-comment-children-ids ,(dank-comment-children_ids source-placeholder)
                                                dank-comment-count ,(dank-comment-more_count source-placeholder)
                                                dank-comment-depth ,(dank-comment-depth source-placeholder))
                       formatted-placeholder)
  formatted-placeholder)

(defun dank-comment--ewoc-pp (post-or-comment)
  "EWOC pretty-printer for POST-OR-COMMENT.
Only one ewoc can be active in a buffer, so the comments EWOC
needs to be able to handle different objects.
Optional POST-PP must be the post pretty-printer function."
  (cl-typecase post-or-comment
    (dank-comment (insert (if (eq (dank-comment-type post-or-comment) 'comment)
                              (concat (dank-comment-format-metadata post-or-comment) "\n"
                                      (dank-comment-format-body post-or-comment))
                            (dank-comment-format-more post-or-comment))))
    (dank-post (insert (concat (dank-post-format post-or-comment) "\n"
                               (dank-post-format-content post-or-comment))))))

(defun dank-comment--ewoc-parent-node (ewoc node)
  "Return the parent node of NODE in EWOC."
  (let* ((comment (ewoc-data node))
         (parent-id (substring (dank-comment-parent-id comment) 3)))
    (dank-utils-ewoc-next-match-node ewoc node
      (lambda (d)
        (when (dank-comment-p d)
          (string-equal parent-id (dank-comment-id d))))
      #'ewoc-prev)))

(provide 'dank-comment)

;;; dank-comment.el ends here
