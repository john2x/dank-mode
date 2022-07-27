;;; dank-mode-comment.el --- Major mode for browsing Reddit -*- lexical-binding: t -*-

;; Copyright (C) 2021 John Louis Del Rosario

;; Author: John Louis Del Rosario <john2x@gmail.com>
;; Version: 0.2.0
;; Keywords: reddit, social

;;; Commentary:

;; This file defines structs and utility functions for the
;; dank-mode-comments major mode.

;;; Code:

(require 'dank-mode-backend)
(require 'dank-mode-utils)
(require 'dank-mode-post)
(require 'dank-mode-faces)

(defcustom dank-mode-comment-upvote-symbol "🡅"
  "Symbol to use for upvotes. Change this if the emoji doesn't work correctly."
  :type 'string
  :group 'dank-mode)

(defcustom dank-mode-comment-downvote-symbol "🡇"
  "Symbol to use for downvotes. Change this if the emoji doesn't work correctly."
  :type 'string
  :group 'dank-mode)

(cl-defstruct dank-mode-comment
  name id body edited text age date author subreddit score
  author_flair gilded replies depth parent_id post_author_p
  type more_count children_ids likes)

(defcustom dank-mode-comments-body-fill-width 120
  "Fill width for rendering the comment body."
  :type 'integer
  :group 'dank-mode)

(defvar dank-mode-comment-metadata-template
  "- ${author} (${vote}${score} points | ${age})${edited} ")

(defvar dank-mode-comment-more-template
  "+ [${count} more comments]")

(defvar dank-mode-comment-continue-template
  "+ [Continue thread in new buffer...]")

(defvar dank-mode-comments-header-line-format-template
  "/r/${subreddit} - ${sorting} ${starting-comment-id}")

(defun dank-mode-comment-parse (comment post-author)
  "Parse COMMENT into a `dank-mode-comment'.
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
        (make-dank-mode-comment :id (plist-get comment :id)
                                :type 'more
                                :parent_id (plist-get comment :parent_id)
                                :more_count (plist-get comment :count)
                                :depth depth
                                :children_ids (plist-get comment :children))
      (let ((parsed-comment (make-dank-mode-comment
                             :type 'comment
                             :id (plist-get comment :id)
                             :name (plist-get comment :name)
                             :depth depth
                             :body (string-trim (or (plist-get comment :body) ""))
                             :age (dank-mode-utils-timestamp-ago (plist-get comment :created_utc))
                             :date (plist-get comment :created_utc)
                             :score (plist-get comment :score)
                             :author (plist-get comment :author)
                             :subreddit (plist-get comment :subreddit)
                             :author_flair (plist-get comment :author_flair_text)
                             :gilded (plist-get comment :gilded)
                             :parent_id (plist-get comment :parent_id)
                             :post_author_p (string-equal (plist-get comment :author) post-author)
                             :likes (plist-get comment :likes))))
        (if (stringp replies)
            parsed-comment
          `(,parsed-comment . (,(mapcar (lambda (c) (dank-mode-comment-parse c post-author)) children))))))))

(defun dank-mode-comment-format-metadata (comment)
  "Format COMMENT metadata.
Also applies font-lock properties.
The comment body will need to be formatted separately, since it's
formatting/indentation will depend on its position."
  (let* ((author-face (if (dank-mode-comment-post_author_p comment) 'dank-mode-faces-comment-author-op 'dank-mode-faces-comment-author))
         (author (concat "/u/" (dank-mode-comment-author comment)))
         (score (number-to-string (dank-mode-comment-score comment)))
         (likes (dank-mode-comment-likes comment))
         (vote (format "%s" (cond ((eq :false likes) dank-mode-comment-downvote-symbol)
                                  (likes dank-mode-comment-upvote-symbol)  ; likes is t if upvoted
                                  (t ""))))
         (vote-face (format "%s" (cond ((eq :false likes) 'dank-mode-faces-downvote)
                                       (likes 'dank-mode-faces-upvote)  ; likes is t if upvoted
                                       (t 'dank-mode-faces-upvote))))
         (age (dank-mode-comment-age comment))
         (edited (or nil ""))
         (gilded (number-to-string (dank-mode-comment-gilded comment)))
         (depth (dank-mode-comment-depth comment))
         (format-context `(author (,author . ,author-face) age ,age vote (,vote . ,vote-face) score ,score edited ,edited gilded ,gilded depth ,depth))
         (formatted-metadata (dank-mode-utils-format-plist dank-mode-comment-metadata-template format-context 'dank-mode-faces-comment-metadata))
         (formatted-metadata (concat (make-string (* 2 depth) ?\s) formatted-metadata)))
    formatted-metadata))

(defun dank-mode-comment-format-body (comment)
  "Format COMMENT body.
The body is filled up to `dank-mode-comments-body-fill-width'."
  (let* ((body (dank-mode-comment-body comment))
         (depth (dank-mode-comment-depth comment))
         (filled-body (dank-mode-utils-markdown-fill-paragraph-and-indent body depth dank-mode-comments-body-fill-width)) ;; fill the body
         )
    filled-body))

(defun dank-mode-comment-format-more (more)
  "Format MORE."
  (let* ((count (dank-mode-comment-more_count more))
         (format-context `(count ,(number-to-string count)))
         (template (if (> count 0) dank-mode-comment-more-template dank-mode-comment-continue-template))
         (formatted (dank-mode-utils-format-plist template format-context 'dank-mode-faces-comment-more))
         (formatted (concat (make-string (* 2 (dank-mode-comment-depth more)) ?\s) formatted)))
    formatted))

(defun dank-mode-comment--ewoc-pp (post-or-comment)
  "EWOC pretty-printer for POST-OR-COMMENT.
Only one ewoc can be active in a buffer, so the comments EWOC
needs to be able to handle different objects.
Optional POST-PP must be the post pretty-printer function."
  (cl-typecase post-or-comment
    (dank-mode-comment (insert (if (eq (dank-mode-comment-type post-or-comment) 'comment)
                                   (concat (dank-mode-comment-format-metadata post-or-comment) "\n"
                                           (dank-mode-comment-format-body post-or-comment))
                                 (dank-mode-comment-format-more post-or-comment))))
    (dank-mode-post (insert (concat (dank-mode-post-format post-or-comment) "\n"
                                    (dank-mode-post-format-content post-or-comment))))))

(defun dank-mode-comment--ewoc-parent-node (ewoc node)
  "Return the parent node of NODE in EWOC."
  (let* ((comment (ewoc-data node))
         (parent-id (substring (dank-mode-comment-parent_id comment) 3)))
    (dank-mode-utils-ewoc-next-match-node ewoc node
                                          (lambda (d)
                                            (when (dank-mode-comment-p d)
                                              (string-equal parent-id (dank-mode-comment-id d))))
                                          #'ewoc-prev)))

(provide 'dank-mode-comment)

;;; dank-mode-comment.el ends here
