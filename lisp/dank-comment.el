(require 'dank-backend)
(require 'dank-utils)
(require 'dash)

(cl-defstruct dank-comment
  name id body edited text age date author subreddit score
  author_flair gilded replies depth parent_id)

(cl-defstruct dank-comment-load-more-placeholder
  parent_id count depth)

(defvar dank-comment-metadata-template
  "${indent}- ${author} (${score} points | ${age})${edited}")

(defvar dank-comment-body-template
  "${indent}${body}")

(defvar dank-comment-load-more-placeholder-template
  "[More comments]")

(defvar dank-comments-header-line-format-template
  "/r/${subreddit} - ${sorting}")

(defun dank-comment-parse (comment &optional depth)
  "Parse COMMENT into a `dank-comment'."
  (let* ((depth (or depth 0))
         (kind (plist-get comment :kind))
         (comment (plist-get comment :data))
         (replies (plist-get comment :replies))
         (children (plist-get (plist-get replies :data) :children))
         (children-depth (mapcar* #'list children (-repeat (length children) (+ depth 1))))
         (replies-map-fn (lambda (child-depth)
                           (dank-comment-parse (car child-depth) (cadr child-depth)))))
    (if (string= kind "more")
        (make-dank-comment-load-more-placeholder :parent_id (plist-get comment :parent_id)
                                                 :count (plist-get comment :count)
                                                 :depth depth)
      (make-dank-comment :id (plist-get comment :id)
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
                         :replies (if (stringp replies) '()
                                    (mapcar replies-map-fn children-depth))))))

(defun dank-comment-format-post-content (post fill-column)
  "Format POST content as string."
  (concat
   (propertize (concat (s-repeat fill-column " ") "\n") 'font-lock-face 'dank-faces-separator)
   (if (string= (dank-post-post_type post) "self")
       (dank-utils-markdown-fill-paragraph-and-indent (dank-post-text post) 0 fill-column "")
     (dank-post-link post)) ;; TODO: propertize link
   "\n"
   (propertize (concat (s-repeat fill-column " ") "\n") 'font-lock-face 'dank-faces-separator)))

(defun dank-comment-format-metadata (comment post-author)
  "Format COMMENT metadata.
Also applies font-lock properties.
The comment body will need to be formatted separately, since it's
formatting/indentation will depend on its position.
POST-AUTHOR is used to apply a different face to the comment author."
  (let* ((author-face (if (string= (dank-comment-author comment) post-author) 'dank-faces-comment-author-op 'dank-faces-comment-author))
         (author (propertize (concat "/u/" (dank-comment-author comment)) 'font-lock-face author-face))
         (score (propertize (number-to-string (dank-comment-score comment)) 'font-lock-face 'dank-faces-comment-metadata))
         (age (propertize (dank-comment-age comment) 'font-lock-face 'dank-faces-comment-metadata))
         (edited (or nil ""))
         (gilded (number-to-string (dank-comment-gilded comment)))
         (indent (or (s-repeat (dank-comment-depth comment) "  ") ""))
         (depth (dank-comment-depth comment))
         (format-context `(author ,author age ,age score ,score edited ,edited gilded ,gilded indent ,indent depth ,depth))
         (formatted-metadata (dank-utils-format-plist (propertize dank-comment-metadata-template 'font-lock-face 'dank-faces-comment-metadata) format-context)))
    (dank-comment--propertize-comment-with-metadata formatted-metadata comment)))

(defun dank-comment-format-body (comment fill-column)
  "Format COMMENT body.
The comment body will need to be formatted separately, since it's
formatting/indentation will depend on its position."
  (let* ((body (dank-comment-body comment))
         (depth (dank-comment-depth comment))
         (filled-body (dank-utils-markdown-fill-paragraph-and-indent body depth fill-column)) ;; fill the body
         )
    ;(dank-utils-format-plist dank-comment-body-template format-context)
    (dank-comment--propertize-comment-with-metadata filled-body comment)))

(defun dank-comment-format-load-more-placeholder (load-more-placeholder)
  "Format LOAD-MORE-PLACEHOLDER."
  (dank-comment--propertize-load-more-placeholder-with-metadata
   (concat (s-repeat (dank-comment-load-more-placeholder-depth load-more-placeholder) "  ") "|> " dank-comment-load-more-placeholder-template)
   load-more-placeholder))

(defun dank-comment--propertize-comment-with-metadata (formatted-comment source-comment)
  "Assign FORMATTED-COMMENT with metadata from SOURCE-COMMENT."
  (add-text-properties 0 (length formatted-comment)
                       `(dank-comment-id ,(dank-comment-id source-comment)
                                         dank-comment-parent-id ,(dank-comment-parent_id source-comment)
                                         dank-comment-depth ,(dank-comment-depth source-comment))
                       formatted-comment)
  formatted-comment)

(defun dank-comment--propertize-load-more-placeholder-with-metadata (formatted-placeholder source-placeholder)
  "Assign FORMATTED-PLACEHOLDER with metadata from SOURCE-PLACEHOLDER."
  (add-text-properties 0 (length formatted-placeholder)
                       `(dank-comment-parent-id ,(dank-comment-load-more-placeholder-parent_id source-placeholder)
                                                dank-comment-depth ,(dank-comment-load-more-placeholder-depth source-placeholder))
                       formatted-placeholder)
  formatted-placeholder)

(provide 'dank-comment)
