(require 'dank-backend)
(require 'dank-utils)
(require 'dash)

(cl-defstruct dank-comment
  name id body edited text age date author subreddit score
  author_flair gilded replies depth parent_id)

(cl-defstruct dank-comment-more
  id parent_id count depth children_ids)

(defvar dank-comment-metadata-template
  "- ${author} (${score} points | ${age})${edited} ")

(defvar dank-comment-more-template
  "+ [${count} more comments]")

(defvar dank-comment-continue-template
  "+ [Continue thread in new buffer...]")

(defvar dank-comments-header-line-format-template
  "/r/${subreddit} - ${sorting}")

(defun dank-comment-parse (comment)
  "Parse COMMENT into a `dank-comment'."
  (let* ((kind (plist-get comment :kind))
         (comment (plist-get comment :data))
         (depth (plist-get comment :depth))
         (replies (plist-get comment :replies))
         (children (plist-get (plist-get replies :data) :children)))
    (if (string= kind "more")
        (make-dank-comment-more :id (plist-get comment :id)
                                :parent_id (plist-get comment :parent_id)
                                :count (plist-get comment :count)
                                :depth depth
                                :children_ids (plist-get comment :children))
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
                                    (mapcar #'dank-comment-parse children))))))

(defun dank-comment-format-post-content (post fill-column)
  "Format POST content as string."
  (concat
   "\n"
   ;(propertize (concat (s-repeat fill-column " ") "\n") 'font-lock-face 'dank-faces-separator)
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

(defun dank-comment-format-body (comment fill-column)
  "Format COMMENT body."
  (let* ((body (dank-comment-body comment))
         (depth (dank-comment-depth comment))
         (filled-body (dank-utils-markdown-fill-paragraph-and-indent body depth fill-column)) ;; fill the body
         )
    (dank-comment--propertize-comment-with-metadata filled-body comment)))

(defun dank-comment-format-more (more)
  "Format MORE."
  (let* ((count (dank-comment-more-count more))
         (format-context `(count ,(number-to-string count)))
         (template (if (> count 0) dank-comment-more-template dank-comment-continue-template))
         (formatted (dank-utils-format-plist template format-context 'dank-faces-comment-more))
         (formatted (concat (s-repeat (dank-comment-more-depth more) "  ") formatted)))
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
                       `(dank-comment-parent-id ,(dank-comment-more-parent_id source-placeholder)
                                                dank-comment-type more
                                                dank-comment-id ,(dank-comment-more-id source-placeholder)
                                                dank-comment-children-ids ,(dank-comment-more-children_ids source-placeholder)
                                                dank-comment-count ,(dank-comment-more-count source-placeholder)
                                                dank-comment-depth ,(dank-comment-more-depth source-placeholder))
                       formatted-placeholder)
  formatted-placeholder)

(provide 'dank-comment)
