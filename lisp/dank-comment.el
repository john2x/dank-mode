(require 'dank-backend)
(require 'dank-utils)
(require 'dash)

(cl-defstruct dank-comment
  name id body edited text age date author subreddit score
  author_flair gilded replies depth)

(cl-defstruct dank-comment-load-more-placeholder
  parent_id count)

(defvar dank-comment-metadata-template
  "${indent}[/u/${author} (${score} points | ${age})${edited} ${gilded} depth: ${depth}]")

(defvar dank-comment-body-template
  "${indent}${body}")

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
        (make-dank-comment-more :parent_id (plist-get comment :parent_id)
                                :count (plist-get comment :count))
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
                         :replies (if (stringp replies) '()
                                    (mapcar replies-map-fn children-depth))))))

(defun dank-comment-format-metadata (comment)
  "Format COMMENT metadata.
The comment body will need to be formatted separately, since it's
formatting/indentation will depend on its position."
  (message "depth: %s" (dank-comment-depth comment))
  (let* ((author (dank-comment-author comment))
         (score (dank-comment-score comment))
         (age (dank-comment-age comment))
         (edited (or nil ""))
         (gilded (dank-comment-gilded comment))
         (indent (or (-repeat (dank-comment-depth comment) "  ") ""))
         (depth (dank-comment-depth comment))
         (format-context `(author ,author age ,age score ,score edited ,edited gilded ,gilded indent ,indent depth ,depth)))
    (message "%s" format-context)
    (dank-utils-format-plist dank-comment-metadata-template format-context)))

(defun dank-comment-format-body (comment fill-column)
  "Format COMMENT body.
The comment body will need to be formatted separately, since it's
formatting/indentation will depend on its position."
  (let* ((body (dank-comment-body comment))
         (depth (dank-comment-depth comment))
         (filled-body (dank-utils-markdown-fill-paragraph-and-indent body depth fill-column)) ;; fill the body
         )
    ;(dank-utils-format-plist dank-comment-body-template format-context)
    filled-body))

(defun dank-comment-propertize (rendered-comment source-comment &optional pos)
  (add-text-properties 0 (length rendered-comment)
                       `(dank-comment-id ,(dank-comment-id source-comment)
                                         dank-comment-post ,pos)
                       rendered-comment))


(provide 'dank-comment)
