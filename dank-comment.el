(require 'dank-backend)
(require 'dank-utils)
(require 'dash)

(cl-defstruct dank-comment
  name id body edited text age date author subreddit score
  author_flair gilded replies depth)

(cl-defstruct dank-comment-more
  parent_id count)

(defvar dank-comment-header-template
  "${author} (${score} | ${age})${edited}${gilded}")

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

(defun dank-comment-render-header (comment)
  "Render COMMENT header.
The comment body will need to be rendered separately, since it's
formatting/indentation will depend on its position."
  (let* ((author (dank-comment-author comment))
         (score (dank-comment-score comment))
         (age (dank-comment-age comment))
         (edited nil)
         (gilded (dank-comment-gilded comment))
         (format-context `(author ,author age ,age score ,score edited ,edited gilded ,gilded)))
    (dank-utils-format-plist dank-comment-header-template format-context)))

(defun dank-comment-propertize (rendered-comment source-comment &optional pos)
  (add-text-properties 0 (length rendered-comment)
                       `(dank-comment-id ,(dank-comment-id source-comment)
                                         dank-comment-post ,pos)
                       rendered-comment))


(provide 'dank-comment)
