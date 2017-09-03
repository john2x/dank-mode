(require 'dank-backend)

(cl-defstruct dank-comment
  name id body edited text age date author subreddit score
  author_flair gilded replies)

(cl-defstruct dank-comment-more
  parent_id count)

(defun dank-comment-parse (comment)
  "Parse COMMENT into a `dank-comment'."
  (let* ((kind (plist-get comment :kind))
         (comment (plist-get comment :data))
         (replies (plist-get comment :replies)))
    (if (string= kind "more")
        (make-dank-comment-more :parent_id (plist-get comment :parent_id)
                                :count (plist-get comment :count))
      (make-dank-comment :id (plist-get comment :id)
                         :name (plist-get comment :name)
                         :body (s-trim (or (plist-get comment :body) ""))
                         :age (dank-utils-timestamp-ago (plist-get comment :created_utc))
                         :date (plist-get comment :created_utc)
                         :score (plist-get comment :score)
                         :author (plist-get comment :author)
                         :subreddit (plist-get comment :subreddit)
                         :author_flair (plist-get comment :author_flair_text)
                         :gilded (plist-get comment :gilded)
                         :replies (if (stringp replies) '()
                                    (mapcar 'dank-comment-parse (plist-get (plist-get replies :data) :children)))))))

(provide 'dank-comment)
