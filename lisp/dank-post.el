(require 'dank-utils)
(require 'dank-faces)

(cl-defstruct dank-post
  "Struct for a Reddit post."
  name id title link permalink text age date author subreddit score num_comments
  domain post_type nsfw spoiler link_flair author_flair
  gilded stickied locked)

(cl-defstruct dank-subreddit
  "Struct for a subreddit."
  id title description url)

;; Rendering

(defvar dank-post-template
  "${title}\n    | ${score} points | ${num_comments} comments | ${link_flair}${nsfw}${spoiler}${post_type}${domain}\n    | ${subreddit} submitted by ${author}${author_flair} ${age}")

(defun dank-post-format (post &optional post-index)
  "Format POST as string using `dank-post-template'.
Also applies font-lock properties.
Optional POST-INDEX is the position of the post in a list."
  (let* ((title (dank-post-title post))
         (age (dank-post-age post))
         (author (concat "/u/" (dank-post-author post)))
         (author_flair (if (and (dank-post-author_flair post) (not (eq (dank-post-author_flair post) "")))
                           (concat " [" (dank-post-author_flair post) "]")
                         ""))
         (subreddit (concat "/r/" (dank-post-subreddit post)))
         (score (format "%s" (dank-post-score post)))
         (num_comments (format "%s" (dank-post-num_comments post)))
         (nsfw (if (dank-post-nsfw post) "NSFW " ""))
         (spoiler (if (dank-post-spoiler post) "SPOILERS " ""))
         (post_type (if (string= (dank-post-post_type post) "self")
                                    "self-text" (dank-post-post_type post)))
         (domain (if (string= post_type "self-text") "" (concat " from " (dank-post-domain post))))
         (link_flair (if (dank-post-link_flair post)
                         (concat "[" (dank-post-link_flair post) "] ") ""))
         (format-context `(title (,title . dank-faces-post-title) age (,age . dank-faces-age) author (,author . dank-faces-post-author) author_flair (,author_flair . dank-faces-flair)
                                 subreddit (,subreddit . dank-faces-subreddit) score (,score . dank-faces-upvote) num_comments (,num_comments . dank-faces-downvote)
                                 nsfw (,nsfw . dank-faces-nsfw) spoiler (,spoiler . dank-faces-nsfw) domain (,domain . dank-faces-site-domain) post_type (,post_type . dank-faces-post-type)
                                 link_flair (,link_flair . dank-faces-flair)))
         (formatted-post (dank-utils-format-plist dank-post-template format-context)))
    (dank-post--propertize-metadata formatted-post post post-index)))

(defun dank-post--propertize-metadata (formatted-post source-post &optional post-index)
  "Assign FORMATTED-POST metadata properties from SOURCE-POST.
Optional POST-INDEX is the position of the post in a list."
  (add-text-properties 0 (length formatted-post)
                       `(dank-post-id ,(dank-post-id source-post)
                                      dank-post-subreddit ,(dank-post-subreddit source-post)
                                      dank-post-index ,post-index
                                      dank-post-permalink ,(dank-post-permalink source-post)
                                      dank-post-title ,(dank-post-title source-post)
                                      dank-post-link ,(dank-post-link source-post))
                       formatted-post)
  formatted-post)

(defun dank-post-parse (post)
  "Parse POST into a `dank-post'."
  (let* ((post (plist-get post :data)))
    (make-dank-post :id (plist-get post :id)
                    :name (plist-get post :name)
                    :title (dank-utils-escape-html (s-trim (plist-get post :title)))
                    :link (plist-get post :url)
                    :text (plist-get post :selftext)
                    :age (dank-utils-timestamp-ago (plist-get post :created_utc))
                    :date (plist-get post :created_utc)
                    :score (plist-get post :score)
                    :author (plist-get post :author)
                    :subreddit (plist-get post :subreddit)
                    :num_comments (plist-get post :num_comments)
                    :domain (plist-get post :domain)
                    :post_type (or (plist-get post :post_hint) (if (eq (plist-get post :is_self) t) "self" "link"))
                    :nsfw (not (eq (plist-get post :over_18) :json-false))
                    :spoiler (not (eq (plist-get post :spoiler) :json-false))
                    :link_flair (plist-get post :link_flair_text)
                    :author_flair (plist-get post :author_flair_text)
                    :gilded (plist-get post :gilded)
                    :stickied (not (eq (plist-get post :stickied) :json-false))
                    :locked (not (eq (plist-get post :locked) :json-false))
                    :permalink (plist-get post :permalink))))

(defun dank-post-subreddit-parse (subreddit)
  "Parse SUBREDDIT into a `dank-subreddit'."
  (let ((subreddit (plist-get subreddit :data)))
    (make-dank-subreddit :id (plist-get subreddit :name)
                         :title (plist-get subreddit :title)
                         :url (plist-get subreddit :url)
                         :description (plist-get subreddit :description))))

(provide 'dank-post)
