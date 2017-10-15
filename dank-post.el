(require 'dank-utils)
(require 'dank-faces)

(cl-defstruct dank-post
  name id title link permalink text age date author subreddit score num_comments
  domain post_type nsfw spoiler link_flair author_flair
  gilded stickied locked)

;; Rendering

(defvar dank-post-template
  "${title}\n    | submitted ${age} by ${author}${author_flair} to ${subreddit}\n    | ${score} points | ${num_comments} comments | ${link_flair}${nsfw}${spoiler}${post_type}${domain}")

(defun dank-post-render (post)
  "Render POST as string using `dank-post-template'."
  (let* ((title (propertize (dank-post-title post) 'font-lock-face 'dank-faces-post-title))
         (age (propertize (dank-post-age post) 'font-lock-face 'dank-faces-age))
         (author (propertize (dank-post-author post) 'font-lock-face 'dank-faces-author))
         (author_flair (if (and (dank-post-author_flair post) (not (eq (dank-post-author_flair post) "")))
                           (propertize (concat " [" (dank-post-author_flair post) "]") 'font-lock-face 'dank-faces-flair)
                         ""))
         (subreddit (propertize (concat "/r/" (dank-post-subreddit post)) 'font-lock-face 'dank-faces-subreddit))
         (score (propertize (format "%s" (dank-post-score post)) 'font-lock-face 'dank-faces-upvote))
         (num_comments (propertize (format "%s" (dank-post-num_comments post)) 'font-lock-face 'dank-faces-downvote))
         (nsfw (if (dank-post-nsfw post) (propertize "NSFW " 'font-lock-face 'dank-faces-nsfw) ""))
         (spoiler (if (dank-post-spoiler post) (propertize "SPOILERS " 'font-lock-face 'dank-faces-nsfw) ""))
         (post_type (propertize (if (string= (dank-post-post_type post) "self")
                                    "self-text" (dank-post-post_type post))
                                'font-lock-face 'dank-faces-post-type))
         (domain (if (string= post_type "self-text") "" (concat " from " (propertize (dank-post-domain post)
                                                                                     'font-lock-face
                                                                                     'dank-faces-site-domain))))
         (link_flair (if (dank-post-link_flair post)
                         (propertize (concat "[" (dank-post-link_flair post) "] ") 'font-lock-face 'dank-faces-flair) ""))
         (format-context `(title ,title age ,age author ,author author_flair ,author_flair
                                 subreddit ,subreddit score ,score num_comments ,num_comments
                                 nsfw ,nsfw spoiler ,spoiler domain ,domain post_type ,post_type
                                 link_flair ,link_flair))
         (rendered-post (dank-utils-format-plist dank-post-template format-context)))
    rendered-post))

(defun dank-post-propertize (rendered-post source-post &optional pos)
  "Assign RENDERED-POST text properties from SOURCE-POST.
Optional POS is the position of the post in the list."
  (add-text-properties 0 (length rendered-post)
                       `(dank-post-id ,(dank-post-id source-post)
                                      dank-post-subreddit ,(dank-post-subreddit source-post)
                                      dank-post-pos ,pos
                                      dank-post-permalink ,(dank-post-permalink source-post)
                                      dank-post-title ,(dank-post-title source-post))
                       rendered-post))

(defun dank-post-parse (post)
  "Parse POST into a `dank-post'."
  (let* ((post (plist-get post :data)))
    (make-dank-post :id (plist-get post :id)
                    :name (plist-get post :name)
                    :title (s-trim (plist-get post :title))
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

(provide 'dank-post)
