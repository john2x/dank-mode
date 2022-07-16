;;; dank-post.el --- Major mode for browsing Reddit

;; Copyright (C) 2021 John Louis Del Rosario

;; Author: John Louis Del Rosario <john2x@gmail.com>
;; Version: 0.2.0
;; Keywords: reddit, social

;;; Commentary:

;; This file defines structs and utility functions for the dank-posts
;; major mode.

;;; Code:

(require 'dank-utils)
(require 'dank-faces)

(defcustom dank-post-upvote-symbol "🠽"
  "Symbol to use for upvotes. Change this if the emoji doesn't work correctly."
  :type 'string
  :group 'dank-mode)

(defcustom dank-post-downvote-symbol "🠿"
  "Symbol to use for downvotes. Change this if the emoji doesn't work correctly."
  :type 'string
  :group 'dank-mode)

(cl-defstruct dank-post
  "Struct for a Reddit post."
  name id title link permalink text age date author subreddit score num_comments
  domain post_type nsfw spoiler link_flair author_flair
  gilded stickied locked likes)

(cl-defstruct dank-subreddit
  "Struct for a subreddit."
  id title description url)

;; Rendering

(defvar dank-post-template
  "${title}\n    | ${vote}${score} points | ${num_comments} comments | ${link_flair}${nsfw}${spoiler}${post_type}${domain}\n    | ${subreddit} submitted by ${author}${author_flair} ${age}")

(defun dank-post-format (post)
  "Format POST as string using `dank-post-template'.
Also applies font-lock properties."
  (let* ((title (dank-post-title post))
         (age (dank-post-age post))
         (author (concat "/u/" (dank-post-author post)))
         (author_flair (if (and (dank-post-author_flair post) (not (eq (dank-post-author_flair post) "")))
                           (concat " [" (dank-post-author_flair post) "]")
                         ""))
         (subreddit (concat "/r/" (dank-post-subreddit post)))
         (score (format "%s" (dank-post-score post)))
         (likes (dank-post-likes post))
         (vote (format "%s" (cond ((eq :false likes) dank-post-downvote-symbol)
                                  (likes dank-post-upvote-symbol)  ; likes is t if upvoted
                                  (t ""))))
         (vote-face (format "%s" (cond ((eq :false likes) 'dank-faces-downvote)
                                       (likes 'dank-faces-upvote)  ; likes is t if upvoted
                                       (t 'dank-faces-upvote))))
         (num_comments (format "%s" (dank-post-num_comments post)))
         (nsfw (if (dank-post-nsfw post) "NSFW " ""))
         (spoiler (if (dank-post-spoiler post) "SPOILERS " ""))
         (post_type (if (string= (dank-post-post_type post) "self")
                                    "self-text" (dank-post-post_type post)))
         (domain (if (string= post_type "self-text") "" (concat " from " (dank-post-domain post))))
         (link_flair (if (dank-post-link_flair post)
                         (concat "[" (dank-post-link_flair post) "] ") ""))
         (format-context `(title (,title . dank-faces-post-title) age (,age . dank-faces-age) author (,author . dank-faces-post-author) author_flair (,author_flair . dank-faces-flair)
                                 subreddit (,subreddit . dank-faces-subreddit) vote (,vote . ,vote-face) score (,score . ,vote-face) num_comments (,num_comments . dank-faces-downvote)
                                 nsfw (,nsfw . dank-faces-nsfw) spoiler (,spoiler . dank-faces-nsfw) domain (,domain . dank-faces-site-domain) post_type (,post_type . dank-faces-post-type)
                                 link_flair (,link_flair . dank-faces-flair)))
         (formatted-post (dank-utils-format-plist dank-post-template format-context)))
    formatted-post))

(defun dank-post-parse (post)
  "Parse POST into a `dank-post'."
  (let* ((post (plist-get post :data)))
    (make-dank-post :id (plist-get post :id)
                    :name (plist-get post :name)
                    :title (dank-utils-escape-html (string-trim (plist-get post :title)))
                    :link (plist-get post :url)
                    :text (plist-get post :selftext)
                    :age (dank-utils-timestamp-ago (plist-get post :created_utc))
                    :date (plist-get post :created_utc)
                    :likes (plist-get post :likes)
                    :score (plist-get post :score)
                    :author (plist-get post :author)
                    :subreddit (plist-get post :subreddit)
                    :num_comments (plist-get post :num_comments)
                    :domain (plist-get post :domain)
                    :post_type (or (plist-get post :post_hint) (if (eq (plist-get post :is_self) t) "self" "link"))
                    :nsfw (not (eq (plist-get post :over_18) :false))
                    :spoiler (not (eq (plist-get post :spoiler) :false))
                    :link_flair (plist-get post :link_flair_text)
                    :author_flair (plist-get post :author_flair_text)
                    :gilded (plist-get post :gilded)
                    :stickied (not (eq (plist-get post :stickied) :false))
                    :locked (not (eq (plist-get post :locked) :false))
                    :permalink (plist-get post :permalink))))

(defun dank-post-subreddit-parse (subreddit)
  "Parse SUBREDDIT into a `dank-subreddit'."
  (let ((subreddit (plist-get subreddit :data)))
    (make-dank-subreddit :id (plist-get subreddit :name)
                         :title (plist-get subreddit :title)
                         :url (plist-get subreddit :url)
                         :description (plist-get subreddit :description))))

(defun dank-post-format-content (post)
  "Format POST content as string."
  (concat
   (if (string= (dank-post-post_type post) "self")
       (dank-utils-markdown-fill-paragraph-and-indent (dank-post-text post) 0 dank-comments-body-fill-width "")
     (dank-post-link post))
   "\n"
   (propertize (make-string dank-comments-body-fill-width ?\s) 'font-lock-face 'dank-faces-separator)))

(defun dank-post--ewoc-pp (post)
  "EWOC pretty-printer for POST."
  (insert (dank-post-format post)))

(provide 'dank-post)

;;; dank-post.el ends here
