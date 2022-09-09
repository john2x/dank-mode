;;; dank-mode-post.el --- Major mode for browsing Reddit -*- lexical-binding: t -*-

;; Copyright (C) 2021 John Louis Del Rosario

;; Author: John Louis Del Rosario <john2x@gmail.com>
;; Version: 0.2.0
;; Keywords: reddit, social

;;; Commentary:

;; This file defines structs and utility functions for the dank-mode-posts
;; major mode.

;;; Code:

(require 'dank-mode-utils)
(require 'dank-mode-faces)

(defcustom dank-mode-post-upvote-symbol "🡅"
  "Symbol to use for upvotes. Change this if the emoji doesn't work correctly."
  :type 'string
  :group 'dank-mode)

(defcustom dank-mode-post-downvote-symbol "🡇"
  "Symbol to use for downvotes. Change this if the emoji doesn't work correctly."
  :type 'string
  :group 'dank-mode)

(cl-defstruct dank-mode-post
  "Struct for a Reddit post."
  name id title link permalink text age date author subreddit score num_comments
  domain post_type nsfw spoiler link_flair author_flair
  gilded stickied locked likes)

(cl-defstruct dank-subreddit
  "Struct for a subreddit."
  id title description url)

;; Rendering

(defvar dank-mode-post-template
  "${title}\n    | ${vote}${score} points | ${num_comments} comments | ${link_flair}${nsfw}${spoiler}${post_type}${domain}\n    | ${subreddit} submitted by ${author}${author_flair} ${age}")

(defun dank-mode-post-format (post)
  "Format POST as string using `dank-mode-post-template'.
Also applies font-lock properties."
  (let* ((title (dank-mode-post-title post))
         (age (dank-mode-post-age post))
         (author (concat "/u/" (dank-mode-post-author post)))
         (author_flair (if (and (dank-mode-post-author_flair post) (not (eq (dank-mode-post-author_flair post) "")))
                           (concat " [" (dank-mode-post-author_flair post) "]")
                         ""))
         (subreddit (concat "/r/" (dank-mode-post-subreddit post)))
         (score (format "%s" (dank-mode-post-score post)))
         (likes (dank-mode-post-likes post))
         (vote (format "%s" (cond ((eq :false likes) dank-mode-post-downvote-symbol)
                                  (likes dank-mode-post-upvote-symbol)  ; likes is t if upvoted
                                  (t ""))))
         (vote-face (format "%s" (cond ((eq :false likes) 'dank-mode-faces-downvote)
                                       (likes 'dank-mode-faces-upvote)  ; likes is t if upvoted
                                       (t 'dank-mode-faces-upvote))))
         (num_comments (format "%s" (dank-mode-post-num_comments post)))
         (nsfw (if (dank-mode-post-nsfw post) "NSFW " ""))
         (spoiler (if (dank-mode-post-spoiler post) "SPOILERS " ""))
         (post_type (if (string= (dank-mode-post-post_type post) "self")
                        "self-text" (dank-mode-post-post_type post)))
         (domain (if (string= post_type "self-text") "" (concat " from " (dank-mode-post-domain post))))
         (link_flair (if (dank-mode-post-link_flair post)
                         (concat "[" (dank-mode-post-link_flair post) "] ") ""))
         (format-context `(title (,title . dank-mode-faces-post-title) age (,age . dank-mode-faces-age) author (,author . dank-mode-faces-post-author) author_flair (,author_flair . dank-mode-faces-flair)
                                 subreddit (,subreddit . dank-mode-faces-subreddit) vote (,vote . ,vote-face) score (,score . dank-mode-faces-upvote) num_comments (,num_comments . dank-mode-faces-downvote)
                                 nsfw (,nsfw . dank-mode-faces-nsfw) spoiler (,spoiler . dank-mode-faces-nsfw) domain (,domain . dank-mode-faces-site-domain) post_type (,post_type . dank-mode-faces-post-type)
                                 link_flair (,link_flair . dank-mode-faces-flair)))
         (formatted-post (dank-mode-utils-format-plist dank-mode-post-template format-context)))
    formatted-post))

(defun dank-mode-post-parse (post)
  "Parse POST into a `dank-mode-post'."
  (let* ((post (plist-get post :data)))
    (make-dank-mode-post :id (plist-get post :id)
                         :name (plist-get post :name)
                         :title (dank-mode-utils-escape-html (string-trim (plist-get post :title)))
                         :link (plist-get post :url)
                         :text (plist-get post :selftext)
                         :age (dank-mode-utils-timestamp-ago (plist-get post :created_utc))
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

(defun dank-mode-post-subreddit-parse (subreddit)
  "Parse SUBREDDIT into a `dank-subreddit'."
  (let ((subreddit (plist-get subreddit :data)))
    (make-dank-subreddit :id (plist-get subreddit :name)
                         :title (plist-get subreddit :title)
                         :url (plist-get subreddit :url)
                         :description (plist-get subreddit :description))))

(defun dank-mode-post-format-content (post)
  "Format POST content as string."
  (concat
   (if (string= (dank-mode-post-post_type post) "self")
       (dank-mode-utils-markdown-fill-paragraph-and-indent (dank-mode-post-text post) 0 dank-mode-comments-body-fill-width "")
     (dank-mode-post-link post))
   "\n"
   (propertize (make-string dank-mode-comments-body-fill-width ?\s) 'font-lock-face 'dank-mode-faces-separator)))

(defun dank-mode-post--ewoc-pp (post)
  "EWOC pretty-printer for POST."
  (insert (dank-mode-post-format post)))

(provide 'dank-mode-post)

;;; dank-mode-post.el ends here
