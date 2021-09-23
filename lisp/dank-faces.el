;;; dank-faces.el --- Major mode for browsing Reddit

;; Copyright (C) 2021 John Louis Del Rosario

;; Author: John Louis Del Rosario <john2x@gmail.com>
;; Version: 0.1.0
;; Keywords: reddit, social

;;; Commentary:

;; This file defines faces for dank-mode.

;;; Code:

(defvar dank-faces-color-orangered "#FF4500")
(defvar dank-faces-color-upvote "#FF8B60")
(defvar dank-faces-color-downvote "#9494FF")

(defface dank-faces-default
  '((t :inherit default))
  "Face for default dank text."
  :group 'dank-faces)

(defface dank-faces-upvote
  `((t :foreground ,dank-faces-color-upvote))
  "Face for upvotes."
  :group 'dank-faces)

(defface dank-faces-downvote
  `((t :foreground ,dank-faces-color-downvote))
  "Face for downvotes."
  :group 'dank-faces)

(defface dank-faces-post-title
  '((t :inherit default :weight bold))
  "Face for post titles."
  :group 'dank-faces)

(defface dank-faces-age
  '((t :inherit default))
  "Face for post age."
  :group 'dank-faces)

(defface dank-faces-post-author
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for post author username."
  :group 'dank-faces)

(defface dank-faces-subreddit
  '((t :inherit font-lock-type-face))
  "Face for subreddit."
  :group 'dank-faces)

(defface dank-faces-flair
  '((t :inherit font-lock-comment-face))
  "Face for flairs."
  :group 'dank-faces)

(defface dank-faces-nsfw
  '((t :inherit default))
  "Face for NSFW labels."
  :group 'dank-faces)

(defface dank-faces-post-type
  '((t :inherit default))
  "Face for post types."
  :group 'dank-faces)

(defface dank-faces-site-domain
  '((t :inherit default))
  "Face for link site domains."
  :group 'dank-faces)

(defface dank-faces-highlight
  '((t :inherit highlight))
  "Face for highlighted sections."
  :group 'dank-faces)

(defface dank-faces-separator
  '((t :background "light gray"))
  "Face for separators."
  :group 'dank-faces)

(defface dank-faces-comment-author
  '((t :inherit font-lock-string-face :background "white smoke"))
  "Face for comment author username."
  :group 'dank-faces)

(defface dank-faces-comment-author-op
  '((t :inherit dank-faces-post-author :background "white smoke"))
  "Face for comment author username when they are the post author."
  :group 'dank-faces)

(defface dank-faces-comment-metadata
  '((t :background "white smoke"))
  "Face for comment metadata."
  :group 'dank-faces)

(defface dank-faces-comment-more
  '((t :inherit font-lock-comment-face))
  "Face for comment 'more' placeholder."
  :group 'dank-faces)

(provide 'dank-faces)

;;; dank-faces.el ends here
