;;; dank-mode-faces.el --- Major mode for browsing Reddit

;; Copyright (C) 2021 John Louis Del Rosario

;; Author: John Louis Del Rosario <john2x@gmail.com>
;; Version: 0.2.0
;; Keywords: reddit, social

;;; Commentary:

;; This file defines faces for dank-mode.

;;; Code:

(defvar dank-mode-faces-color-orangered "#FF4500")
(defvar dank-mode-faces-color-upvote "#FF8B60")
(defvar dank-mode-faces-color-downvote "#9494FF")

(defface dank-mode-faces-default
  '((t :inherit default))
  "Face for default dank text."
  :group 'dank-mode-faces)

(defface dank-mode-faces-upvote
  `((t :foreground ,dank-mode-faces-color-upvote))
  "Face for upvotes."
  :group 'dank-mode-faces)

(defface dank-mode-faces-downvote
  `((t :foreground ,dank-mode-faces-color-downvote))
  "Face for downvotes."
  :group 'dank-mode-faces)

(defface dank-mode-faces-post-title
  '((t :inherit default :weight bold))
  "Face for post titles."
  :group 'dank-mode-faces)

(defface dank-mode-faces-age
  '((t :inherit default))
  "Face for post age."
  :group 'dank-mode-faces)

(defface dank-mode-faces-post-author
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for post author username."
  :group 'dank-mode-faces)

(defface dank-mode-faces-subreddit
  '((t :inherit font-lock-type-face))
  "Face for subreddit."
  :group 'dank-mode-faces)

(defface dank-mode-faces-flair
  '((t :inherit font-lock-comment-face))
  "Face for flairs."
  :group 'dank-mode-faces)

(defface dank-mode-faces-nsfw
  '((t :inherit default))
  "Face for NSFW labels."
  :group 'dank-mode-faces)

(defface dank-mode-faces-post-type
  '((t :inherit default))
  "Face for post types."
  :group 'dank-mode-faces)

(defface dank-mode-faces-site-domain
  '((t :inherit default))
  "Face for link site domains."
  :group 'dank-mode-faces)

(defface dank-mode-faces-highlight
  '((t :inherit highlight))
  "Face for highlighted sections."
  :group 'dank-mode-faces)

(defface dank-mode-faces-separator
  '((t :background "light gray"))
  "Face for separators."
  :group 'dank-mode-faces)

(defface dank-mode-faces-comment-author
  '((t :inherit font-lock-string-face :background "white smoke"))
  "Face for comment author username."
  :group 'dank-mode-faces)

(defface dank-mode-faces-comment-author-op
  '((t :inherit dank-mode-faces-post-author :background "white smoke"))
  "Face for comment author username when they are the post author."
  :group 'dank-mode-faces)

(defface dank-mode-faces-comment-metadata
  '((t :background "white smoke"))
  "Face for comment metadata."
  :group 'dank-mode-faces)

(defface dank-mode-faces-comment-more
  '((t :inherit font-lock-comment-face))
  "Face for comment 'more' placeholder."
  :group 'dank-mode-faces)

(provide 'dank-mode-faces)

;;; dank-mode-faces.el ends here
