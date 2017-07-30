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

(defface dank-faces-author
  '((t :inherit font-lock-keyword-face))
  "Face for author username."
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

(provide 'dank-faces)
