(defvar dank-backend--active-requests nil)

(cl-defstruct dank-post id slug url title body score user date)
(cl-defstruct dank-comment id post-id url body score user date)

(defun dank-backend--cancel-active-request ())

(defun dank-backend--do-request (f))

(defun dank-backend-get-posts (get-posts-f))

(provide 'dank-backend)
