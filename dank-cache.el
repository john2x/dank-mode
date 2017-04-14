;(defcustom dank-backend-cache-directory (expand-file-name "dank-mode" user-emacs-directory))

(defcustom dank-cache-enabled t)
(defcustom dank-cache-directory "cache")
(defcustom dank-cache-timeout 3600)

(defun dank-cache--create-cache-directory ()
  "Create the cache directory `dank-cache-directory'.")

(defun dank-cache-get (key)
  "Fetch KEY from cache.")

(defun dank-cache-set (key value)
  "Set KEY in cache with VALUE.")

(defun dank-cache-delete (key)
  "Delete KEY from cache.")

(defun dank-cache-delete-all ()
  "Delete everything in cache.")

(defun dank-cache-cleanup ()
  "Delete files from cache that exceed `dank-cache-timeout'.")

(provide 'dank-cache)
