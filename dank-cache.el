;(defcustom dank-backend-cache-directory (expand-file-name "dank-mode" user-emacs-directory))

(defcustom dank-cache-enabled t "")
(defcustom dank-cache-directory "cache" "")
(defcustom dank-cache-timeout 3600 "")

(defun dank-cache--create-cache-directory ()
  "Create the cache directory `dank-cache-directory'."
  (when dank-cache-enabled
    (unless (file-exists-p dank-cache-directory)
      (make-directory dank-cache-directory))))

(defun dank-cache-key (request-url)
  "Return a cache-friendly key from REQUEST-URL."
  (print request-url)
  (print (md5 request-url)))

(defun dank-cache--full-key (key)
  "Get the full path of KEY."
  (expand-file-name key dank-cache-directory))

(defun dank-cache--read-cache (cache-file)
  "Read the contents of CACHE-FILE."
  (with-temp-buffer
    (insert-file-contents cache-file)
    (buffer-string)))

(defun dank-cache-get (key)
  "Fetch contents of KEY from cache and return it as evaluated list.
When AS-RAW-STRING is non-nil, then don't eval as a list."
  (when (and dank-cache-enabled (file-readable-p (dank-cache--full-key key)))
    (dank-cache--read-cache (dank-cache--full-key key))))

(defun dank-cache-key-exists (key)
  "Check if KEY exists in cache."
  (file-readable-p (dank-cache--full-key key)))

(defun dank-cache-set (key value)
  "Set KEY in cache with VALUE."
  (when (and dank-cache-enabled (file-writable-p dank-cache-directory))
    (with-temp-file (dank-cache--full-key key)
      (insert value))))

(defun dank-cache-delete (key)
  "Delete KEY from cache.")

(defun dank-cache-delete-all ()
  "Delete everything in cache.")

(defun dank-cache-cleanup ()
  "Delete files from cache that exceed `dank-cache-timeout'.")

(provide 'dank-cache)
