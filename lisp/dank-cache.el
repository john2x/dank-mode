;;; dank-cache.el --- Major mode for browsing Reddit

;; Copyright (C) 2021 John Louis Del Rosario

;; Author: John Louis Del Rosario <john2x@gmail.com>
;; Version: 0.1.0
;; Keywords: reddit, social

;;; Commentary:

;; This file defines functions for caching Reddit API responses.

;;; Code:

;(defcustom dank-backend-cache-directory (expand-file-name "dank-mode" user-emacs-directory))

(defcustom dank-cache-enabled t
  "Enable caching API responses.")
(defcustom dank-cache-directory "cache"
  "Name of the cache directory.")
(defcustom dank-cache-timeout 60
  "Maximum age of cached objects in seconds.")

(defun dank-cache--create-cache-directory ()
  "Create the cache directory `dank-cache-directory'."
  (when dank-cache-enabled
    (unless (file-exists-p dank-cache-directory)
      (make-directory dank-cache-directory))))

(defun dank-cache-key (request-url)
  "Return a cache-friendly key from REQUEST-URL."
  (md5 request-url))

(defun dank-cache--full-key (key)
  "Get the full path of KEY."
  (expand-file-name key dank-cache-directory))

(defun dank-cache--read-cache (cache-file)
  "Read the contents of CACHE-FILE."
  (with-temp-buffer
    (insert-file-contents cache-file)
    (buffer-string)))

(defun dank-cache-get (key)
  "Fetch contents of KEY from cache and return it as evaluated list."
  (when (and dank-cache-enabled (dank-cache-key-exists key))
    (dank-cache--read-cache (dank-cache--full-key key))))

(defun dank-cache-key-exists (key)
  "Check if KEY exists in cache and is not expired."
  (when (file-readable-p (dank-cache--full-key key))
    (if (dank-cache-key-expired-p key)
        nil t)))

(defun dank-cache-set (key value)
  "Set KEY in cache with VALUE."
  (when (and dank-cache-enabled (file-writable-p dank-cache-directory))
    (with-temp-file (dank-cache--full-key key)
      (insert value))))

(defun dank-cache-delete (key)
  "Delete KEY from cache."
  (when (dank-cache-key-exists key)
    (delete-file (dank-cache--full-key key))))

(defun dank-cache-delete-all ()
  "Delete everything in cache."
  (when (file-exists-p dank-cache-directory)
    (delete-directory dank-cache-directory t))
  (dank-cache--create-cache-directory))

(defun dank-cache-delete-expired ()
  "Delete files from cache that exceed `dank-cache-timeout'.")

(defun dank-cache-key-expired-p (key &optional delete)
  "Return t if KEY exists and is expired.
If DELETE is non-nil, delete the key if it is expired."
  (when (file-readable-p (dank-cache--full-key key))
    (let* ((modified (nth 4 (file-attributes (dank-cache--full-key key))))
           (now (current-time))
           (modified-secs (+ (* (car modified) 65536) (cadr modified)))
           (now-secs (+ (* (car now) 65536) (cadr now))))
      (when (> (- now-secs modified-secs) dank-cache-timeout)
        (if delete (dank-cache-delete key))
        t))))

(provide 'dank-cache)

;;; dank-cache.el ends here
