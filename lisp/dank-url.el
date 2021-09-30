;;; dank-url.el --- Major mode for browsing Reddit

;; Copyright (C) 2021 John Louis Del Rosario

;; Author: John Louis Del Rosario <john2x@gmail.com>
;; Version: 0.1.0
;; Keywords: reddit, social

;;; Commentary:

;; This file defines helper and utility functions to work with url.el

;;; Code:

(require 'url)

;; This is copied from https://github.com/tkf/emacs-request
(defun dank-url-encode-alist (alist)
  "Hexify ALIST fields according to RFC3986."
  (cl-loop for sep = "" then "&"
           for (k . v) in alist
           concat sep
           concat (url-hexify-string (format "%s" k))
           concat "="
           concat (url-hexify-string (format "%s" v))))

(defun dank-url-response-status-code ()
  "Parse the first header line such as \"HTTP/1.1 200 OK\" and return the status code."
  (when (re-search-forward "\\=[ \t\n]*HTTP/\\([0-9\\.]+\\) +\\([0-9]+\\)" nil t)
    (string-to-number (match-string 2))))

(defun dank-url-response-header (header)
  "Get the value of HEADER in the current response buffer.
HEADER should be a lower-case string."
  (goto-char (point-min))
  (when (re-search-forward (concat "^" header ": "))
    (buffer-substring (point) (line-end-position))))

(defun dank-url-response-uncompress ()
  "Uncompress the content of the `url-retrieve' response buffer BUF."
  (let ((filename (make-temp-file (buffer-name (current-buffer)) nil ".gz")))
    (search-forward "\n\n")               ; Skip response headers.
    (write-region (point) (point-max) filename)
    (with-auto-compression-mode
      (with-temp-buffer
        (insert-file-contents filename)
        (buffer-string)))))

;;; dank-url.el ends here
