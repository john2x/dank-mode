(require 's)
(require 'xml)
(require 'markdown-mode)

(defun dank-warning (type &rest message-fmt)
  "Convenience method to print warning messages of TYPE for dank-reddit and return nil.
Passes MESSAGE-FMT to `format-message'."
  (progn (display-warning type (apply 'format-message message-fmt) :warning "*dank-mode warnings*")
         nil))

(defun dank-utils-format-plist (s plist)
  "Format string S with data from PLIST."
  (s-format s (lambda (var &optional extra) (plist-get extra (intern var))) plist))

(defun dank-utils-timestamp-ago (timestamp)
  "Return the 'time-ago' string of TIMESTAMP."
  (let* ((now (current-time))
         (now-secs (+ (* (car now) 65536) (cadr now)))
         (diff-secs (round (- now-secs timestamp))))
    (cond ((< diff-secs 60) "a few seconds ago")
          ((< diff-secs 3600) (format "%s minutes ago" (/ diff-secs 60)))
          ((< diff-secs 86400) (format "%s hours ago" (/ diff-secs 3600)))
          ((< diff-secs 2592000) (format "%s days ago" (/ diff-secs 86400)))
          ((< diff-secs 946080000) (format "%s months ago" (/ diff-secs 2592000)))
          (t (format "%s years ago" (/ diff-secs 946080000))))))

(defmacro dank-defrender (name buf arg-list &optional doc &rest body)
  "Define a render function named NAME that expects the buffer variable BUF to be non-nil and active."
  (declare (indent defun)
           (doc-string 4))
  `(defun ,name ,arg-list ,doc
          (if (buffer-live-p ,buf)
              (with-current-buffer ,buf
                ,@body)
            (message "Render failed"))))

(defun dank-utils-find-by-text-prop (prop value &optional direction)
  "Search current buffer for a range of text that has text property PROP and VALUE.
Optional DIRECTION can be either 'up, 'down, or nil.
If DIRECTION is nil, then the search starts at the top of the buffer.
If DIRECTION is 'up, the search starts from the current point postion going up.
If DIRECTION is 'down, the search starts from the current point position going down.
Returns the range of the text, if found, or nil if not found (until the end or beginning of the buffer).")

(defun dank-utils-markdown-fill-paragraph-and-indent (body depth fill-column &optional indent-guide)
  "Use `markdown-fill-paragraph' on Markdown BODY up to FILL-COLUMN width.  Indent BODY by INDENT at the same time."
  (let ((fill-column (- fill-column (* 2 depth)))) ;; subtract twice of depth from fill-column because the indent will take up part of the fill width
    (with-temp-buffer
      (save-excursion (insert body))
      (xml-parse-string) ;; unescape HTML in body
      (beginning-of-buffer)
      (while (not (eobp))
        ;; this is probably not ideal with large comment trees
        (markdown-fill-paragraph)
        (markdown-fill-forward-paragraph))
      (beginning-of-buffer)
      (replace-regexp "^" (concat (string-join (-repeat (* 2 depth) " ")) (or indent-guide "| ")))
      (buffer-string))
    ;; TODO: insert indent at the start of each line
    ))

(provide 'dank-utils)
