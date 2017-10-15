(require 's)

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

(provide 'dank-utils)
