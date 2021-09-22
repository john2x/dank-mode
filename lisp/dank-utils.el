(require 's)
(require 'xml)
(require 'markdown-mode)

(defun dank-warning (type &rest message-fmt)
  "Convenience method to print warning messages of TYPE for dank-reddit and return nil.
Passes MESSAGE-FMT to `format-message'."
  (progn (display-warning type (apply 'format-message message-fmt) :warning "*dank-mode warnings*")
         nil))

(defun dank-utils-format-plist (template plist &optional before-face after-face)
  "Format string TEMPLATE with data from PLIST.

Values in the PLIST can be a cons cell where the cdr is the face
to apply on that part of the template.  Optional BEFORE-FACE will
apply that face to the template before formatting (and during, if
that value has no specific face provided).  Optional AFTER-FACE
will be applied after the template is rendered. Useful for
applying background faces."
  (let ((formatted (s-format (if  before-face
                                 (propertize template 'font-lock-face before-face)
                               template)
                             (lambda (var &optional extra)
                               (let ((value (plist-get extra (intern var))))
                                 (if (eq (type-of value) 'cons)
                                     (propertize (car value) 'font-lock-face (cdr value))
                                   (if before-face (propertize value 'font-lock-face before-face) value))))
                             plist)))
    (if after-face
        (propertize formatted 'font-lock-face after-face)
      formatted)))

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
  "Use `markdown-fill-paragraph' on Markdown BODY up to FILL-COLUMN width.  Indent BODY by DEPTH at the same time."
  (let ((fill-column (- fill-column (* 2 depth))) ;; subtract twice of depth from fill-column because the indent will take up part of the fill width
        (fill-prefix (concat (s-repeat depth "  ") "| ")))
    (with-temp-buffer
      (insert body)
      (beginning-of-buffer)
      (xml-parse-string)
      (beginning-of-buffer)
      (while (not (eobp))
        ;; this is probably not ideal with large comment trees
        (markdown-fill-paragraph)
        (markdown-fill-forward-paragraph 1))
      (when (> depth 0)
        (beginning-of-buffer)
        (while (not (eobp))
          (beginning-of-line)
          (insert fill-prefix)
          (forward-line)))
      (beginning-of-buffer)
      (buffer-string))))

(defun dank-utils-escape-html (s)
  "Escape HTML from S.
TODO: optimize this."
  (with-temp-buffer
    (insert s)
    (beginning-of-buffer)
    (xml-parse-string)
    (buffer-string)))

(defun dank-utils-make-highlight-overlay ()
  "Return an overlay for highlighting text ranges."
  (let ((ovl (make-overlay 1 1)))
    (overlay-put ovl 'category 'dank-point-highlight)
    (overlay-put ovl 'font-lock-face 'dank-faces-highlight)
    (overlay-put ovl 'priority '(nil . 99))
    ovl))

(defun dank-utils-get-prop (point property)
  "Get the value of a PROPERTY at POINT."
  (plist-get (text-properties-at point) property))

(provide 'dank-utils)
