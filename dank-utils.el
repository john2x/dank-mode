(require 's)

(defun dank-warning (type &rest message-fmt)
  "Convenience method to print warning messages of TYPE for dank-reddit and return nil.
Passes MESSAGE-FMT to `format-message'."
  (progn (display-warning type (apply 'format-message message-fmt) :warning "*dank-mode warnings*")
         nil))

(defun dank-utils-format-plist (s plist)
  "Format string S with data from PLIST."
  (s-format s (lambda (var &optional extra) (plist-get extra (intern var))) plist))

(provide 'dank-utils)
