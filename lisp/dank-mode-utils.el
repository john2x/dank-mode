;;; dank-mode-utils.el --- Major mode for browsing Reddit -*- lexical-binding: t -*-

;; Copyright (C) 2021 John Louis Del Rosario

;; Author: John Louis Del Rosario <john2x@gmail.com>
;; Version: 0.2.0
;; Keywords: reddit, social

;;; Commentary:

;; This file defines common utility functions for dank-mode.

;;; Code:

(require 'xml)

(defcustom dank-mode-utils-fill-paragraph-function 'fill-paragraph
  "Function to use to fill content paragraphs."
  :type 'function
  :group 'dank-mode)

(defcustom dank-mode-utils-fill-forward-paragraph-function 'fill-forward-paragraph
  "Function to use to fill forward content paragraphs."
  :type 'function
  :group 'dank-mode)

(defcustom dank-mode-prefer-new-reddit nil
  "Prefer new Reddit over old Reddit."
  :type 'boolean
  :group 'dank-mode)

(when (require 'markdown-mode nil 'noerror)
  (setq dank-mode-utils-fill-paragraph-function 'markdown-fill-paragraph)
  (setq dank-mode-utils-fill-forward-paragraph-function 'markdown-fill-forward-paragraph))

(defun dank-warning (type &rest message-fmt)
  "Convenience method to print warning messages of TYPE for dank-reddit and return nil.
Passes MESSAGE-FMT to `format-message'."
  (progn (display-warning type (apply 'format-message message-fmt) :warning "*dank-mode warnings*")
         nil))

(defun dank-mode-utils-format-plist (template plist &optional before-face after-face)
  "Format string TEMPLATE with data from PLIST.

Values in the PLIST can be a cons cell where the cdr is the face
to apply on that part of the template.  Optional BEFORE-FACE will
apply that face to the template before formatting (and during, if
that value has no specific face provided).  Optional AFTER-FACE
will be applied after the template is rendered. Useful for
applying background faces."
  (let ((formatted (dank-mode-utils-s-format
                    (if  before-face
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

(defun dank-mode-utils-timestamp-ago (timestamp)
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

(defun dank-mode-utils-markdown-fill-paragraph-and-indent (body depth fill-column &optional indent-guide)
  "Use `dank-mode-utils-fill-paragraph-function' on Markdown BODY up to FILL-COLUMN width.  Indent BODY by DEPTH at the same time."
  (let ((fill-column (- fill-column (* 2 depth))) ;; subtract twice of depth from fill-column because the indent will take up part of the fill width
        (fill-prefix (concat (make-string (* 2 depth) ?\s) "| ")))
    (with-temp-buffer
      (insert body)
      (beginning-of-buffer)
      (xml-parse-string)
      (beginning-of-buffer)
      (while (not (eobp))
        ;; this is probably not ideal with large comment trees
        (funcall dank-mode-utils-fill-paragraph-function)
        (funcall dank-mode-utils-fill-forward-paragraph-function 1))
      (when (> depth 0)
        (beginning-of-buffer)
        (while (not (eobp))
          (beginning-of-line)
          (insert fill-prefix)
          (forward-line)))
      (beginning-of-buffer)
      (buffer-string))))

(defun dank-mode-utils-escape-html (s)
  "Escape HTML from S.
TODO: optimize this."
  (with-temp-buffer
    (insert s)
    (beginning-of-buffer)
    (xml-parse-string)
    (buffer-string)))

(defun dank-mode-utils-make-highlight-overlay ()
  "Return an overlay for highlighting text ranges."
  (let ((ovl (make-overlay 1 1)))
    (overlay-put ovl 'category 'dank-point-highlight)
    (overlay-put ovl 'font-lock-face 'dank-mode-faces-highlight)
    (overlay-put ovl 'priority '(nil . 99))
    ovl))

(defun dank-mode-utils-ewoc-data (ewoc pos)
  "Get the data of the EWOC node at POS."
  (let ((node (ewoc-locate ewoc pos)))
    (when node
      (ewoc-data node))))

;; These EWOC functions were copied from https://github.com/alphapapa/ement.el
(cl-defun dank-mode-utils-ewoc-next-match-node (ewoc node pred &optional (move-fn #'ewoc-next))
  "Return the next node in EWOC after NODE that PRED is true of.
PRED is called with node's data.  Moves to next node by MOVE-FN."
  (declare (indent defun))
  (cl-loop do (setf node (funcall move-fn ewoc node))
           until (or (null node)
                     (funcall pred (ewoc-data node)))
           finally return node))

;; This formatting function was copied from https://github.com/magnars/s.el
;; Errors for s-format
(progn
  (put 'dank-mode-utils-s-format-resolve
       'error-conditions
       '(error dank-mode-utils-s-format dank-mode-utils-s-format-resolve))
  (put 'dank-mode-utils-s-format-resolve
       'error-message
       "Cannot resolve a template to values"))

(defun dank-mode-utils-s-format (template replacer &optional extra)
  "Format TEMPLATE with the function REPLACER.
REPLACER takes an argument of the format variable and optionally
an extra argument which is the EXTRA value from the call to
`dank-mode-utils-s-format'.
Several standard `dank-mode-utils-s-format' helper functions are recognized and
adapted for this:
    (dank-mode-utils-s-format \"${name}\" 'gethash hash-table)
    (dank-mode-utils-s-format \"${name}\" 'aget alist)
    (dank-mode-utils-s-format \"$0\" 'elt sequence)
The REPLACER function may be used to do any other kind of
transformation."
  (let ((saved-match-data (match-data)))
    (unwind-protect
        (replace-regexp-in-string
         "\\$\\({\\([^}]+\\)}\\|[0-9]+\\)"
         (lambda (md)
           (let ((var
                  (let ((m (match-string 2 md)))
                    (if m m
                      (string-to-number (match-string 1 md)))))
                 (replacer-match-data (match-data)))
             (unwind-protect
                 (let ((v
                        (cond
                         ((eq replacer 'gethash)
                          (funcall replacer var extra))
                         ((eq replacer 'aget)
                          (cdr (assoc-string var extra)))
                         ((eq replacer 'elt)
                          (funcall replacer extra var))
                         ((eq replacer 'oref)
                          (funcall #'slot-value extra (intern var)))
                         (t
                          (set-match-data saved-match-data)
                          (if extra
                              (funcall replacer var extra)
                            (funcall replacer var))))))
                   (if v (format "%s" v) (signal 'dank-mode-utils-s-format-resolve md)))
               (set-match-data replacer-match-data))))
         template
         ;; Need literal to make sure it works
         t t)
      (set-match-data saved-match-data))))

(defun dank-mode-utils-reddit-url ()
  "Return the preferred Reddit URL"
  (if dank-mode-prefer-new-reddit
      "https://www.reddit.com"
    "https://old.reddit.com"))

(provide 'dank-mode-utils)

;;; dank-mode-utils.el ends here
