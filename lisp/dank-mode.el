;;; dank-mode.el --- Major mode for browsing Reddit

;; Copyright (C) 2021 John Louis Del Rosario

;; Author: John Louis Del Rosario <john2x@gmail.com>
;; Version: 0.1.5
;; Keywords: reddit, social

;;; Commentary:

;; This file defines the entry point for dank-mode (dank-posts-mode
;; and dank-comments-mode).
;;
;; Before you can start using dank-mode, you will need to configure
;; your auth credentials. See dank-auth.el for details.
;;
;; To start a dank-mode session, use `M-x dank-mode`. This will start
;; a dank-posts-mode buffer with the hot posts of your frontpage.
;;
;; To read a post's comments, use `M-x
;; dank-posts-goto-post-comments-at-point`.

;;; Code:

(require 'dank-auth)
(require 'dank-backend)
(require 'dank-posts)

(defvar dank-mode-initialized nil)

;;;###autoload
(defun dank-mode ()
  "Entrypoint function for dank-mode.  Initialize a dank-posts-mode buffer for the reddit frontpage."
  (interactive)
  (dank-mode-init)
  (dank-posts-init nil))

(defun dank-mode-reset ()
  "Reset dank-mode.  Kill all buffers, clear the cache, and re-initialize auth."
  (interactive)
  (setq dank-mode-initialized nil)
  ;; TODO: kill all dank-mode buffers
  (dank-mode))

(defun dank-mode-init ()
  "Initialize dank-mode cache and auth."
  (unless dank-mode-initialized
    (dank-auth-read-from-disk)
    (unless dank-auth--credentials
      (dank-auth-prompt))
    (setq dank-mode-initialized t)))

(provide 'dank-mode)
