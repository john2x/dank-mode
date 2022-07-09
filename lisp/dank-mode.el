;;; dank-mode.el --- Major mode for browsing Reddit

;; Copyright (C) 2021 John Louis Del Rosario

;; Author: John Louis Del Rosario <john2x@gmail.com>
;; Version: 0.2.0
;; Keywords: reddit, social
;; Package-Requires: ((emacs "27.1") (oauth2 "0.16") (web-server "0.1.2") (markdown-mode "2.6-dev"))

;;; Commentary:

;; This file defines the entry point for dank-mode (dank-posts-mode
;; and dank-comments-mode).
;;
;; To start a dank-mode session, use `M-x dank-mode`. If this is the
;; first time dank-mode is run, you will be redirected to Reddit's
;; OAuth page for authorization. Once that's done, a dank-posts-mode
;; buffer with the hot posts of your frontpage will be opened.
;;
;; To read a post's comments, use `M-x
;; dank-posts-goto-post-comments-at-point`.

;;; Code:

(require 'dank-oauth)
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
    (dank-oauth-read-from-disk)
    (unless dank-oauth--token-data
      (dank-oauth-start))
    (setq dank-mode-initialized t)))

(provide 'dank-mode)
