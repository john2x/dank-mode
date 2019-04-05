(require 'dank-auth)
(require 'dank-backend)
(require 'dank-cache)
(require 'dank-posts)

(defvar dank-mode-initialized nil)
(defvar posts )
(defun dank--render-posts-list (posts)
  "")
;; create new buffer
;; fetch subreddits
;; push to history
;; render
;; open subreddit
;; push to history
;; render

(defun dank--render-post (post)
  "")

;;;###autoload
(defun dank-mode ()
  (interactive)
  ;; TODO: if the buffer is already open in another window, switch to that window instead
  (unless dank-mode-initialized
    (dank-mode-init))
  (dank-posts-init nil))

(defun dank-mode-reload ()
  (interactive)
  (dank-mode-reset)
  (dank-mode))

(defun dank-mode-init ()
  "Initialize dank-mode buffer."
  (dank-mode--init-cache)
  (dank-mode--init-auth)
  (setq dank-mode-initialized t))


(defun dank-mode-reset ()
  (setq dank-mode-initialized nil))

(defun dank-mode--init-cache ()
  "Initialize dank-mode's cache (i.e. clear it)"
  (message "clearing cache...")
  (dank-cache-delete-all))

(defun dank-mode--init-auth ()
  "Initialize dank-mode auth."
  (message "init auth...")
  (when dank-auth-file
    (unless (dank-auth-configured-p)
      (dank-auth-load-auth-vars-from-file dank-auth-file))))

(provide 'dank-mode)
