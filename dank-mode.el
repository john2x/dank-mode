(require 'dank-auth)
(require 'dank-backend)
(require 'dank-listing)

(defvar-local dank-mode-initialized nil)
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

(defun dank-mode ()
  (interactive)
  (switch-to-buffer "*dank-mode*")
  (dank-listing-mode)
  (unless dank-mode-initialized
    (dank-mode-init)))

(defun dank-mode-reload ()
  (interactive)
  (dank-mode-init))

(defun dank-mode-init ()
  "Initialize dank-mode buffer."
  (let ((inhibit-read-only t))
    (erase-buffer))
  (dank-mode--init-auth)
  (setq dank-mode-initialized t))

(defun dank-mode--init-auth ()
  "Initialize dank-mode auth."
  (when dank-auth-file
    (unless (dank-auth-configured-p)
      (dank-auth-load-auth-vars-from-file dank-auth-file))))

(defun dank-mode--init-front-page ()
  "Render the user's front page."
  (unless dank-listing-buffer
    (setq dank-listing-buffer (get-buffer "*dank-mode*")))
  (with-current-buffer "*dank-mode*"
    (let ((posts (dank-backend-post-listing nil 'hot))
          (inhibit-read-only t))
      (cl-loop for post across posts do
               (dank-listing-insert post)))))


(provide 'dank-mode)
