(require 'dank-auth)
(require 'dank-backend)
(require 'dank-cache)
(require 'dank-posts)

(defvar dank-mode-initialized nil)

;;;###autoload
(defun dank-mode ()
  "Entrypoint function for dank-mode.  Initialize a dank-posts-mode buffer for the reddit frontpage."
  (interactive)
  (unless dank-mode-initialized
    (dank-mode-init))
  (dank-posts-init nil))

(defun dank-mode-reset ()
  "Reset dank-mode.  Kill all buffers, clear the cache, and re-initialize auth."
  (interactive)
  (setq dank-mode-initialized nil)
  ;; TODO: kill all dank-mode buffers
  (dank-mode))

(defun dank-mode-init ()
  "Initialize dank-mode cache and auth."
  (dank-mode--init-cache)
  (dank-mode--init-auth)
  (setq dank-mode-initialized t))

(defun dank-mode--init-cache ()
  "Initialize dank-mode's cache (i.e. clear it)."
  (message "Clearing dank-cache...")
  (dank-cache-delete-all))

(defun dank-mode--init-auth ()
  "Initialize dank-mode auth."
  (message "Initialize dank-auth...")
  (when dank-auth-file
    (unless (dank-auth-configured-p)
      (dank-auth-load-auth-vars-from-file dank-auth-file))))

(provide 'dank-mode)
