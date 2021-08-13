;; local config file for dank-mode development
;; $ emacs -nw -q -l /path/to/dank-mode/localdev/init.el

(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
  (load custom-file))

;; install dependencies
(require 'package)
(add-to-list 'package-archives
  '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(package-install 'dash)
(package-install 'request)
(package-install 's)

;; add this package's lisp directory to the load path
(push (expand-file-name "../lisp" user-emacs-directory) load-path)

(require 'cl)
(require 'dank-mode)
(setq dank-auth-file (expand-file-name "auth.json" user-emacs-directory))
(setq dank-cache-directory (expand-file-name "cache" user-emacs-directory))
(dank-mode)
