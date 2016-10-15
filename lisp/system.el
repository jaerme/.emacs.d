;; begin
(setq inhibit-splash-screen t
      initial-major-mode 'emacs-lisp-mode
      initial-scratch-message "sup")

(defalias 'yes-or-no-p 'y-or-n-p)
(setq-default indent-tabs-mode nil)
(global-linum-mode t)
(show-paren-mode t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(delete-selection-mode t)
(global-auto-revert-mode t)

(setq delete-old-versions t)
(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq ring-bell-function 'ignore)
(setq browse-url-browser-function 'browse-url-xdg-open)
(fset 'display-startup-echo-area-message #'ignore)

(provide 'system)

