;; custom settings
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(setq inhibit-splash-screen t
      initial-major-mode 'emacs-lisp-mode
      initial-scratch-message "")

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
(setq browse-url-browser-function 'eww-browse-url)
(fset 'display-startup-echo-area-message #'ignore)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; OS X
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

(provide 'settings)

