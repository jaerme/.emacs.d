(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(defconst my-custom-file (locate-user-emacs-file "custom.el")
  "File used to store settings from Customization UI.")

(setq temporary-file-directory (expand-file-name "~/.emacs.d/tmp"))
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'gruvbox t)

(set-face-attribute 'default nil
                    :family "DejaVu Sans Mono" :height 98)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'system)

;; get packages
(use-package cus-edit
  :defer t
  :config
  (setq custom-file my-custom-file
        custom-buffer-done-kill nil            
        custom-buffer-verbose-help nil         
        ;; Show me the real variable name
        custom-unlispify-tag-names nil
        custom-unlispify-menu-entries nil)
  :init (load my-custom-file 'no-error 'no-message))

(use-package clojure-mode
  :ensure t)

(use-package cider
  :ensure t)

(use-package paredit
  :diminish paredit-mode
  :init
  (add-hook 'clojure-mode-hook 'enable-paredit-mode)
  (add-hook 'cider-repl-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
  (add-hook 'ielm-mode-hook 'enable-paredit-mode)
  (add-hook 'json-mode-hook 'enable-paredit-mode))

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package slime
  :ensure t
  :defer t
  :config
  (setq inferior-lisp-program "sbcl")
  (setq slime-contribs '(slime-fancy)))

(use-package company     
  :ensure t
  :init (global-company-mode)
  :config
  (progn
    (delete 'company-dabbrev company-backends)
    (setq company-tooltip-align-annotations t
	  company-tooltip-minimum-width 27
	  company-idle-delay 0.3
	  company-tooltip-limit 10
	  company-minimum-prefix-length 2
	  company-tooltip-flip-when-above t))
  :diminish company-mode)

(use-package js2-mode
  :ensure t
  :mode (("\\.jsx?\\'" . js2-jsx-mode))
  :config
  (setq js2-basic-offset 2)
  (setq js2-strict-missing-semi-warning nil)
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil))

(use-package web-mode
  :ensure t
  :config (progn
            (setq web-mode-markup-indent-offset 2
                  web-mode-css-indent-offset 2
                  web-mode-code-indent-offset 2)))

(use-package projectile
  :ensure t
  :bind (("C-x p" . projectile-persp-switch-project))
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching nil)
  :diminish (projectile-mode))

(use-package geiser
  :ensure t)

(use-package swiper
  :ensure t
  :diminish (ivy-mode . "")
  :config
  (ivy-mode 1)
  ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  (setq ivy-use-virtual-buffers t)
  ;; number of result lines to display
  (setq ivy-height 20)
  ;; does not count candidates
  (setq ivy-count-format "")
  ;; no regexp by default
  (setq ivy-initial-inputs-alist nil)
  ;; configure regexp engine.
  (setq ivy-re-builders-alist
	;; allow input not in order
        '((t   . ivy--regex-ignore-order))))

(use-package elm-mode
  :ensure t)
  
(provide 'init)
