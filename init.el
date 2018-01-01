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

(add-to-list 'load-path (expand-file-name "settings" user-emacs-directory))
(require 'settings)

;; global packages
(setq use-package-always-ensure t)

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

(use-package exec-path-from-shell
  :defer t
  :init
  (progn
    (setq exec-path-from-shell-check-startup-files nil)
    (exec-path-from-shell-initialize)))

(use-package paredit
  :init
  (dolist (hook '(emacs-lisp-mode-hook
                  clojure-mode-hook
                  lisp-mode-hook
                  lisp-interaction-mode-hook
                  geiser-mode-hook))
    (add-hook hook 'paredit-mode))
  :diminish paredit-mode)

(use-package slime
  :defer t
  :config
  (progn
    (setq inferior-lisp-program "sbcl")
    (setq slime-contribs '(slime-fancy))))

(use-package undo-tree
  :diminish undo-tree-mode
  :config (global-undo-tree-mode))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

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

(use-package company-quickhelp
  :config (company-quickhelp-mode 1))

(use-package js2-mode
  :mode (("\\.js\\'" . js2-mode))
  :config (setq js2-basic-offset 2))

(use-package rjsx-mode
  :mode (("\\.jsx\\'" . rjsx-mode)
         ("components\\/.*\\.js\\'" . rjsx-mode)))

(use-package web-mode
  :config
  (progn
    (setq web-mode-markup-indent-offset 2
          web-mode-css-indent-offset 2
          web-mode-code-indent-offset 2
          web-mode-script-padding 0
          web-mode-style-padding 0))
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode)))

(use-package projectile
  :ensure t
  :bind (("C-x p" . projectile-persp-switch-project))
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching nil)
  :diminish (projectile-mode))

(use-package geiser)

(use-package swiper
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

(use-package neotree
  :config
  (global-set-key [f8] 'neotree-toggle))

(use-package all-the-icons
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

(use-package nyan-mode
  :config
  (progn
    (nyan-mode 1)
    (nyan-start-animation)))

(use-package clojure-mode)

(use-package cider)

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package evil)

(use-package go-mode
  :config
  (add-hook 'go-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'gofmt-before-save)
              (setq tab-width 4)
              (setq indent-tabs-mode 1))))

(use-package intero
  :init
  (add-hook 'haskell-mode-hook 'intero-mode))

(use-package zerodark-theme
  :init
  (load-theme 'zerodark t))

(provide 'init)
