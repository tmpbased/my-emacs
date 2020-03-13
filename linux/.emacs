(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(lispy-use-sly t))

;; Disable the menu bar.
(menu-bar-mode -1)
;; Disable the scrollbar.
(toggle-scroll-bar -1)
;; Disable the toolbar.
(tool-bar-mode -1)

;; use-package: a package configuration macro
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'bind-key)

(require 'server)
(unless (server-running-p)
  (server-start))

(use-package diminish
  :ensure t)

(use-package company
  :ensure t
  :config (global-company-mode))

(use-package dired-sidebar
  :bind ("<f8>" . dired-sidebar-toggle-sidebar)
  :ensure t
  :commands (dired-sidebar-toggle-sidebar))

;; Ivy
(use-package ivy
  :ensure t
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t)))

;; Swiper
(use-package swiper
  :ensure t
  :bind ("C-s" . swiper))

;; Counsel
(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)))

;; Projectile
(use-package projectile
  :ensure t
  :bind-keymap (("C-c p" . projectile-command-map))
  :config
  (progn
    (projectile-global-mode)
    (setq projectile-enable-caching t)))

(setq projectile-keymap-prefix (kbd "C-c C-p"))

;; Counsel + Projectile
(use-package counsel-projectile
  :ensure t
  :config (counsel-projectile-mode))

;; Magit
(use-package magit
  :ensure t)

;; Sly
(use-package sly
  :ensure t
  ;; :load-path "site-lisp/sly"
  :bind (:map sly-mode-map
	      ("C-<up>" . sly-mrepl-previous-input-or-button)
	      ("C-<down>" . sly-mrepl-next-input-or-button)
	      ("C-<left>" . sly-button-backward)
	      ("C-<right>" . sly-button-forward)
	      ("M-." . sly-edit-definition)) 
  :init
  (progn
    (setq inferior-lisp-program "/usr/bin/sbcl")))

;; Lispy
(use-package lispy
  :ensure t
  ;; :load-path "site-lisp/lispy"
  :bind (:map lispy-mode-map
	      ("M-." . nil))
  :init
  (progn
    (global-set-key
     (kbd "C-c )")
     (lambda ()
       (interactive)
       (if (bound-and-true-p lispy-mode)
	   (progn
	     (lispy-mode 0)
	     (insert ")")
	     (lispy-mode 1))
	 (insert ")"))))
    (let ((it (lambda () (lispy-mode 1))))
      (add-hook 'lisp-mode-hook it)
      (add-hook 'emacs-lisp-mode-hook it)
      (add-hook 'sly-mode-hook it))
    (setq lispy-colon-p nil)))

(use-package lispyville
  :ensure t
  :init
  (progn
    (add-hook 'lispy-mode-hook #'lispyville-mode)))

(add-hook 'lisp-mode-hook (lambda () (show-paren-mode t)))
(add-hook 'emacs-lisp-mode-hook (lambda () (show-paren-mode t)))
(add-hook 'sly-lisp-mode-hook (lambda () (show-paren-mode t)))

;; easy-escape
(use-package easy-escape
  :ensure t
  :init
  (progn
    (add-hook 'lisp-mode-hook 'easy-escape-minor-mode)
    (add-hook 'emacs-lisp-mode-hook 'easy-escape-minor-mode)
    (add-hook 'sly-mode-hook 'easy-escape-minor-mode)))

;; expand-region
(use-package expand-region
  :ensure t
  :init
  (progn
    (global-set-key (kbd "C-=") 'er/expand-region)))

;; aggressive-indent-mode
(use-package aggressive-indent
  :ensure t
  :init
  (progn
    (add-hook 'lisp-mode-hook #'aggressive-indent-mode)
    (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
    (add-hook 'sly-mode-hook #'aggressive-indent-mode)))

(use-package undo-tree
  :ensure t)

(use-package evil
  :ensure t
  :init
  (progn
    (setq evil-want-C-i-jump nil)
    (evil-mode 1)))

(use-package drag-stuff
  :ensure t
  :init
  (progn
    (drag-stuff-global-mode 1)
    (drag-stuff-define-keys)))

(use-package amx
  :ensure t) 

;; https://www.emacswiki.org/emacs/SetFonts
(let ((font "Hack-10"))
  (add-to-list 'default-frame-alist `(font . ,font))
  (set-face-attribute 'default t :font font))
