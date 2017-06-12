;; MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(package-refresh-contents)

;; Intero
(package-install 'intero)
(add-hook 'haskell-mode-hook 'intero-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (slime eclim counsel-projectile counsel swiper ivy neotree auto-complete projectile intero magit heap geiser))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Erlang mode
(setq load-path (cons  "/usr/lib/erlang/lib/tools-2.10/emacs" load-path))
(setq erlang-root-dir "/usr/lib/erlang")
(setq exec-path (cons "/usr/lib/erlang/bin" exec-path))
(require 'erlang-start)

;; Neotree
(package-install 'neotree)
(setq-default neo-show-hidden-files t)
(global-set-key [f8] 'neotree-toggle)

;; Ivy
(package-install 'ivy)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(package-install 'swiper)
(global-set-key "\C-s" 'swiper)
(package-install 'counsel)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)

;; Projectile
(package-install 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching t)

;; Counsel + Projectile
(package-install 'counsel-projectile)
(counsel-projectile-on)

;; Magit
(package-install 'magit)

;; Slime
(package-install 'slime)
(setq inferior-lisp-program "/usr/bin/sbcl")
(setq slime-contribs '(slime-fancy))
