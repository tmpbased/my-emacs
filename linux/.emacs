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
 '(package-selected-packages (quote (auto-complete projectile intero magit heap geiser))))
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
(add-to-list 'load-path "/home/evgeniy/build/emacs-neotree")
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;; Projectile
(global-set-key [C-f8] 'projectile-mode)
