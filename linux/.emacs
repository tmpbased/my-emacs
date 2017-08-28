(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)
(package-refresh-contents)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (parinfer lispy ac-slime slime eclim counsel-projectile counsel swiper ivy neotree auto-complete projectile intero magit heap geiser))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; use-package: a package configuration macro
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; Intero
(use-package intero
  :ensure t
  :init
  (add-hook 'haskell-mode-hook 'intero-mode))

;; Erlang mode
(setq load-path (cons  "/usr/lib/erlang/lib/tools-2.10/emacs" load-path))
(setq erlang-root-dir "/usr/lib/erlang")
(setq exec-path (cons "/usr/lib/erlang/bin" exec-path))
(require 'erlang-start)

;; Neotree
(use-package neotree
  :ensure t
  :bind ("<f8>" . neotree-toggle)
  :init
  (setq-default neo-show-hidden-files t))

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
  :config
  (progn
    (projectile-global-mode)
    (setq projectile-enable-caching t)))

;; Counsel + Projectile
(use-package counsel-projectile
  :ensure t
  :config (counsel-projectile-on))

;; Magit
(use-package magit
  :ensure t)

;; Slime
(use-package slime
  :ensure t
  :init
  (progn
    (setq inferior-lisp-program "/usr/bin/sbcl")
    (setq slime-contribs '(slime-fancy))))

;; auto-complete
(global-auto-complete-mode t)
(auto-complete-mode t)

;; ac-slime
(use-package ac-slime
  :ensure t
  :init
  (progn
    (add-hook 'slime-mode-hook 'set-up-slime-ac)
    (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
    (eval-after-load "auto-complete"
      '(add-to-list 'ac-modes 'slime-repl-mode))))

;; Lispy
(use-package lispy
  :ensure t)

;; Parinfer
(use-package parinfer
  :ensure t
  :bind
  (("C-," . parinfer-toggle-mode))
  :init
  (progn
    (setq parinfer-extensions
          '(defaults       ; should be included.
	     pretty-parens  ; different paren styles for different modes.
	     evil           ; If you use Evil.
	     lispy          ; If you use Lispy. With this extension, you should install Lispy and do not enable lispy-mode directly.
	     paredit        ; Introduce some paredit commands.
	     smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
	     smart-yank))   ; Yank behavior depend on mode.
    (add-hook 'clojure-mode-hook #'parinfer-mode)
    (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
    (add-hook 'common-lisp-mode-hook #'parinfer-mode)
    (add-hook 'scheme-mode-hook #'parinfer-mode)
    (add-hook 'lisp-mode-hook #'parinfer-mode)))

;; Make keyboard shortcuts work for non-english keyboard layout.
;; Source: http://reangdblog.blogspot.com/2015/05/emacs.html
(defun reverse-input-method (input-method)
  "Build the reverse mapping of single letters from INPUT-METHOD."
  (interactive
   (list (read-input-method-name "Use input method (default current): ")))
  (if (and input-method (symbolp input-method))
      (setq input-method (symbol-name input-method)))
  (let ((current current-input-method)
        (modifiers '(nil (control) (meta) (control meta))))
    (when input-method
      (activate-input-method input-method))
    (when (and current-input-method quail-keyboard-layout)
      (dolist (map (cdr (quail-map)))
        (let* ((to (car map))
               (from (quail-get-translation
                      (cadr map) (char-to-string to) 1)))
          (when (and (characterp from) (characterp to))
            (dolist (mod modifiers)
              (define-key local-function-key-map
                (vector (append mod (list from)))
                (vector (append mod (list to)))))))))
    (when input-method
      (activate-input-method current))))

(reverse-input-method 'ukrainian-computer)
