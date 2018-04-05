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
 '(lispy-use-sly t))

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

;; Sly
(use-package sly
  :ensure t
  :bind (:map sly-mode-map
	      ("C-<up>" . sly-mrepl-previous-input-or-button)
	      ("C-<down>" . sly-mrepl-next-input-or-button)
	      ("C-<left>" . sly-button-backward)
	      ("C-<right>" . sly-button-forward)
	      ("M-." . sly-edit-definition)) 
  :init
  (progn
    (setq inferior-lisp-program "/usr/bin/sbcl")))

;; auto-complete
;; (use-package auto-complete
;;   :ensure t
;;   :init
;;   (progn
;;     (add-hook 'lisp-mode-hook (lambda () (auto-complete-mode t)))
;;     (add-hook 'emacs-lisp-mode-hook (lambda () (auto-complete-mode t)))
;;     (add-hook 'sly-mode-hook (lambda () (auto-complete-mode t)))))

;; ac-sly
;; (use-package ac-sly
;;   :ensure t
;;   :init
;;   (progn
;;     (add-hook 'sly-mode-hook 'set-up-sly-ac)
;;     (eval-after-load 'auto-complete
;;       '(add-to-list 'ac-modes 'sly-mrepl-mode))))

(use-package sly-company
  :ensure t
  :init
  (progn
    (add-hook 'sly-mode-hook 'sly-company-mode)
    (add-to-list 'company-backends 'sly-company)))

;; Lispy
(use-package lispy
  :ensure t
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
      (add-hook 'sly-mode-hook it))))

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

(use-package evil
  :ensure t
  :init
  (progn
    (evil-mode 1)))

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
