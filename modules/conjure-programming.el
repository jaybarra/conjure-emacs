;;; conjure-programming.el --- Configurations for Programming with Conjure
;;; Commentary:
;;; Code:

(use-package eglot
  :ensure nil
  :commands eglot eglot-ensure
  :init
  (setq eglot-sync-connect 1
        eglot-autoreconnect t
        eglot-auto-display-help-buffer nil)
  :config
  (setq eglot-events-buffer-size 0))

(set-language-environment 'utf-8)

;; show the current function def in the modeline
(require 'which-func)
(setq which-func-unknown "⊥")

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package apheleia
  :ensure t
  :delight
  :hook (prog-mode html-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode text-mode))

;; font-lock annotations like TODO
(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode +1))

;; flycheck style commands for flymake
(define-key prog-mode-map (kbd "C-c ! n") 'flymake-goto-next-error)
(define-key prog-mode-map (kbd "C-c ! p") 'flymake-goto-prev-error)
(define-key prog-mode-map (kbd "C-c ! c") 'flymake-start)

(define-key text-mode-map (kbd "C-c ! n") 'flymake-goto-next-error)
(define-key text-mode-map (kbd "C-c ! p") 'flymake-goto-prev-error)
(define-key text-mode-map (kbd "C-c ! c") 'flymake-start)

(defun conjure-prog-mode-defaults ()
  "Sensible defaults for `prog-mode'."
  (hl-line-mode +1)
  (which-function-mode +1)
  (whitespace-mode -1)

  (setq-local display-line-numbers 'relative))

(add-hook 'prog-mode-hook #'conjure-prog-mode-defaults)

(use-package smartrep :ensure t)
(use-package operate-on-number :ensure t)

(smartrep-define-key global-map "C-c ."
  '(("+" . apply-operation-to-number-at-point)
    ("-" . apply-operation-to-number-at-point)
    ("*" . apply-operation-to-number-at-point)
    ("/" . apply-operation-to-number-at-point)
    ("\\" . apply-operation-to-number-at-point)
    ("^" . apply-operation-to-number-at-point)
    ("<" . apply-operation-to-number-at-point)
    (">" . apply-operation-to-number-at-point)
    ("#" . apply-operation-to-number-at-point)
    ("%" . apply-operation-to-number-at-point)
    ("'" . operate-on-number-at-point)))

(define-key prog-mode-map (kbd "<f7>") 'compile)

(use-package logview
  :ensure t
  :defer t)

(provide 'conjure-programming)
;;; conjure-programming.el ends here
