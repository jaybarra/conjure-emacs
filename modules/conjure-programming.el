;;; conjure-programming.el --- Configurations for Programming with Conjure
;;; Commentary:
;;; Code:

(require 'eglot)

(with-eval-after-load 'eglot
  (setq eglot-events-buffer-size 0
        eglot-ignored-server-capabilities '(:hoverProvider
                                            :documentHighlightProvider)
        eglot-autoshutdown t))

(set-language-environment 'utf-8)

(use-package rainbow-mode
  :delight
  :hook (web-mode css-mode css-ts-mode))

;; show the current function def in the modeline
(require 'which-func)
(setq which-func-unknown "⊥")
(add-hook 'prog-mode-hook (lambda () (which-function-mode +1)))

;; font-lock annotations like TODO
(use-package hl-todo
  :config
  (global-hl-todo-mode +1))

;; make parens visually different
(use-package rainbow-delimiters
  :hook (prog-mode text-mode))

;; flycheck style commands for flymake
(define-key prog-mode-map (kbd "C-c ! n") 'flymake-goto-next-error)
(define-key prog-mode-map (kbd "C-c ! p") 'flymake-goto-prev-error)
(define-key prog-mode-map (kbd "C-c ! c") 'flymake-start)

(define-key text-mode-map (kbd "C-c ! n") 'flymake-goto-next-error)
(define-key text-mode-map (kbd "C-c ! p") 'flymake-goto-prev-error)
(define-key text-mode-map (kbd "C-c ! c") 'flymake-start)

(use-package pulsar
  :config
  (add-to-list 'pulsar-pulse-functions 'flymake-goto-next-error)
  (add-to-list 'pulsar-pulse-functions 'flymake-goto-prev-error))

;; formatting
(use-package apheleia
  :delight
  :hook prog-mode)

(defun conjure-prog-mode-defaults ()
  "Sensible defaults for `prog-mode'."
  (setq-local display-line-numbers 'relative))

(add-hook 'prog-mode-hook #'conjure-prog-mode-defaults)

;; bug references
(add-hook 'prog-mode-hook #'bug-reference-prog-mode)

(use-package markdown-mode)

(use-package tempel)

;; disable liagures when merging
(add-hook 'smerge-mode-hook (lambda () (ligature-mode -1)))

(provide 'conjure-programming)

;;; conjure-programming.el ends here
