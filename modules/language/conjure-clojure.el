;;; conjure-clojure.el --- Configurations for Clojure in Conjure -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package cider
  :ensure t
  :defer t
  
  )

(require 'eglot)

(defun conjure--lisp-coding-defaults ()
  (smartparens-strict-mode +1)
  (rainbow-delimiters-mode +1))

(setq conjure--lisp-coding-hook 'conjure--lisp-coding-defaults)

;; interactive modes don't need whitespace checks
(defun conjure--interactive-lisp-coding-defaults ()
  (smartparens-strict-mode +1)
  (rainbow-delimiters-mode +1)
  (whitespace-mode -1))

(setq conjure--interactive-lisp-coding-hook 'conjure--interactive-lisp-coding-defaults)

(with-eval-after-load 'cider
  (setq nrepl-log-messages t)
  (setq cider-print-fn 'puget
        cider-repl-display-help-banner nil
        cider-repl-history-size 100
        cider-repl-history-display-duplicates nil
        cider-repl-history-display-style 'one-line
	cider-repl-display-in-current-window t
	cider-font-lock-dynamically '(macro core function var)
        cider-print-options '(("print-length" 100))
        cider-repl-pop-to-buffer-on-connect 'display-only)

  (add-hook 'cider-mode-hook 'eldoc-mode)

  (defun conjure--cider-repl-mode-defaults ()
    (subword-mode +1)
    (run-hooks 'conjure--interactive-lisp-coding-hook))
  (add-hook 'cider-repl-mode-hook 'conjure--cider-repl-mode-defaults)

  (defun conjure--cider-format-on-save ()
    (when (or (derived-mode-p 'clojure-ts-mode)
              (derived-mode-p 'clojure-mode))
      (add-hook 'before-save-hook 'cider-format-buffer nil 'local)))
  (add-hook 'cider-mode-hook 'conjure--cider-format-on-save))

(use-package clojure-ts-mode
  :ensure t
  :config
  (let ((clojure-ls-path (executable-find "clojure-lsp")))
    (when clojure-ls-path
      (add-to-list 'eglot-server-programs
                   '(clojure-ts-mode "clojure-lsp"))))

  (defun conjure--clojure-mode-defaults ()
    (subword-mode +1)
    (run-hooks 'conjure--lisp-coding-hook))
  (add-hook 'clojure-ts-mode-hook 'conjure--clojure-mode-defaults)

  (add-to-list 'major-mode-remap-alist
               '(clojure-mode . clojure-ts-mode)))

(provide 'conjure-clojure)
;;; conjure-clojure.el ends here
