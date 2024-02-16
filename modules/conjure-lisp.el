;;; conjure-lisp.el --- Lisp Languages Initialization
;;; Commentary:
;;; Code:

(setq debugger-bury-or-kill 'kill)

(use-package slime :ensure t)
(setq inferior-lisp-program "sbcl")

(require 'smartparens-config)
(sp-local-pair '(emacs-lisp-mode lisp-data-mode) "'" nil :actions nil)
(sp-local-pair 'emacs-lisp-mode "`" "'" :when '(sp-in-string-p))

(defun conjure-lisp-coding-defaults ()
  "Sensible defaults for `org-mode'"
  (smartparens-strict-mode +1))

(setq conjure-lisp-coding-hook 'conjure-lisp-coding-defaults)

;; REPL programming
(defun conjure-interactive-lisp-coding-defaults ()
  "Defaults for interactive Lisp (REPL) buffers."
  (smartparens-strict-mode +1)
  (whitespace-mode -1))

(setq conjure-interactive-lisp-coding-hook
      'conjure-interactive-lisp-coding-defaults)

(add-hook 'lisp-data-mode-hook
          (lambda () (run-hooks 'conjure-lisp-coding-hook)))

(provide 'conjure-lisp)
;;; conjure-lisp.el ends here
