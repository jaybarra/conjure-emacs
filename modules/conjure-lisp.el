;;; conjure-lisp.el --- Lisp Languages Initialization
;;; Commentary:
;;; Code:
(conjure-require-packages '(rainbow-delimiters))

(require 'smartparens-config)
(defun conjure-lisp-coding-defaults ()
  "Sensible defaults for Lisp languages."
  (smartparens-strict-mode +1)
  (rainbow-delimiters-mode +1))

(setq conjure-lisp-coding-hook 'conjure-lisp-coding-defaults)

(defun conjure-interactive-lisp-coding-defaults ()
  "Defaults for interactive Lisp (REPL) buffers."
  (rainbow-delimiters-mode +1)
  (whitespace-mode -1))

(setq conjure-interactive-lisp-coding-hook 'conjure-interactive-lisp-coding-defaults)

(add-hook 'lisp-data-mode-hook (lambda ()
                                 (run-hooks 'conjure-lisp-coding-hook)))

(provide 'conjure-lisp)
;;; conjure-lisp.el ends here
