;;; conjure-lisp.el --- Lisp Languages Initialization
;;; Commentary:
;;; Code:

(require 'conjure-programming)

(setq debugger-bury-or-kill 'kill)

(require 'smartparens-config)
;; disable single quote pairing to allow for lists '()
(sp-local-pair '(emacs-lisp-mode lisp-data-mode) "'" "'" :actions nil)

(defun conjure-lisp-coding-defaults ()
  "Sensible defaults for Lispy languages."
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
