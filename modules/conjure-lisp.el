;;; conjure-lisp.el --- Lisp Languages Initialization
;;; Commentary:
;;; Code:
(conjure-require-packages '(rainbow-delimiters))

(setq debugger-bury-or-kill 'kill)

(setq-default initial-scratch-message
	      (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you!\n\n"))

(require 'smartparens-config)
(defun conjure-lisp-coding-defaults ()
  "Sensible defaults for Lisp languages."

  (run-hooks 'conjure-prog-mode-defaults)
  
  (smartparens-strict-mode +1)
  (diminish 'smartparens-mode)
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
