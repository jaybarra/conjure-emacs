;;; conjure-modules.el --- Conjure Personal Modules Configuration
;;; Commentary:
;;; Code:

;; utilities

(require 'conjure-vertico)

;; Modules for common languages

(require 'conjure-c)
(require 'conjure-clojure)
(require 'conjure-emacs-lisp)
(require 'conjure-javascript)
(require 'conjure-lisp)

(setq-default initial-scratch-message
	      (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you!\n\n"))

(provide 'conjure-modules)

;;; conjure-modules.el ends here
