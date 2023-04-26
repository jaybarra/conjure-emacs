;;; conjure-modules.el --- Conjure Personal Modules Configuration
;;; Commentary:
;;; Code:

;; Narrowing frameworks (should use one)
;; (require 'conjure-helm)
;; (require 'conjure-ivy)
(require 'conjure-vertico)

;; Utilities
(require 'conjure-datatypes)
(require 'conjure-org)

;; Modules for common languages
(require 'conjure-c)
(require 'conjure-clojure)
(require 'conjure-elixir)
(require 'conjure-emacs-lisp)
(require 'conjure-javascript)
(require 'conjure-go)
(require 'conjure-lisp)
(require 'conjure-markdown)
(require 'conjure-python)
(require 'conjure-ruby)
(require 'conjure-rust)
(require 'conjure-svelte)
(require 'conjure-terraform)
(require 'conjure-typescript)
(require 'conjure-vertico)
(require 'conjure-web)
(require 'conjure-yaml)

(setq-default initial-scratch-message
	      (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you!\n\n"))

(setq conjure-flyspell nil)

(ef-themes-select 'ef-dark)

(provide 'conjure-modules)

;;; conjure-modules.el ends here
