;;; conjure-modules.el --- Conjure Sample Personal Modules Configuration
;;; Commentary:
;;; Code:

;; Narrowing frameworks (should use one)
;; (require 'conjure-helm)
;; (require 'conjure-ivy)
(require 'conjure-vertico)

;; Utilities
(require 'conjure-datatypes) ;; define custom type-mappings in here
(require 'conjure-org)

;; Modules for common languages
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

(provide 'conjure-modules)

;;; conjure-modules.el ends here
