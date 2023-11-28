;;; conjure-clojure.el --- Clojure(script) Initialization
;;; Commentary:
;;; Code:

(require 'conjure-lisp)

(use-package clojure-mode
  :config
  ;; disable single quote pairing to allow for lists '()
  (sp-local-pair '(clojure-mode) "'" nil :actions nil)

  (defun conjure-clojure-mode-defaults ()
    "Configure sensible defaults for `clojure-mode'."

    ;; flymake and cider don't talk to each yet
    (flymake-mode -1)

    ;; add a formatter that doesn't rely on a REPL connection
    ;; but generally prefer `cider-format-buffer' when possible
    (push '(clojure-cljfmt . ("clj"
			      "-Sdeps" (format "{:deps {cljfmt/cljfmt {:mvn/version \"RELEASE\"} } }")
			      "-M" "-m" "cljfmt.main" "fix"
			      filepath))
	  apheleia-formatters)

    (add-to-list 'apheleia-mode-alist '((clojure-mode . clojure-cljfmt)))

    ;; run general lisp hooks
    (subword-mode +1)
    (run-hooks 'conjure-lisp-coding-hook))

  (setq conjure-clojure-mode-hook 'conjure-clojure-mode-defaults)

  (add-hook 'clojure-mode-hook
            (lambda () (run-hooks 'conjure-clojure-mode-hook)))

  (define-clojure-indent
   ;; Compojure
   ;; https://github.com/weavejester/compojure/wiki/Emacs-indentation
   (defroutes 'defun)
   (GET 2)
   (POST 2)
   (PUT 2)
   (DELETE 2)
   (HEAD 2)
   (ANY 2)
   (OPTIONS 2)
   (PATCH 2)
   (rfn 2)
   (let-routes 1)
   (context 2)

   ;; Midje testing
   ;; https://github.com/marick/Midje
   (fact 1)
   (facts 1)))

(use-package cider
  :config
  (setq nrepl-log-messages t
        nrepl-hide-special-buffers t
        cider-repl-display-help-banner nil
	cider-connection-message-fn nil
        cider-repl-result-prefix ";; => "
        cider-result-overlay-position 'at-eol
        cider-font-lock-max-length 4096
	cider-print-fn 'fipp
        cider-print-options '(("print-length" 100)))

  (defun conjure-cider-repl-mode-defaults ()
    "Setup defaults for when `cider' loads."
    (subword-mode +1)
    (run-hooks 'conjure-interactive-lisp-coding-hook))

  (defvar conjure-cider-repl-mode-hook nil)
  (setq conjure-cider-repl-mode-hook 'conjure-cider-repl-mode-defaults)

  (add-hook 'cider-mode-hook 'eldoc-mode)
  (add-hook 'cider-repl-mode-hook
            (lambda () (run-hooks 'conjure-cider-repl-mode-hook))))

(provide 'conjure-clojure)

;;; conjure-clojure.el ends here
