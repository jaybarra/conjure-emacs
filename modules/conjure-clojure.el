;;; conjure-clojure.el --- Clojure(script) Initialization
;;; Commentary:
;;; Code:
(conjure-require-packages '(clojure-mode
			    cider
			    flymake-kondor))

(require 'conjure-lisp)
(require 'clojure-mode)

(require 'smartparens-config)
(sp-local-pair '(clojure-mode) "'" "'" :actions nil)

(defun conjure-clojure-mode-defaults ()
  "Configure sensible defaults for `clojure-mode'."

  ;;(flymake-kondor-setup)

  (run-hooks 'conjure-lisp-coding-hook))

(setq conjure-clojure-mode-hook 'conjure-clojure-mode-defaults)

(add-hook 'clojure-mode-hook (lambda() (run-hooks 'conjure-clojure-mode-hook)))

(with-eval-after-load 'cider
  (setq nrepl-log-messages nil
        nrepl-hide-special-buffers t
        cider-repl-display-help-banner nil
	cider-connection-message-fn nil
        cider-repl-result-prefix ";; => "
	cider-repl-buffer-size-limit 10000
	cider-print-fn 'fipp
	cider-print-options '(("print-length" 100)))

  (add-hook 'cider-mode-hook 'eldoc-mode)

  (defun conjure-cider-repl-mode-defaults ()
    "Setup defaults for when `cider' loads."
    (subword-mode +1)
    (run-hooks 'conjure-interactive-lisp-coding-hook))
  
  (setq conjure-cider-repl-mode-hook 'conjure-cider-repl-mode-defaults)

  (add-hook 'cider-repl-mode-hook
            (lambda ()
              (run-hooks 'conjure-cider-repl-mode-hook))))

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

  ;; Midje testing framework
  (fact 1)
  (facts 1))

(provide 'conjure-clojure)
;;; conjure-clojure.el ends here
