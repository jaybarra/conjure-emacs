;;; conjure-clojure.el --- Clojure(script) Initialization
;;; Commentary:
;;; Code:
(conjure-require-packages '(clojure-mode
			    cider))

(require 'conjure-lisp)
(require 'clojure-mode)

(defun conjure-clojure-mode-defaults ()
  "Setup sensible defaults for `clojure-mode'."
  (subword-mode +1)
  (run-hooks 'conjure-lisp-coding-hook))

(setq conjure-clojure-mode-hook 'conjure-clojure-mode-defaults)

(add-hook 'clojure-mode-hook (lambda () (run-hooks 'conjure-clojure-mode-hook)))

(require 'smartparens-config)
(sp-local-pair '(clojure-mode) "'" "'" :actions nil)

(with-eval-after-load 'cider
  (setq nrepl-log-messages nil
        nrepl-hide-special-buffers t
        cider-repl-display-help-banner nil
	cider-connection-message-fn nil
        cider-repl-result-prefix ";; => "
	cider-repl-buffer-size-limit 100000
        ;;cider-print-fn 'puget
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

;; handle Compojure formatting as specified
;; https://github.com/weavejester/compojure/wiki/Emacs-indentation
(define-clojure-indent
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
  (context 2))

(provide 'conjure-clojure)
;;; conjure-clojure.el ends here
