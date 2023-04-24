;;; conjure-clojure.el --- Clojure(script) Initialization
;;; Commentary:
;;; Code:

(require 'conjure-lisp)
(conjure-require-packages '(cider clojure-mode))

(with-eval-after-load 'clojure-mode
  (require 'smartparens-config)
  ;; disable single quote pairing to allow for lists '()
  (sp-local-pair '(clojure-mode) "'" "'" :actions nil)

  (defun conjure-clojure-mode-defaults ()
    "Configure sensible defaults for `clojure-mode'."
    (subword-mode +1)
    (run-hooks 'conjure-lisp-coding-hook))

  (setq conjure-clojure-mode-hook 'conjure-clojure-mode-defaults)

  (add-hook 'clojure-mode-hook
            (lambda ()
              (run-hooks 'conjure-clojure-mode-hook)))

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
    (facts 1))

  (defun clerk-show ()
    "Start Clerk."
    (interactive)
    (when-let
        ((filename
          (buffer-file-name)))
      (save-buffer)
      (cider-interactive-eval
       (concat "(nextjournal.clerk/show! \"" filename "\")"))))

  (define-key clojure-mode-map (kbd "<M-return>") 'clerk-show))

(with-eval-after-load 'cider
  (setq nrepl-log-messages t
        nrepl-hide-special-buffers t
        cider-repl-display-help-banner nil
	cider-connection-message-fn nil
        cider-repl-result-prefix ";; => "
	cider-repl-buffer-size-limit 8192
	cider-test-show-report-on-success nil
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
