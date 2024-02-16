;;; conjure-clojure.el --- Clojure Initialization
;;; Commentary:
;;; Code:

(use-package clojure-mode
  :ensure t
  :defer t
  :after smartparens
  :config
  (sp-local-pair '(clojure-mode) "'" nil :actions nil)

  (defun conjure-clojure-mode-defaults ()
    "Sensible defaults for `clojure-mode'."
    (subword-mode +1)
    (smartparens-strict-mode +1)
    (local-unset-key (kbd "C-:")))

  (add-hook 'clojure-mode-hook #'conjure-clojure-mode-defaults)
  
  (define-clojure-indent
   ;; Compojure defaults
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

(use-package clojure-mode-extra-font-locking
  :ensure t
  :after clojure-mode)

(use-package cider
  :ensure t
  :after clojure-mode
  :config
  (setq cider-print-fn 'fipp
        cider-repl-display-help-banner nil
        cider-repl-result-prefix ";; =>"
        cider-print-options '(("print-length" 100))))

(provide 'conjure-clojure)

;;; conjure-clojure.el ends here
