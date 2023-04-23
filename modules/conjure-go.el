;;; conjure-go.el --- Go configuration
;;; Commentary:
;;; Code:

(require 'conjure-programming)

(conjure-require-packages '(eglot go-mode))

(add-to-list 'completion-ignored-extensions ".test")
(add-to-list 'super-save-predicates
             (lambda () (not (eq major-mode 'go-mode))))

(with-eval-after-load 'go-mode
  (defun conjure-go-mode-defaults ()
    "Sensible defaults for `go-mode'."

    ;; prefer goimports to gofmt
    (let ((goimports (executable-find "goimports")))
      (when goimports
        (setq gofmt-command goimports)))

    (whitespace-toggle-options '(tabs))
    (subword-mode +1)

    (defun eglot-go-install-save-hooks ()
      (add-hook 'before-save-hook #'eglot-format-buffer t t))

    (add-hook 'go-mode-hook 'eglot-go-install-save-hooks)
    (add-hook 'go-mode-hook 'eglot-ensure)))

(provide 'conjure-go)

;;; conjure-go.el ends here
