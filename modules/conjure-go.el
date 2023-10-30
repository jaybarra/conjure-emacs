;;; conjure-go.el --- Go configuration
;;; Commentary:
;;; Code:

(require 'conjure-programming)
(require 'eglot)

(use-package go-mode
  :mode "\\.go\\'"
  :hook ((before-save . gofmt-before-save)
	 (go-mode . eglot-ensure))
  :config
  (let ((goimports (executable-find "goimports")))
    (when goimports (setq gofmt-command goimports))))

(provide 'conjure-go)

;;; conjure-go.el ends here
