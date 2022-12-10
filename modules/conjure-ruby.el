;;; conjure-ruby.el --- Ruby configuration
;;; Commentary:
;;; Code:
(conjure-require-packages '(lsp-mode))

;;(add-hook 'ruby-mode-hook 'eglot-ensure)

(add-hook 'ruby-mode-hook 'lsp-deferred)

(provide 'conjure-ruby)
;;; conjure-ruby.el ends here
