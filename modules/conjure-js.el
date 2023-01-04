;;; conjure-js.el --- JavaScript configuration
;;; Commentary:
;;; Code:
(conjure-require-packages '(rjsx-mode))

(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . rjsx-mode))

;;(add-hook 'rjsx-mode-hook 'eglot-ensure)
;;(add-hook 'js-mode-hook 'eglot-ensure)

(add-hook 'rjsx-mode-hook 'lsp-deferred)
(add-hook 'js-mode-hook 'lsp-deferred)

(provide 'conjure-js)
;;; conjure-js.el ends here
