;;; conjure-javascript.el --- JavaScript configuration
;;; Commentary:
;;; Code:

(require 'conjure-programming)

(conjure-require-packages '(eglot js2-mode prettier rjsx-mode))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))

(add-hook 'js2-mode-hook 'eglot-ensure)
(add-hook 'js2-mode-hook 'prettier-mode)

(add-hook 'rjsx-mode-hook 'eglot-ensure)

(provide 'conjure-javascript)
;;; conjure-javascript.el ends here
