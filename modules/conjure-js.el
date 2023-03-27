;;; conjure-js.el --- JavaScript configuration
;;; Commentary:
;;; Code:
(require 'conjure-packages)
(conjure-require-packages '(js2-mode rjsx-mode))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))

(add-hook 'js2-mode-hook 'eglot-ensure)
(add-hook 'rjsx-mode-hook 'eglot-ensure)

(provide 'conjure-js)
;;; conjure-js.el ends here
