;;; conjure-js.el --- JavaScript configuration
;;; Commentary:
;;; Code:
(conjure-require-packages '(rjsx-mode flymake-eslint))

(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . rjsx-mode))

(add-hook 'rjsx-mode-hook 'eglot-ensure)
(add-hook 'js-mode-hook 'eglot-ensure)

(require 'flymake-eslint)
(add-hook 'js-mode-hook (lambda () (flymake-eslint-enable)))
(add-hook 'web-mode-hook (lambda () (flymake-eslint-enable)))
(add-hook 'rjsx-mode-hook (lambda () (flymake-eslint-enable)))

(provide 'conjure-js)
;;; conjure-js.el ends here
