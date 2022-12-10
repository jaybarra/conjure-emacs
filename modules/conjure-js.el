;;; conjure-js.el --- JavaScript configuration
;;; Commentary:
;;; Code:
(conjure-require-packages '(rjsx-mode
                            eglot))

(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . rjsx-mode))

(add-hook 'rjsx-mode-hook 'eglot-ensure)
(add-hook 'js-mode-hook 'eglot-ensure)

(provide 'conjure-js)
;;; conjure-js.el ends here
