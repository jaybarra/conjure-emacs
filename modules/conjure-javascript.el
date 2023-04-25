;;; conjure-javascript.el --- JavaScript configuration
;;; Commentary:
;;; Code:

(require 'conjure-programming)

(conjure-require-packages '(eglot prettier))

(add-hook 'js-mode-hook 'eglot-ensure)
(add-hook 'js-mode-hook 'prettier-mode)

(provide 'conjure-javascript)

;;; conjure-javascript.el ends here
