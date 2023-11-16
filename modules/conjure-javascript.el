;;; conjure-javascript.el --- JavaScript configuration
;;; Commentary:
;;; Code:

(require 'conjure-programming)

(use-package apheleia
  :config
  (add-to-list 'apheleia-mode-alist ' (js-ts-mode . prettier-typescript)))

(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js-ts-mode))
(add-to-list 'major-mode-remap-alist '(javascript-mode . js-ts-mode))

(add-hook 'js-ts-mode-hook 'eglot-ensure)

(add-to-list 'major-mode-remap-alist '(js-json-mode . json-ts-mode))

(provide 'conjure-javascript)

;;; conjure-javascript.el ends here
