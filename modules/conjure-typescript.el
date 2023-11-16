;;; conjure-typescript.el --- TypeScript configuration
;;; Commentary:
;;; Code:

(require 'conjure-programming)

(add-hook 'typescript-ts-mode-hook 'eglot-ensure)
(add-hook 'tsx-ts-mode-hook 'eglot-ensure)

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))

(use-package apheleia
  :config
  (add-to-list 'apheleia-mode-alist '(tsx-ts-mode . prettier-typescript))
  (add-to-list 'apheleia-mode-alist ' (typescript-ts-mode . prettier-typescript)))

(provide 'conjure-typescript)
;;; conjure-typescript.el ends here
