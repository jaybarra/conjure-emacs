;;; conjure-ts.el --- TypeScript configuration
;;; Commentary:
;;; Code:
(conjure-require-packages '(typescript-mode
                            eglot))

(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-mode))

(setq typescript-indent-level 2)

(add-hook 'typescript-mode-hook 'eglot-ensure)

(provide 'conjure-ts)
;;; conjure-ts.el ends here
