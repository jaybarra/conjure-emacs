;;; conjure-typescript.el --- TypeScript configuration
;;; Commentary:
;;; Code:

(require 'conjure-programming)

(add-hook 'typescript-ts-mode-hook 'eglot-ensure)
(add-hook 'tsx-ts-mode-hook 'eglot-ensure)

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))

(defun conjure-typescript-mode-defaults ()
  "Sensible defaults for working `typescript'."
  ;; nothing here yet
  
  )
(setq conjure-typescript-mode-hook #'conjure-typescript-mode-defaults)

(defun conjure-jsx-tsx-mode-defaults ()
  "Sensible defaults for working `typescript'."
  ;; disable for TSX/JSX because smart-parens does not handle empty `<></>' cleanly
  (turn-off-show-smartparens-mode))

(setq conjure-tsx-jsx-mode-hook #'conjure-jsx-tsx-mode-defaults)

(add-hook 'tsx-ts-mode-hook (lambda () (run-hooks 'conjure-typescript-mode-hook 'conjure-tsx-jsx-mode-hook)))
(add-hook 'typescript-ts-mode-hook (lambda () (run-hooks 'conjure-typescript-mode-hook)))

(provide 'conjure-typescript)
;;; conjure-typescript.el ends here
