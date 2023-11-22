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
  ;; disable show-smartparens because of tsx problems with `<></>' pairs,
  (show-smartparens-mode -1)
  (subword-mode +1))

(setq conjure-typescript-mode-hook #'conjure-typescript-mode-defaults)

(add-hook 'tsx-ts-mode-hook (lambda () (run-hooks 'conjure-typescript-mode-hook)))
(add-hook 'typescript-ts-mode-hook (lambda () (run-hooks 'conjure-typescript-mode-hook)))

(provide 'conjure-typescript)
;;; conjure-typescript.el ends here
