;;; conjure-typescript.el --- TypeScript configuration
;;; Commentary:
;;; Code:

(require 'conjure-programming)

(use-package typescript-ts-mode
  :mode (("\\.ts\\'" . typescript-ts-mode)
	 ("\\.tsx\\'" . tsx-ts-mode))
  :hook ((typescript-ts-mode tsx-ts-mode) . eglot-ensure))

(provide 'conjure-typescript)
;;; conjure-typescript.el ends here
