;;; conjure-typescript.el --- TypeScript configuration
;;; Commentary:
;;; Code:

(require 'conjure-programming)

(conjure-require-packages '(eglot prettier typescript-mode))

(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-mode))

(require 'typescript-mode)
(setq typescript-indent-level 2)

(add-hook 'typescript-mode-hook 'prettier-mode)
(add-hook 'typescript-mode-hook 'eglot-ensure)

(require 'projectile)
(projectile-register-project-type 'npm-spec '("package.json")
				  :project-file "package.json"
				  :compile "npm install"
                                  :test "npm test"
				  :test-suffix ".spec")
(provide 'conjure-typescript)
;;; conjure-typescript.el ends here
