;;; conjure-ts.el --- TypeScript configuration
;;; Commentary:
;;; Code:
(require 'conjure-packages)

(conjure-require-packages '(typescript-mode))

(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-mode))

(require 'typescript-mode)
(setq typescript-indent-level 2)

(add-hook 'typescript-mode-hook #'eglot-ensure)

(require 'projectile)
(projectile-register-project-type 'npm-spec '("package.json")
				  :project-file "package.json"
				  :compile "npm install"
                                  :test "npm test"
				  :test-suffix ".spec")
(provide 'conjure-ts)
;;; conjure-ts.el ends here
