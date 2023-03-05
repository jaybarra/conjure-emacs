;;; conjure-ts.el --- TypeScript configuration
;;; Commentary:
;;; Code:
(conjure-require-packages '(typescript-mode flymake-eslint))

(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-mode))

(setq typescript-indent-level 2)

(add-hook 'typescript-mode-hook #'eglot-ensure)

(require 'flymake-eslint)
(when (executable-find "eslint")
  (add-hook 'typescript-mode-hook (lambda () (flymake-eslint-enable))))

(projectile-register-project-type 'npm-spec '("package.json")
				  :project-file "package.json"
				  :compile "npm install"
                                  :test "npm test"
				  :test-suffix ".spec")

(provide 'conjure-ts)
;;; conjure-ts.el ends here
