;;; conjure-python.el --- Python configuration
;;; Commentary:
;;; Code:

(require 'conjure-programming)

(use-package exec-path-from-shell)
(use-package pyvenv-auto)

(use-package python
  :straight nil
  :config
  (exec-path-from-shell-initialize)
  (setq python-interpreter "ipython"))

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))
(add-hook 'python-ts-mode-hook #'eglot-ensure)

(with-eval-after-load 'projectile
  (projectile-register-project-type 'poetry '("pyproject.toml")
                                    :project-file "pyproject.toml"
				    :compile "poetry install"
				    :test "poetry run pytest"
				    :test-prefix "test_"))

(provide 'conjure-python)
;;; conjure-python.el ends here
