;;; conjure-python.el --- Python configuration
;;; Commentary:
;;; Code:

(use-package exec-path-from-shell :ensure t)

(use-package python
  :ensure nil
  :config
  (when (and (executable-find "python3")
             (string= python-shell-interpreter "python"))
    (setq python-shell-interpreter "python3")))

(use-package pyvenv
  :ensure t
  :after python
  :config
  (add-to-list 'global-mode-string
               '(pyvenv-virtual-env-name (" venv:" pyvenv-virtual-env-name " "))
               'append))

(use-package poetry
  :ensure t
  :after python
  :init
  (setq poetry-tracking-strategy 'switch-buffer)
  (add-hook 'python-mode-hook #'poetry-tracking-mode))

(provide 'conjure-python)
;;; conjure-python.el ends here
