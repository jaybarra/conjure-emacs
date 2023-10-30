;;; conjure-python.el --- Python configuration
;;; Commentary:
;;; Code:

(require 'exec-path-from-shell)

(use-package pyvenv-auto
  :ensure t)

(use-package apheleia
  :ensure t)

(use-package python-mode
  :ensure nil
  :hook
  ((python-mode . python-ts-mode)
   (python-mode . apheleia-mode)
   (python-ts-mode . eglot-ensure))
  :init
  (exec-path-from-shell-initialize)
  (setq python-interpreter "ipython"))

(message "loading python")

(provide 'conjure-python)
;;; conjure-python.el ends here
