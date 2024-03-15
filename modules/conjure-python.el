;;; conjure-python.el --- Python configuration
;;; Commentary:
;;; Code:

(use-package exec-path-from-shell
  :ensure t)

(setq python-shell-interpreter "python3")

(use-package pyvenv
  :ensure t
  :config
  (defun conjure/pyvenv-autoload()
    "Automatically activate pyvenv if a .venv folder exists."
    (let ((venv-dir (locate-dominating-file (buffer-file-name) ".venv")))
      (when venv-dir
	    (pyvenv-activate (concat venv-dir ".venv")))))
  
  (add-hook 'python-mode-hook 'conjure/pyvenv-autoload))

(add-hook 'python-mode-hook #'eglot-ensure)
(add-hook 'python-ts-mode-hook #'eglot-ensure)

(provide 'conjure-python)

;;; conjure-python.el ends here
