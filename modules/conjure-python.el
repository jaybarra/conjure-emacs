;;; conjure-python.el --- Python configuration
;;; Commentary:
;;; Code:
(conjure-require-packages '(anaconda-mode ein pipenv))

(defun conjure-python-mode-defaults ()
  "Defaults for `python'."
  (subword-mode +1)
  (anaconda-mode +1)
  (eldoc-mode +1)
  (pipenv-mode +1)
  (dap-mode +1)
  (dap-ui-mode +1))

(setq conjure-python-mode-hook 'conjure-python-mode-defaults)

(if *is-a-mac*
  (setq python-shell-interpreter "python3")
  (setq python-shell-interpreter "python"))

(add-hook 'python-mode-hook (lambda () (run-hooks 'conjure-python-mode-hook)))
(add-hook 'python-mode-hook 'eglot-ensure)

(require 'dap-python)
(setq dap-python-debugger 'debugpy)
(advice-add #'dap-python--pyenv-executable-find
 	    :override
  	    (lambda (command)
	      (pipenv-activate)
              (executable-find python-shell-interpreter)))

(dap-register-debug-template
 "Python :: Django"
 (list :type "python"
       :request "launch"
       :cwd "${workspaceFolder}"
       :program "${workspaceFolder}/manage.py"
       :args "runserver 8088"
       :django t))

(provide 'conjure-python)
;;; conjure-python.el ends here
