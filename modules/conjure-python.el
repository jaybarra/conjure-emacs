;;; conjure-python.el --- Python configuration
;;; Commentary:
;;; Code:
(conjure-require-packages '(anaconda-mode ein pipenv))

(defun conjure-python-mode-defaults ()
  "Sensible defaults for `python-mode'."
  (interactive)
  (subword-mode +1)
  (eldoc-mode +1)
  (anaconda-mode +1)
  
  (setq python-shell-interpreter "jupyter"
        python-shell-interpreter-args "console --simple-prompt"
	python-shell-prompt-detect-failure-warning nil)
  
  (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter"))

(add-hook 'python-mode-hook 'conjure-python-mode-defaults)

(with-eval-after-load 'dap-mode
  (dap-register-debug-template
   "Python :: Django"
   (list :type "python"
	 :request "launch"
	 :cwd "${workspaceFolder}"
	 :program "${workspaceFolder}/manage.py"
	 :args "runserver 4000"
	 :django t)))

(provide 'conjure-python)
;;; conjure-python.el ends here
