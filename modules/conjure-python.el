;;; conjure-python.el --- Python configuration
;;; Commentary:
;;; Code:
(conjure-require-packages '(anaconda-mode ein))

(defun conjure-python-mode-defaults ()
  "Defaults for `python'."
  (subword-mode +1)
  (anaconda-mode +1)
  (eldoc-mode +1))

(setq conjure-python-mode-hook 'conjure-python-mode-defaults)

(setq python-shell-interpreter "python3")
(add-hook 'python-mode-hook (lambda () (run-hooks 'conjure-python-mode-hook)))
(add-hook 'python-mode-hook 'eglot-ensure)

(provide 'conjure-python)
;;; conjure-python.el ends here
