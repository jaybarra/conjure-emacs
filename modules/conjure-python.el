;;; conjure-python.el --- Python configuration
;;; Commentary:
;;; Code:
(setq python-shell-interpreter "/usr/local/bin/python3")

(add-hook 'python-mode-hook 'eglot-ensure)

(provide 'conjure-python)
;;; conjure-python.el ends here
