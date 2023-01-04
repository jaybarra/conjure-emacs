;;; conjure-python.el --- Python configuration
;;; Commentary:
;;; Code:
(conjure-require-packages '(lsp-mode))
(setq python-shell-interpreter "/usr/local/bin/python3"
      lsp-pyls-server-command "/Users/jbarra/Library/Python/3.10/bin/pylsp")

(add-hook 'python-mode-hook 'lsp-deferred)

(provide 'conjure-python)
;;; conjure-python.el ends here
