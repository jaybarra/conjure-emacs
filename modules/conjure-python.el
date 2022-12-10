;;; conjure-python.el --- Python configuration
;;; Commentary:
;;; Code:
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
	       '((python-mode) "/Users/jbarra/Library/Python/3.10/bin/jedi-language-server")))

(setq python-shell-interpreter "/usr/local/bin/python3")

(add-hook 'python-mode-hook 'eglot-ensure)

(provide 'conjure-python)
;;; conjure-python.el ends here
