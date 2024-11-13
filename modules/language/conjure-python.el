;;; conjure-python.el --- Configurations for Python in Conjure -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package pyvenv :ensure t)

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")

(provide 'conjure-python)
;;; conjure-python.el ends here
