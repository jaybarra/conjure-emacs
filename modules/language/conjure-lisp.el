;;; conjure-lisp.el --- Configurations for Lisp in Conjure -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "sbcl"))

(provide 'conjure-lisp)
;;; conjure-lisp.el ends here

