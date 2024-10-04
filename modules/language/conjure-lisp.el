;;; conjure-lisp.el --- Configurations for Lisp in Conjure -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package slime
  :config
  (setq inferior-lisp-program "sbcl"))

(provide 'conjure-lisp)
;;; conjure-lisp.el ends here

