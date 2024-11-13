;;; conjure-csharp.el --- Configurations for Csharp in Conjure -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.cshtml?\\'" . web-mode))
  (setq web-mode-engines-alist '(("razor" . "\\.cshtml\\'"))))

(provide 'conjure-csharp)
;;; conjure-csharp.el ends here
