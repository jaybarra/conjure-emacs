;;; conjure-terraform.el --- Configurations for OpenTofu/Terraform in Conjure -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package terraform-mode
  :hook (terraform-mode . eglot-ensure)
  :config
  (setq terraform-indent-level 2)
  (setq terraform-format-on-save t)

  ;;(add-to-list 'auto-mode-alist '("\\.tfvars\\'" . terraform-mode))
  )

(provide 'conjure-terraform)
;;; conjure-terraform.el ends here
