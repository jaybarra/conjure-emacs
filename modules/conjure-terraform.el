;;; conjure-terraform.el --- Configurations for Terraform in Conjure
;;; Commentary:
;;; Code:

(use-package terraform-mode
  :ensure t
  :defer t
  :config
  (when (executable-find "terraform-ls")
    (add-to-list 'eglot-server-programs '(terraform-mode "terraform-ls" "serve"))))

(provide 'conjure-terraform)
;;; conjure-terraform.el ends here
