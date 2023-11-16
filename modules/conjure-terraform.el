;;; conjure-terraform.el --- Terraform Settings
;;; Commentary:
;;; Code:

(require 'conjure-programming)

(use-package terraform-mode
  :config
  (when (executable-find "terraform-ls")
    (add-to-list 'eglot-server-programs '(terraform-mode "terraform-ls" "serve"))
    (add-hook 'terraform-mode-hook 'eglot-ensure)))

;; TODO add terragrunt support in here as well

(provide 'conjure-terraform)
;;; conjure-terraform.el ends here
