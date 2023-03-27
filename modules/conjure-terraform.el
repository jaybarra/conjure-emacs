;;; conjure-terraform.el --- Terraform Initialization
;;; Commentary:
;;; Code:
(require 'conjure-packages)
(conjure-require-packages '(terraform-mode))

(require 'eglot)
(when (executable-find "terraform-ls")
  (add-to-list 'eglot-server-programs '(terraform-mode "terraform-ls" "serve"))
  (add-hook 'terraform-mode-hook 'eglot-ensure))

(provide 'conjure-terraform)
;;; conjure-terraform.el ends here
