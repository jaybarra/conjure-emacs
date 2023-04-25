;;; conjure-helm.el --- Helm configuration
;;; Commentary:
;;; Code:

(require 'conjure-packages)

(conjure-require-packages '(helm helm-projectile))

(helm-mode 1)
(helm-projectile-on)

(global-set-key (kbd "M-x") 'helm-M-x)

(provide 'conjure-helm)
;;; conjure-helm.el ends here
