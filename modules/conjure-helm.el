;;; conjure-helm.el --- Helm configuration
;;; Commentary:
;;; Code:

(conjure-require-packages '(helm helm-projectile))

(require 'helm-projectile)

(helm-mode 1)
(helm-projectile-on)

(provide 'conjure-helm)
;;; conjure-helm.el ends here
