;;; conjure-helm.el --- Helm configuration
;;; Commentary:
;;; Code:

(use-package helm
  :bind ("M-x" . helm-M-x)
  :config
  (helm-mode 1))

(use-package helm-projectile
  :after helm
  :config
  (helm-projectile-on))

(provide 'conjure-helm)
;;; conjure-helm.el ends here
