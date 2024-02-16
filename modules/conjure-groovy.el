;;; conjure-groovy.el --- Configurations for Groovy in Conjure
;;; Commentary:
;;; Code:

(use-package groovy-mode
  :ensure t
  :defer t
  :mode (("\\.groovy\\'" . groovy-mode)
         ("Jenkinsfile\\'" . groovy-mode))
  :config
  (setq groovy-indent-offset 4))

(provide 'conjure-groovy)
;;; conjure-groovy.el ends here
