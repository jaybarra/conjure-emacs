;;; conjure-php.el --- PHP Initialization
;;; Commentary:
;;; Code:

(use-package php-mode
  :ensure t
  :defer t
  :config
  (when (executable-find "intelephense")
    (add-to-list 'eglot-server-programs '(php-mode "intelephense" "--stdio"))))

(provide 'conjure-php)

;;; conjure-php.el ends here
