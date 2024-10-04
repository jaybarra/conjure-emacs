;;; conjure-magit.el --- Configurations for Magit in Conjure -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package transient)
(use-package magit
  :commands (magit-status magit-get-current-branch)
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
        git-commit-summary-max-length 50))

(provide 'conjure-magit)
;;; conjure-magit.el ends here
