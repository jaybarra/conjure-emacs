;;; conjure-git.el --- Configurations for git in Conjure -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package transient)
(use-package magit
  :commands (magit-status magit-get-current-branch)
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
        git-commit-summary-max-length 50))

(use-package git-timemachine)

(provide 'conjure-git)
;;; conjure-git.el ends here
