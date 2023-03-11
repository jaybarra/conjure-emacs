;;; conjure-magit.el --- magit configuration
;;; Commentary:
;;; Code:
(require 'magit)
(require 'diff-hl)

(with-eval-after-load 'magit
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
	git-commit-summary-max-length 50)
  
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

(provide 'conjure-magit)
;;; conjure-magit.el ends here

