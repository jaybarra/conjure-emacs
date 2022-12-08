;;; conjure-magit.el --- magit configuration
;;; Commentary:
;;; Code:

(conjure-require-packages '(magit))

(with-eval-after-load 'magit
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)

  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(provide 'conjure-magit)
;;; conjure-magit.el ends here

