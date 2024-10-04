;;; conjure-diff-hl.el --- Configurations for Diff-Hl in Conjure -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (fboundp 'fringe-mode) (fringe-mode '8))
(setq-default fringes-outside-margins t)

(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode nil)
  
  (setq vc-git-diff-switches '("--histogram"))
  (setq diff-hl-flydiff-delay 0.5)
  
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(provide 'conjure-diff-hl)
;;; conjure-diff-hl.el ends here
