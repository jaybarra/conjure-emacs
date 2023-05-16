;;; conjure-icons.el --- Icons in UI elements
;;; Commentary:
;;; Code:

(require 'conjure-packages)
(conjure-require-packages '(nerd-icons
			    nerd-icons-completion
			    nerd-icons-dired
			    nerd-icons-ibuffer))

(setq nerd-icons-font-family "Symbols Nerd Font Mono")

(when (display-graphic-p)
  (add-hook 'dired-mode-hook 'nerd-icons-dired-mode)
  (add-hook 'ibuffer-mode-hook 'nerd-icons-ibuffer-mode)
  (nerd-icons-completion-mode))

(provide 'conjure-icons)

;;; conjure-icons.el ends here
