;;; conjure-icons.el --- Icons in UI elements
;;; Commentary:
;;; Code:

(require 'conjure-packages)
;;(conjure-require-packages '(all-the-icons))
(conjure-require-packages '(nerd-icons))

(require 'nerd-icons)
(require 'nerd-icons-dired)
(require 'nerd-icons-ibuffer)

(setq nerd-icons-font-family "Symbols Nerd Font Mono")

(when (display-graphic-p)
  (add-hook 'dired-mode-hook 'nerd-icons-dired-mode)
  (add-hook 'ibuffer-mode-hook 'nerd-icons-ibuffer-mode))

(provide 'conjure-icons)

;;; conjure-icons.el ends here
