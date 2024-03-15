;;; conjure-ui.el --- Conjure UI settings
;;; Commentary:
;;; Code:

(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

;; disable the menu-bar on non-Mac systems
(unless (eq system-type 'darwin)
  (menu-bar-mode -1))

;; disable cursor blink
(blink-cursor-mode -1)

(setq frame-title-format
      '("" invocation-name " Conjure - " (:eval (if (buffer-file-name)
                                                    (abbreviate-file-name (buffer-file-name))
                                                  "%b"))))

;; disable the bell
(setq ring-bell-function 'ignore)

(use-package ef-themes
  :ensure t
  :config
  (ef-themes-load-random 'dark))

(use-package delight :ensure t)

(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-dired
  :ensure t
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(provide 'conjure-ui)

;;; conjure-ui.el ends here
