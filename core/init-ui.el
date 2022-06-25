;;; init-ui.el -- ui settings
;;; Commentary:
;;; Code:
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(scroll-bar-mode -1)   ; Disable visible scroll-bar
(tool-bar-mode -1)     ; Disable the toolbar
(tooltip-mode -1)      ; Disable tooltips
(blink-cursor-mode -1) ; no blinky

(setq-default cursor-type 'box)

(column-number-mode)
(size-indication-mode)
(global-display-line-numbers-mode t)

(fset 'yes-or-no-p 'y-or-n-p)

(setq inhibit-startup-message t ; disable startup
      display-time-24hr-format t
      display-time-use-mail-icon t
      visible-bell t
      use-dialog-box nil
      ring-bell-function 'ignore)

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Make scrolling less stuttered
(setq auto-window-vscroll nil)
(customize-set-variable 'fast-but-imprecise-scrolling t)
(customize-set-variable 'scroll-conservatively 101)
(customize-set-variable 'scroll-margin 0)
(customize-set-variable 'scroll-preserve-screen-position t)

;; window positioning
(if (display-graphic-p)
    (progn
      (setq initial-frame-alist `((left . 80)
                                  (top . 50)
                                  (height . 50)
                                  (width . 240)))

      (setq default-frame-alist `((left . 80)
                                  (top . 50)
                                  (height . 50)
                                  (width . 240)))))

(set-face-attribute 'default nil :font "MesloLGS NF" :height 140)

(load-theme 'zenburn t)

(provide 'init-ui)
;; init-ui.el ends here