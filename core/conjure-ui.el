;;; conjure-ui.el --- Conjure UI settings
;;; Commentary:
;;; Code:

;; Disable visible scroll-bar and toolbar when applicable
(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

;; always disable the menu-bar
(menu-bar-mode -1)

;; disable cursor blink
(blink-cursor-mode -1)

;; disable the bell
(setq ring-bell-function 'ignore)

;; disable startup screen
(setq inhibit-startup-message t)

;; fixup scrolling
(setq scroll-margin 0
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode)
;; (size-indication-mode -1)

;; clean up modeline entries
(use-package delight)

;; improved line highlighting for other modes
(use-package lin
  :config
  (lin-global-mode))

;; better frame titles
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Theme packages
(use-package ef-themes)
(use-package modus-themes
  :config
  (modus-themes-select 'modus-vivendi-tinted))

(defun font-exists-p (font)
  "Check if FONT exists (but only in x-window)."
  (when (and (display-graphic-p) (fboundp 'x-list-fonts))
    (if (null (x-list-fonts font)) nil t)))

(defun set-default-font (font)
  "Set the default FONT for Conjure."
  (when font
    (set-face-attribute 'default nil :family font :height 120 :weight 'normal :width 'normal)))

(when (and (display-graphic-p) (fboundp 'x-list-fonts))
  (let ((selected-font (cond
                        ((font-exists-p "Fira Code Retina") "Fira Code Retina")
			((font-exists-p "Fira Code") "Fira Code")
                        ((font-exists-p "Cascadia Code") "Cascadia Code")
                        ((font-exists-p "Source Code Pro") "Source Code Pro")
                        ((font-exists-p "Iosevka") "Iosevka")
			((font-exists-p "Meslo") "Meslo"))))
    (when (bound-and-true-p selected-font)
      (set-default-font selected-font))))

(use-package ligature
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(use-package which-key
  :delight
  :config
  (if (daemonp)
      (add-hook 'server-after-make-frame-hook 'which-key-mode)
    (which-key-mode +1)))

(use-package marginalia)

(use-package nerd-icons)

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-dired
  :delight
  :hook dired-mode)

(use-package nerd-icons-ibuffer
  :hook ibuffer-mode)

(provide 'conjure-ui)

;;; conjure-ui.el ends here
