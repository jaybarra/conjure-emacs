;;; conjure-ui.el --- UI Configurations for Conjure -*- lexical-binding: t -*-
;;; Commentary:
;;; Optimized UI configuration with MonoLisa font and ligatures
;;; Code:

;; Disable unnecessary UI elements
(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

(unless (eq system-type 'darwin)
  (menu-bar-mode -1))

;; UI preferences
(blink-cursor-mode -1)
(setq ring-bell-function 'ignore)
(setq use-dialog-box nil)

;; Font configuration
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defvar conjure-preferred-font "MonoLisa"
  "The preferred font for Conjure.")

(defvar conjure-fallback-fonts
  '("Hack Nerd Font" "Cascadia Code" "JetBrains Mono" "Fira Code"
    "SF Mono" "Source Code Pro" "Menlo" "Monaco")
  "Ordered list of fallback fonts.")

(defun conjure-setup-fonts ()
  "Setup fonts with `conjure-preferred-font' as primary choice."
  (when (display-graphic-p)
    (let ((font (if (font-installed-p conjure-preferred-font)
                    (format "%s-13" conjure-preferred-font)
                  (cl-loop for font in conjure-fallback-fonts
                           when (font-installed-p font)
                           return (format "%s-13" font)))))
      (when font
        (set-face-attribute 'default nil
                            :font font
                            :height 130
                            :weight 'normal)))))

(require 'cl-lib)
;; Set up fonts when Emacs starts and for new frames
(conjure-setup-fonts)
(add-to-list 'default-frame-alist
             `(font . ,(format "%s-13"
                               (or (car (cl-remove-if-not #'font-installed-p
                                                       (cons conjure-preferred-font conjure-fallback-fonts)))
                                   conjure-preferred-font))))

(add-hook 'window-setup-hook #'conjure-setup-fonts)
(add-hook 'server-after-make-frame-hook #'conjure-setup-fonts)

;; MonoLisa-specific ligatures
(defvar monolisa-ligatures
  '("-->" "---" "->>" "-<<" "->" "-~" "!=" "!!" "!=" "!==" "!!." "!?"
    "#(" "#_" "#{" "#?" "#>" "##" "#:" "#=" "#!" "%%" "&%" "&&" "&*" "&+" "&-"
    "&/" "&=" "&>" "$>" "***" "*>" "*/" ".=" "..=" "..<" ".?" "::" ":::" ":="
    ":>" ":<" ";;" "!=" "!==" "??" "?." "?=" "?:" "##" "###" "####" "#{"
    "~>" "~~" "~~>" "~@" "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>"
    "^=" "::" ":=" "==" "===" "==>" "=>" "=>>" "=<<" "=/=" ">-" ">=" ">=>"
    ">>" ">>-" ">>=" ">>>" "<*" "<*>" "<|" "<|>" "<$" "<$>" "<+" "<+>" "<-"
    "<<-" "<<=" "<=" "<==" "<=>" "<=<" "<>" "<|" "<||" "<|||" "</" "</>" "<!--"
    "www" "&&" "||" "||=" "|=" "|>" "|]" "|}" "|->"))

;; Ligature setup
(use-package ligature
  :config
  ;; Enable ligatures in programming modes
  (ligature-set-ligatures 'prog-mode monolisa-ligatures)

  ;; Enable "www" ligature globally
  (ligature-set-ligatures 't '("www"))

  (global-ligature-mode t))

;; Theme configuration
(use-package catppuccin-theme
  :custom
  (catppuccin-flavor 'mocha)
  :config
  (load-theme 'catppuccin :no-confirm))

;; Additional themes available
(elpaca modus-themes)
(elpaca ef-themes)
(elpaca zenburn-theme)

(provide 'conjure-ui)
;;; conjure-ui.el ends here
