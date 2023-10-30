;;; conjure-ui.el --- Conjure UI settings
;;; Commentary:
;;; Code:

;; Disable visible scroll-bar and toolbar when applicable
(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

;; always disable the menu-bar
(menu-bar-mode -1)

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
(size-indication-mode)

;; improved line highlighting for selected modes
(use-package lin
  :custom
  (lin-face 'lin-green)
  :config
  (lin-global-mode))

(global-hl-line-mode)

;; show line numbers
(require 'nlinum)
(require 'nlinum-relative)

(setq nlinum-format "%4d "
      nlinum-relative-redisplay-delay 0.2)

(global-nlinum-mode t)
(global-nlinum-relative-mode)

;; allow y/n responses
(fset 'yes-or-no-p 'y-or-n-p)

;; better frame titles
(setq frame-title-format
      '((:eval (if (buffer-file-name)
		   (abbreviate-file-name (buffer-file-name))
		 "%b"))))

(require 'which-key)
(if (daemonp)
    (add-hook 'server-after-make-frame-hook 'which-key-mode)
  (which-key-mode))

(defun font-exists-p (font)
  "Check if FONT exists (but only in x-window)."
  (when (and (display-graphic-p) (fboundp 'x-list-fonts))
    (if (null (x-list-fonts font)) nil t)))

(defun set-default-font (font)
  "Set the default FONT for Conjure."
  (when font
    (set-face-attribute 'default nil :family font :height 120 :weight 'normal :width 'normal)))

(when (and (display-graphic-p) (fboundp 'x-list-fonts))
  ;; TODO use a macro expansion to generate this
  (let ((selected-font (cond ((font-exists-p "Fira Code Retina") "Fira Code Retina")
			     ((font-exists-p "Fira Code") "Fira Code")
			     ((font-exists-p "Cascadia Code") "Cascadia Code")
			     ((font-exists-p "Source Code Pro") "Source Code Pro")
			     ((font-exists-p "Iosevka") "Iosevka")
			     ((font-exists-p "Meslo") "Meslo"))))
    (when (bound-and-true-p selected-font)
      (set-default-font selected-font))))

(require 'ligature)
;; Enable the www ligature in every possible major mode
(ligature-set-ligatures 't '("www"))

;; Enable ligatures in programming modes
(ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
				     ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
				     "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
				     "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
				     "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
				     "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
				     "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
				     "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
				     "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
				     "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))

(defvar conjure-use-ligatures)
(global-ligature-mode conjure-use-ligatures)

;; Make ef-themes functions available but defer to user-preferences
(require 'ef-themes)

(provide 'conjure-ui)

;;; conjure-ui.el ends here
