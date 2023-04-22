;;; conjure-ui.el --- Conjure UI settings
;;; Commentary:
;;; Code:

;; Disable visible scroll-bar and toolbar when applicable
(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

;; always disable the menu-bar
(menu-bar-mode -1)

;; don't blink the cursor
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

;; show line numbers
(global-nlinum-mode t)

;; allow y/n responses
(fset 'yes-or-no-p 'y-or-n-p)

;; better frame titles
(setq frame-title-format
      '((:eval (if (buffer-file-name)
		   (abbreviate-file-name (buffer-file-name))
		 "%b"))))
(if (daemonp)
    (add-hook 'server-after-make-frame-hook 'which-key-mode)
  (which-key-mode))

(defun font-exists-p (font)
  "Check if FONT exists (but only in x-window)."
  (when (fboundp 'x-list-fonts)
    (if (null (x-list-fonts font)) nil t)))

(defun set-default-font (font)
  "Set the default FONT for Conjure."
  (when font
    (set-face-attribute 'default nil :family font :height 130 :weight 'normal :width 'normal)))

(when (fboundp 'x-list-fonts)
  ;; TODO use a macro expansion to generate this
  (let ((selected-font (cond ((font-exists-p "Fira Code Retina") "Fira Code Retina")
			     ((font-exists-p "Fira Code") "Fira Code")
			     ((font-exists-p "Source Code Pro") "Source Code Pro")
			     ((font-exists-p "Iosevka") "Iosevka")
			     ((font-exists-p "Cascadia Code") "Cascadia Code")
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

(require 'ef-themes)
(ef-themes-load-random 'dark)

(provide 'conjure-ui)

;;; conjure-ui.el ends here
