;;; conjure-ui.el --- Conjure UI settings
;;; Commentary:
;;; Code:

(when (display-graphic-p)
    (progn
      ; Disable visible scroll-bar and toolbar
      (tool-bar-mode -1)
      (scroll-bar-mode -1)))

(tooltip-mode -1)      ; Disable tooltips
(menu-bar-mode -1)     ; Disable menu-bar
(blink-cursor-mode -1) ; no blinky cursor

(column-number-mode)
(size-indication-mode)
(global-display-line-numbers-mode t)

(fset 'yes-or-no-p 'y-or-n-p)

(setq inhibit-startup-message t
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

(add-hook 'dired-mode-hook (lambda ()
		 (all-the-icons-dired-mode)
		 (diminish 'all-the-icons-dired-mode)))

(add-hook 'ibuffer-mode-hook (lambda ()
		   (all-the-icons-ibuffer-mode)))

(pulsar-global-mode +1)

;; improve `hl-line' highlighting
(lin-global-mode +1)

(setq goto-address-url-face 'link
      goto-address-url-mouse-face 'highlight
      goto-address-mail-face 'link
      goto-address-mail-mouse-face 'highlight)

(defun font-exists-p (font)
  "Checks if font exists."
  (if (null (x-list-fonts font)) nil t))

(defun set-default-font (font)
  "Sets the default font for Conjure."
  (when font
    (set-face-attribute 'default nil :family font :height 130 :weight 'normal :width 'normal)))

;; Check fonts in order, if they are present on the system
;; TODO use a macro expansion to generate this
(set-default-font
 (cond ((font-exists-p "Fira Code") "Fira Code")
       ((font-exists-p "Source Code Pro") "Source Code Pro")
       ((font-exists-p "Cascadia Code") "Cascadia Code")
       ((font-exists-p "Meslo") "Meslo")
       ((font-exists-p "Iosevka") "Iosevka")))

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

(global-ligature-mode conjure-use-ligatures)

(ef-themes-load-random 'dark)

(provide 'conjure-ui)
;;; conjure-ui.el ends here
