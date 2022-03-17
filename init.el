;;; init.el --- Emacs Initialization File
;;; Commentary:
;;; Code:
(defconst emacs-start-time (current-time))
(defconst *is-a-mac* (eq system-type 'darwin))
(setq gc-cons-threshold 402653184 gc-cons-percentage 0.6)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(setq inhibit-startup-message t
      display-time-24hr-format t
      display-time-use-mail-icon t
      visible-bell t)

(scroll-bar-mode -1)   ; Disable visible scroll-bar
(tool-bar-mode -1)     ; Disable the toolbar
(tooltip-mode -1)      ; Disable tooltips
(set-fringe-mode 10)   ; Make space on the left

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

(customize-set-variable 'global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)
(setq-default indent-tabs-mode nil)
(fset 'yes-or-no-p 'y-or-n-p)

(when *is-a-mac*
  (setq mouse-wheel-scroll-amount '(1
                                    ((shift) . 5)
                                    ((control)))))

(customize-set-variable 'kill-do-not-save-duplicates t)

;; Make scrolling less stuttered
(setq auto-window-vscroll nil)
(customize-set-variable 'fast-but-imprecise-scrolling t)
(customize-set-variable 'scroll-conservatively 101)
(customize-set-variable 'scroll-margin 0)
(customize-set-variable 'scroll-preserve-screen-position t)

;; Better support for files with long lines
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)
(global-so-long-mode 1)

;; Make shbang (#!) file executable when saved
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(set-face-attribute 'default nil :font "MesloLGS NF" :height 140)

(defun light ()
  "Set a light theme."
  (interactive)
  (load-theme 'doom-one-light t))

(defun dark ()
  "Set a dark theme."
  (interactive)
  (load-theme 'zenburn))

(defun disable-active-themes ()
  "Disable themes before switching."
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(defadvice load-theme (before disable-themes-first activate)
  "Update theme loading."
  (disable-active-themes))

;; Initialize Package Sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package)
(setq use-package-always-ensure t)

(require 'paradox)
(paradox-enable)

(use-package exec-path-from-shell
  :config
  (dolist (var '("JAVA_HOME" "SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"))
    (add-to-list 'exec-path-from-shell-variables var)))
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(use-package hydra)
(require 'hydra)

(global-set-key
 (kbd "<f2>")
 (defhydra hydra-text-scale (:timeout 4)
   "scale text"
   ("j" text-scale-increase "in")
   ("k" text-scale-decrease "out")
   ("=" (text-scale-set 0) "reset")
   ("q" nil "quit" :exit t)))

;; See what commands were run
(use-package command-log-mode)

(use-package beacon
  :hook (after-init . beacon-mode))

(use-package feature-mode)

(use-package diminish)

(use-package magit
  :defer t)

(use-package git-gutter
  :diminish
  :hook (prog-mode . git-gutter-mode))

(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package rg
  :hook (after-init . rg-enable-default-bindings))

(use-package flycheck
  :custom
  (setq-default flycheck-emacs-lisp-load-path 'inherit))

(use-package ibuffer
  :bind (("C-x C-b" . ibuffer)))

(use-package ibuffer-vc)

(setq python-shell-interpreter "python3")

(use-package aggressive-indent)
(use-package auto-compile)
(use-package auto-highlight-symbol
  :commands (global-auto-highlight-symbol-mode auto-highlight-symbol-mode)
  :bind (("C-x a h" . auto-highlight-symbol-mode)
         :map
         auto-highlight-symbol-mode-map)
  :config
  (add-to-list 'ahs-modes 'clojure-mode))

(use-package centered-cursor-mode)
(use-package clean-aindent-mode)
(use-package column-enforce-mode)
(use-package dired-quick-sort)
(use-package drag-stuff)
(use-package editorconfig)
(use-package golden-ratio)
(use-package highlight-indentation)
(use-package highlight-numbers)
(use-package highlight-parentheses)
(use-package hl-todo)
(use-package lorem-ipsum)
(use-package nameless)
(use-package password-generator)
(use-package rainbow-delimiters
  :diminish
  :hook (prog-mode . rainbow-delimiters-mode))
(use-package uuidgen)
(use-package undo-tree)
(use-package winum)
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-K" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package projectile
  :diminish
  :bind (("C-c p" . #'projectile-command-map))
  :config
  (projectile-mode +1)
  :custom
  (projectile-completion-system 'ivy)
  (projectile-switch-project-action #'projectile-dired)
  (projectile-project-search-path '(("~/workspace" . 2))))

(use-package flx
  :defer t
  :init
  (setq ivy-flx-limit 10000))

(use-package counsel
  :diminish
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)
  (counsel-mode 1))

(use-package smex
  :defer 1
  :after counsel)

(use-package yaml-mode)

(use-package which-key
  :diminish
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

(use-package paredit
  :diminish
  :hook ((emacs-lisp-mode . paredit-mode)
         (clojure-mode . paredit-mode)
         (lisp-mode . paredit-mode)))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1)
  :config
  (setq ivy-format-function #'ivy-format-function-line))

(use-package helpful
  :after counsel
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package company
  :diminish
  :config
  (global-company-mode t))

(column-number-mode)
(global-display-line-numbers-mode t)

(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode 1))

(use-package dockerfile-mode)
(use-package markdown-mode)
(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(setq-default js-indent-level 2)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(use-package whitespace-cleanup-mode
  :config
  (whitespace-cleanup-mode t))

(use-package all-the-icons)

(use-package all-the-icons-dired
  :requires all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package all-the-icons-ibuffer
  :requires all-the-icons
  :init (all-the-icons-ibuffer-mode 1))

(use-package all-the-icons-ivy
  :requires all-the-icons
  :after (ivy all-the-icons)
  :config (all-the-icons-ivy-setup))

(use-package all-the-icons-ivy-rich
  :requires all-the-icons
  :after (ivy-rich all-the-icons)
  :init (all-the-icons-ivy-rich-mode 1))

(use-package all-the-icons-ivy-rich
  :config
  (all-the-icons-ivy-rich-mode 1))

(use-package terraform-mode)

(use-package darkroom)

(defun format-xml ()
  (interactive)
  (shell-command-on-region 1 (point-max) "xmllint --format -" (current-buffer) t))

(require 'init-clojure)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(cider-enlighten-mode t)
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-safe-themes
   '("835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" "0d01e1e300fcafa34ba35d5cf0a21b3b23bc4053d388e352ae6a901994597ab1" "b77a00d5be78f21e46c80ce450e5821bdc4368abf4ffe2b77c5a66de1b648f10" default))
 '(exwm-floating-border-color "#c8c8c8")
 '(fast-but-imprecise-scrolling t)
 '(fci-rule-color "#383838")
 '(global-auto-revert-non-file-buffers t)
 '(highlight-tail-colors
   ((("#e9f1e8" "#50a14f" "green")
     . 0)
    (("#e1eef3" "#0184bc" "brightcyan")
     . 20)))
 '(jdee-db-active-breakpoint-face-colors (cons "#f0f0f0" "#4078f2"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#f0f0f0" "#50a14f"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#f0f0f0" "#9ca0a4"))
 '(kill-do-not-save-duplicates t)
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(objed-cursor-color "#e45649")
 '(package-selected-packages
   '(darkroom darkroom-mode ibuffer-vc terraform-mode all-the-icons-ivy all-the-icons-ibuffer all-the-icons-dired all-the-icons-ivy-rich all-the-icons doom-themes beacon whitespace-cleanup-mode git-gutter vs-light-theme flycheck flymake-kondor lsp-java lsp-metals lsp-mode zenburn-theme typescript-mode markdown-mode dockerfile-mode yasnippet exec-path-from-shell rg company helpful ivy-rich paredit yaml-mode doom-modeline smex counsel flx projectile ivy cider editorconfig drag-stuff dired-quick-sort column-enforce-mode clean-aindent-mode centered-cursor-mode auto-highlight-symbol auto-compile aggressive-indent clojure-mode magit diminish command-log-mode paradox use-package))
 '(pdf-view-midnight-colors '("#DCDCCC" . "#383838"))
 '(rustic-ansi-faces
   ["#fafafa" "#e45649" "#50a14f" "#986801" "#4078f2" "#a626a4" "#0184bc" "#383a42"])
 '(scroll-conservatively 101)
 '(scroll-margin 0)
 '(scroll-preserve-screen-position t)
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   '((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3")))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
