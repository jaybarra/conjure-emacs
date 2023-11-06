;;; -*- mode: emacs-lisp -*-

;;; * Debug

(setq debug-on-error t)

(defconst emacs-start-time (current-time))
(defconst osx-p (eq system-type 'darwin))
(defconst linux-p (eq system-type 'gnu/linux))

(message "[Conjure] Emacs is coming online...")

;; Profile Emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "[Conjure] Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(defvar conjure-user
  "Current system username."
  (getenv (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(defvar conjure-dir (file-name-directory user-init-file)
  "The root dir of the Emacs Conjure distribution.")
(defvar conjure-core-dir (expand-file-name "core" conjure-dir)
  "The home of Conjure's core functionality.")
(defvar conjure-modules-dir (expand-file-name  "modules" conjure-dir)
  "This directory houses all of the built-in Conjure modules.")
(defvar conjure-personal-dir (expand-file-name  "personal" conjure-dir)
  "This directory is for you personal configuration.")
(defvar conjure-personal-preload-dir (expand-file-name "preload" conjure-personal-dir)
  "This directory is for you personal configuration.")
(defvar conjure-vendor-dir (expand-file-name  "vendor" conjure-dir)
  "This directory is for you personal configuration.")
(defvar conjure-savefile-dir (expand-file-name "savefile" user-emacs-directory)
  "Folder for storing generated history files.")
(defvar conjure-modules-file (expand-file-name "conjure-modules.el" conjure-personal-dir)
  "File containing a list of modules that will be loaded by Conjure.")

(unless (file-exists-p conjure-savefile-dir)
  (make-directory conjure-savefile-dir))

(defun conjure-add-subfolders-to-load-path (parent-dir)
  "Add all level PARENT-DIR subdirs to the `load-path'."
  (dolist (f (directory-files parent-dir))
    (let ((name (expand-file-name f parent-dir)))
      (when (and (file-directory-p name)
                 (not (string-prefix-p "." f)))
        (add-to-list 'load-path name)
        (conjure-add-subfolders-to-load-path name)))))

;; Use newer byte-code automatically
(setq load-prefer-newer t)

(add-to-list 'load-path conjure-core-dir)
(add-to-list 'load-path conjure-modules-dir)
(add-to-list 'load-path conjure-vendor-dir)
(conjure-add-subfolders-to-load-path conjure-vendor-dir)

;; reduce the frequency of garbage collection events
;; allocate 50MB intead of the default 0.8MB
(setq gc-cons-threshold (* 50 1000 1000))

;; warn on large files (50MB or more)
(setq large-file-warning-threshold (* 50 1000 1000))

;; preload personal settings from `conjure-personal-preload-dir'
(when (file-exists-p conjure-personal-dir)
  (message "[Conjure] Loading personal configuration files in %s..." conjure-personal-preload-dir)
  (mapc 'load (directory-files conjure-personal-preload-dir 't "^[^#\.].*el$")))

(message "[Conjure] Loading Conjure's core modules...")
(require 'conjure-custom) ;; Load this first
(require 'conjure-core)
(require 'conjure-ui)
(require 'conjure-mode)
(require 'conjure-editor)
(require 'conjure-global-keybindings)

;; (when (eq system-type 'gnu/linux)
;;   (require 'conjure-linux))

(when (eq system-type 'darwin)
  (require 'conjure-macos))

;; WSL (windows subsystem for linux) specific setting
;; (when (and (eq system-type 'gnu/linux) (getenv "WSLENV"))
;;   (require 'prelude-wsl))

;; (when (eq system-type 'windows-nt)
;;   (require 'conjure-windows))

(message "[Conjure] Loading Conjure's additional modules...")

;; Conjure's modules
(if (file-exists-p conjure-modules-file)
    (load conjure-modules-file)
  (message "[Conjure] Missing personal modules file %s" conjure-modules-file)
  (message "[Conjure] Falling back to the default example file sample/conjure-modules.el")
  (message "[Conjure] Copy the sample to your personal configuration folder [%s] to change behaviors" conjure-modules-dir))

(setq custom-file (expand-file-name "custom.el" conjure-personal-dir))

;; load personal settings
(when (file-exists-p conjure-personal-dir)
  (message "[Conjure] Loading personal configuration files in %s..." conjure-personal-dir)
  (mapc 'load (delete
               conjure-modules-file
               (directory-files conjure-personal-dir 't "^[^#\.].*\\.el$"))))

(message "[Conjure] Conjure is primed and ready...")


;; (use-package async)



;; (use-package projectile
;;   :config
;;   (setq projectile-cache-file (expand-file-name "projectile.cache" conjure-savefile-dir)
;;         projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" conjure-savefile-dir)
;;         projectile-ignored-projects '("~/")
;;         projectile-create-missing-test-files t)
;;   (projectile-mode +1)
;;   :bind
;;   (("C-x p" . projectile-find-file)))

;; (use-package rg)

;; ;; enable y/n answers in a non-destructive and native-comp friendly manner
;; (defun yes-or-no-p-advice (_orig-fun &rest args)
;;   "Advice to use `y-or-n-p' instead of `yes-or-no-p', passing along ARGS."
;;   (apply 'y-or-n-p args))
;; (advice-add 'yes-or-no-p :around 'yes-or-no-p-advice)

;; (use-package exec-path-from-shell
;;   :config
;;   (exec-path-from-shell-initialize))

;; (use-package anzu
;;   :diminish
;;   :config
;;   (global-anzu-mode +1))

;; (use-package zop-to-char)



;; (use-package flymake-eslint)

;; (require 'eglot)
;; (require 'flymake)
;; (require 'treesit)

;; (use-package treesit-auto
;;   :config
;;   (global-treesit-auto-mode))

;; (use-package delight)

;; (setq-default display-line-numbers 'relative)

;; (use-package nerd-icons)
;; (use-package nerd-icons-dired
;;   :after nerd-icons
;;   :hook (dired-mode . nerd-icons-dired-mode))

;; (use-package nerd-icons-ibuffer
;;   :after nerd-icons
;;   :hook ibuffer-mode)

;; (use-package apheleia
;;   :hook (prog-mode . apheleia-mode)
;;   :config
;;   (add-to-list 'apheleia-mode-alist '(tsx-ts-mode . prettier-typescript))
;;   (add-to-list 'apheleia-mode-alist '(typescript-ts-mode . prettier-typescript)))

;; (add-hook 'tsx-ts-mode-hook 'eglot-ensure)
;; (add-hook 'typescript-ts-mode-hook 'eglot-ensure)

;; (use-package coverlay
;;   :diminish)

;; (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; (global-set-key (kbd "<f5>") 'revert-buffer)

;; (when (display-graphic-p)
;;   (tool-bar-mode -1)
;;   (scroll-bar-mode -1))

;; ;; always disable the menu-bar
;; (menu-bar-mode -1)

;; (blink-cursor-mode -1)

;; ;; disable the bell
;; (setq ring-bell-function 'ignore)

;; ;; disable startup screen
;; (setq inhibit-startup-message t)

;; ;; fixup scrolling
;; (setq scroll-margin 0
;;       scroll-conservatively 10000
;;       scroll-preserve-screen-position 1)

;; (use-package lin
;;   :config
;;   (lin-global-mode))
;; (global-hl-line-mode)

;; (column-number-mode)
;; (size-indication-mode)

;; ;; better frame titles
;; (setq frame-title-format
;;       '((:eval (if (buffer-file-name)
;; 		           (abbreviate-file-name (buffer-file-name))
;; 		         "%b"))))

;; (use-package which-key
;;   :diminish
;;   :config
;;   (which-key-mode))

;; (defun font-exists-p (font)
;;   "Check if FONT exists (but only in x-window)."
;;   (when (and (display-graphic-p) (fboundp 'x-list-fonts))
;;     (if (null (x-list-fonts font)) nil t)))

;; (defun set-default-font (font)
;;   "Set the default FONT for Conjure."
;;   (when font
;;     (set-face-attribute 'default nil :family font :height 120 :weight 'normal :width 'normal)))

;; (when (and (display-graphic-p) (fboundp 'x-list-fonts))
;;   ;; TODO use a macro expansion to generate this
;;   (let ((selected-font (cond
;;                         ((font-exists-p "FiraCode Nerd Font") "FiraCode Nerd Font")
;;                         ((font-exists-p "Fira Code Retina") "Fira Code Retina")
;; 			            ((font-exists-p "Fira Code") "Fira Code")
;; 			            ((font-exists-p "Cascadia Code") "Cascadia Code")
;; 			            ((font-exists-p "Source Code Pro") "Source Code Pro")
;; 			            ((font-exists-p "Iosevka") "Iosevka")
;; 			            ((font-exists-p "Meslo") "Meslo"))))
;;     (when (bound-and-true-p selected-font)
;;       (set-default-font selected-font))))

;; (use-package ligature
;;   :config
;;   ;; Enable the www ligature in every possible major mode
;;   (ligature-set-ligatures 't '("www"))
;;   ;; Enable ligatures in programming modes
;;   (ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
;; 				                       ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
;; 				                       "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
;; 				                       "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
;; 				                       "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
;; 				                       "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
;; 				                       "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
;; 				                       "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
;; 				                       "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
;; 				                       "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))
;;   (global-ligature-mode t))

;; (use-package avy
;;   :bind
;;   (("C-;" . avy-goto-char)))

;; (setq tab-always-indent 'complete)
;; (setq completion-cycle-threshold nil)

;; (use-package rainbow-mode
;;   :commands (rainbow-mode))

;; (use-package wgrep)

;; (use-package xclip
;;   :commands xclip-mode)

;; (use-package whitespace
;;   :diminish
;;   :ensure nil
;;   :commands (whitespace-mode)
;;   :config
;;   (setq whitespace-style '(face trailing tabs spaces tab-mark space-mark))
;;   (setq whitespace-display-mappings '((tab-mark 9 [8250 9])
;;                                       (space-mark 32 [183][46])
;;                                       (space-mark 160 [164][95])))
;;   :hook (prog-mode markdown-mode))

;; (use-package rainbow-delimiters
;;   :diminish
;;   :hook (prog-mode . rainbow-delimiters-mode))

;; (setq require-final-newline t)

;; (use-package volatile-highlights
;;   :diminish
;;   :config
;;   (volatile-highlights-mode t))

;; (use-package flyspell
;;   :ensure nil
;;   :config
;;   (setq ispell-program-name "aspell"
;;         ispell-extra-args '("--sug-mode=ultra")))



;; (use-package savehist
;;   :init
;;   (savehist-mode))




;; (use-package consult
;;   :bind
;;   (("C-s" . consult-line)
;;    ("C-s-f" . consult-ripgrep)
;;    ("M-g f" . consult-flymake))
;;   :config
;;   (consult-customize consult-theme :preview-key '(:debounce 0.2 any) :preview-key '(:debounce 0.4 any)))

;; (use-package consult-projectile
;;   :straight (consult-projectile :type git :host gitlab :repo "OlMon/consult-projectile" :branch "master")
;;   :after (consult projectile)
;;   :bind
;;   ("C-x p p" . consult-projectile)
;;   ("C-x p f" . consult-projectile-find-file))

;; (use-package consult-eglot)

;; ;; newlines at the end of files
;; (setq require-final-newline t)

;; ;; store all backups and auto-save files in tmp
;; (setq backup-directory-alist
;;       `((".*" . ,temporary-file-directory)))
;; (setq auto-save-file-name-transforms
;;       `((".*" ,temporary-file-directory t)))

;; ;; revert buffers when the underlying files are changed
;; (global-auto-revert-mode t)

;; (use-package undo-tree
;;   :diminish
;;   :init
;;   (setq undo-tree-history-directory-alist `((".*" . ,temporary-file-directory))
;;         undo-tree-auto-save-history t)
;;   :config
;;   (global-undo-tree-mode))

;; (use-package affe
;;   :after consult
;;   :config
;;   (consult-customize affe-grep :preview-key "M-."))

;; (use-package easy-kill
;;   :bind
;;   ([remap kill-ring-save] . easy-kill))

;; (use-package operate-on-number)

;; (use-package super-save
;;   :diminish
;;   :config
;;   (super-save-mode +1))

;; (use-package browse-kill-ring
;;   :bind
;;   ("M-y" . browse-kill-ring))

;; (use-package clojure-mode)
;; (use-package rustic)

;; (setq consult-narrow-key "<"
;;       consult-goto-line-numbers nil)

;; (advice-add #'register-preview :override #'consult-register-window)

;; (use-package xref
;;   :init
;;   (setq xref-show-xrefs-function #'consult-xref
;; 	    xref-show-definitions-function #'consult-xref
;; 	    xref-search-program (cond
;; 			                 ((executable-find "ugrep") 'ugrep)
;; 			                 ((executable-find "rg") 'ripgrep)
;; 			                 (t 'grep))))

;; (add-hook 'completion-list-mode-hook 'consult-preview-at-point-mode)

;; (setq major-mode-remap-alist
;;       '((yaml-mode . yaml-ts-mode)
;;         (python-mode . python-ts-mode)
;;         (css-mode . css-ts-mode)
;;         (json-mode . json-ts-mode)))

;; (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))

;; (use-package emmet-mode
;;   :config
;;   (add-to-list 'emmet-jsx-major-modes 'tsx-ts-mode)
;;   (add-to-list 'emmet-jsx-major-modes 'typescript-ts-mode))

;; (use-package tempel
;;   :custom
;;   (tempel-trigger-prefix "<")
;;   :preface
;;   (defun tempel-setup-capf ()
;;     (setq-local completion-at-point-functions
;;                 (cons #'tempel-complete
;;                       completion-at-point-functions)))

;;   :hook
;;   ((prog-mode conf-mode text-mode) . tempel-setup-capf)

;;   :bind (:map tempel-map
;;               ("M-+" . tempel-complete) ;; Alternative tempel-expand
;;               ("<tab>" . tempel-next)
;;               ("M-*" . tempel-insert)))

;; (use-package tempel-collection)

;; (use-package eglot-tempel)

;; (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))
