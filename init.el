;;; init.el --- Enhanced Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; A modular Emacs configuration system with improved error handling
;;; and startup performance.
;;; Code:

;; Startup optimizations
(setq gc-cons-threshold (* 50 1024 1024))  ; Temporarily maximize GC threshold
(setq package-enable-at-startup nil)       ; Disable package.el at startup

(defvar conjure--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)         ; Disable file handlers during init

;; Restore after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1024 1024))
            (setq file-name-handler-alist conjure--file-name-handler-alist)
            (message "[Conjure] Startup completed in %.2f seconds with %d garbage collections."
                     (float-time (time-subtract after-init-time before-init-time))
                     gcs-done)))

(message "[Conjure] Emacs is coming online...")

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Core variables
(defgroup conjure nil
  "Conjure Emacs distribution customization."
  :group 'convenience)

(defvar conjure-user
  (getenv (if (eq system-type 'windows-nt) "USERNAME" "USER"))
  "Current system username.")

(defvar conjure-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "The root directory of the Emacs Conjure distribution.")

(dolist (dir '(("core"      . "conjure-core-dir")
               ("modules"   . "conjure-modules-dir")
               ("personal"  . "conjure-personal-dir")
               ("vendor"    . "conjure-vendor-dir")))
  (eval `(defvar ,(intern (cdr dir))
           (expand-file-name ,(car dir) conjure-dir)
           ,(format "Directory for %s files." (car dir)))))

(defvar conjure-savefile-dir
  (expand-file-name "savefile" user-emacs-directory)
  "Directory for storing generated files.")

(defvar conjure-custom-file
  (expand-file-name "custom.el" conjure-savefile-dir)
  "File for storing customizations.")

;; Ensure directories exist
(dolist (dir (list conjure-savefile-dir
                   conjure-personal-dir
                   (expand-file-name "preload" conjure-personal-dir)))
  (unless (file-exists-p dir)
    (make-directory dir t)))

;; Safe load function
(defun conjure-load-file (file)
  "Safely load FILE, showing any errors."
  (condition-case err
      (load file)
    (error
     (message "[Conjure] Error loading %s: %s" file (error-message-string err)))))

;; Load core components
(dolist (core-file '("conjure-custom.el"
                     "conjure-core.el"
                     "conjure-ui.el"
                     "conjure-editor.el"))
  (conjure-load-file (expand-file-name core-file conjure-core-dir)))

;; Enhanced directory loader
(defun conjure-load-directory (dir &optional recursive)
  "Load all Emacs Lisp files in DIR.
If RECURSIVE is non-nil, load files in subdirectories as well."
  (dolist (file (directory-files-recursively
                 dir "\\.el$"
                 recursive
                 (lambda (dir) (not (string-match-p "^\\.+" (file-name-nondirectory dir))))))
    (conjure-load-file file)))

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

;; TODO restore this
;; ;; Load modules
;; (dolist (module '("os" "tools" "language"))
;;   (let ((module-dir (expand-file-name module conjure-modules-dir)))
;;     (when (file-directory-p module-dir)
;;       (conjure-load-directory module-dir t))))

;; Set and load custom file
(setq custom-file conjure-custom-file)
(when (file-exists-p custom-file)
  (conjure-load-file custom-file))





;; allow overwrite of active region by typing
(delete-selection-mode t)

(setq inhibit-startup-message t
      initial-scratch-message nil
      require-final-newline t)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(use-package exec-path-from-shell
  :ensure t
  :demand t)

(use-package undo-tree
  :ensure t
  :delight
  :config
  (setq undo-tree-history-directory-alist `((".*" . ,temporary-file-directory))
        undo-tree-auto-save-history t)
  (global-undo-tree-mode))

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package ef-themes
  :ensure t)

(use-package consult
  :bind(;; C-c bindings in `mode-specific-map'
        ("C-c M-x" . consult-mode-command)
        ("C-c h" . consult-history)
        ("C-c k" . consult-kmacro)
        ("C-c m" . consult-man)
        ("C-c i" . consult-info)
        ([remap Info-search] . consult-info)
        ;; C-x bindings in `ctl-x-map'
        ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
        ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
        ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
        ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
        ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
        ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
        ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
        ;; Custom M-# bindings for fast register access
        ("M-#" . consult-register-load)
        ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
        ("C-M-#" . consult-register)
        ;; Other custom bindings
        ("M-y" . consult-yank-pop)                ;; orig. yank-pop
        ;; M-g bindings in `goto-map'
        ("M-g e" . consult-compile-error)
        ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
        ("M-g g" . consult-goto-line)             ;; orig. goto-line
        ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
        ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
        ("M-g m" . consult-mark)
        ("M-g k" . consult-global-mark)
        ("M-g i" . consult-imenu)
        ("M-g I" . consult-imenu-multi)
        ;; M-s bindings in `search-map'
        ("M-s d" . consult-find)                  ;; Alternative: consult-fd
        ("M-s c" . consult-locate)
        ("M-s g" . consult-grep)
        ("M-s G" . consult-git-grep)
        ("M-s r" . consult-ripgrep)
        ("M-s l" . consult-line)
        ("M-s L" . consult-line-multi)
        ("M-s k" . consult-keep-lines)
        ("M-s u" . consult-focus-lines)
        ;; Isearch integration
        ("M-s e" . consult-isearch-history)
        :map isearch-mode-map
        ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
        ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
        ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
        ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
        ;; Minibuffer history
        :map minibuffer-local-map
        ("M-s" . consult-history)                 ;; orig. next-matching-history-element
        ("M-r" . consult-history))                ;; orig. previous-matching-history-element
  
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<"))

(use-package consult-project-extra
  :ensure t
  :bind
  (("C-c p f" . consult-project-extra-find)
   ("C-c p o" . consult-project-extra-find-other-window)))

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :config
  (vertico-multiform-mode)

  ;; Different display modes for different commands
  (setq vertico-multiform-commands
        '((consult-line buffer)
          (consult-imenu buffer)
          (consult-ripgrep buffer)
	  (consult-yank-pop indexed)
          (t reverse)))
  
  ;; Different display modes for different categories
  (setq vertico-multiform-categories
        '((file flat)
          (imenu flat)
          (symbol flat)
          (consult-grep buffer))))

(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package which-key
  :ensure t
  :delight
  :config
  (which-key-mode +1))

(use-package delight
  :ensure t)

(use-package smartparens
  :ensure t
  :hook ((prog-mode . smartparens-strict-mode)
	 (prog-mode . show-smartparens-mode))
  :bind (:map smartparens-mode-map
	      
              ;; Navigation
              ("C-M-f" . sp-forward-sexp)
              ("C-M-b" . sp-backward-sexp)
              ("C-M-n" . sp-next-sexp)
              ("C-M-p" . sp-previous-sexp)
              ("C-M-a" . sp-beginning-of-sexp)
              ("C-M-e" . sp-end-of-sexp)

	      ;; Slurping & Barfing (extending/contracting pairs)
              ("C-)" . sp-forward-slurp-sexp)
              ("C-}" . sp-forward-barf-sexp)
              ("C-(" . sp-backward-slurp-sexp)
              ("C-{" . sp-backward-barf-sexp)
	      
	      ;; Wrapping
              ("C-M-(" . sp-wrap-round)        ; wrap with ()
              ("C-M-[" . sp-wrap-square)       ; wrap with []
              ("C-M-{" . sp-wrap-curly)        ; wrap with {}
              
              ;; Unwrapping & Splitting
              ("C-M-k" . sp-kill-sexp)
              ("C-M-w" . sp-copy-sexp)
              ("M-[" . sp-unwrap-sexp)
              ("M-]" . sp-split-sexp)
              
              ;; Depth-related commands
              ("C-M-d" . sp-down-sexp)
              ("C-M-u" . sp-up-sexp)
              
              ;; Movement by sexp
              ("C-M-<left>" . sp-backward-slurp-sexp)
              ("C-M-<right>" . sp-forward-slurp-sexp)
              ("C-M-t" . sp-transpose-sexp))
  :config
  (require 'smartparens-config))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode text-mode markdown-mode))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(use-package catppuccin-theme
  :ensure t
  :config
  (load-theme 'catppuccin :no-prompt))

(use-package eglot
  :defer t
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0) ; set to 10k when debugging
  (eglot-extend-to-xref nil)
  (eglot-ignored-server-capabilities
   '(:hoverProvider
     :documentHighlightProvider
     :documentFormattingProvider
     :documentRangeFormattingProvider
     :documentOnTypeFormattingProvider
     :colorProvider
     :foldingRangeProvider))
  ;; (eglot-stay-out-of '(yasnippet))
  )

(use-package goggles
  :ensure t
  :delight
  :hook ((prog-mode text-mode markdown-mode) . goggles-mode))

(use-package tempel
  :ensure t)

(use-package elixir-ts-mode
  :ensure t)

(use-package clojure-ts-mode
  :ensure t)

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c g b" . magit-blame)
         ("C-c g c" . magit-checkout)
         ("C-c g f" . magit-find-file))
  :config
  (setq magit-status-sections-hook
        '(magit-insert-status-headers
          magit-insert-merge-log
          magit-insert-rebase-sequence
          magit-insert-am-sequence
          magit-insert-sequencer-sequence
          magit-insert-bisect-output
          magit-insert-bisect-rest
          magit-insert-bisect-log
          magit-insert-untracked-files
          magit-insert-unstaged-changes
          magit-insert-staged-changes
          magit-insert-stashes
          magit-insert-unpulled-from-upstream
          magit-insert-unpulled-from-pushremote
          magit-insert-unpushed-to-upstream
          magit-insert-unpushed-to-pushremote))

  ;; Performance optimizations
  (setq magit-refresh-status-buffer nil) ; only refresh status buffer when necessary
  (setq magit-diff-highlight-indentation nil)
  (setq magit-diff-highlight-trailing nil)
  (setq magit-diff-paint-whitespace nil)
  
  ;; Display settings
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  (setq magit-repository-directories '(("~/workspace" . 2)))
  (setq magit-save-repository-buffers 'dontask))

(use-package git-timemachine
  :ensure t
  :bind ("C-c g t" . git-timemachine))

(use-package git-modes
  :ensure t)

(use-package diff-hl
  :ensure t
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :init
  (global-diff-hl-mode)
  (diff-hl-margin-mode)
  (diff-hl-flydiff-mode)
  :config
  (setq diff-hl-draw-borders nil)
  (diff-hl-margin-mode))

(use-package magit-todos
  :ensure t
  :defer t
  :after magit
  :hook (magit-mode . magit-todos-mode))

(use-package blamer
  :ensure t
  :defer t
  :bind (("C-c g l" . blamer-show-commit-info))
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                   :background unspecified
                   :height 120
                   :italic t)))
  :config
  (setq blamer-author-formatter " ✎ %s ")
  (setq blamer-datetime-formatter "[%s]")
  (setq blamer-commit-formatter " ● %s")
  (setq blamer-prettify-time-p t))

(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  :config
  (global-corfu-mode))

(use-package kind-icon
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package browse-kill-ring
  :ensure t
  :bind (("s-y" . browse-kill-ring)))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :config
  (setq dired-recursive-copies 'always
	dired-recursive-deletes 'always
	dired-dwim-target t)

  (when (string= system-type "darwin")
    (setq dired-use-ls-dired t
          insert-directory-program "/opt/homebrew/bin/gls"
          dired-listing-switches "-aBhl --group-directories-first"))
  
  (add-hook 'dired-mode-hook 'auto-revert-mode))

(use-package consult-eglot
  :ensure t)

(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode))

(use-package colorful-mode
  :ensure t
  :hook ((prog-mode text-mode) . colorful-mode)
  :config
  (setq colorful-use-prefix t))

(use-package avy
  :ensure t
  :bind (("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-2)
         ;;("M-g f" . avy-goto-line) ;; conflicts with consult-flymake
         ("M-g w" . avy-goto-word-1)))

(use-package zop-to-char
  :ensure t
  :bind (("M-z" . zop-up-to-char)
         ("M-Z" . zop-to-char)))

(use-package easy-kill
  :ensure t
  :bind (([remap mark-sexp] . easy-mark)
	 ([remap kill-ring-save] . easy-kill)))

(use-package anzu
  :ensure t
  :bind (("M-%" . anzu-query-replace)
	 ("C-M-%" . anzu-query-replace-regexp)))

(use-package olivetti
  :ensure t)

(use-package adoc-mode
  :ensure t)

(use-package writeroom-mode
  :ensure t)

(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-ibuffer
  :ensure t
  :defer t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package nerd-icons-dired
  :ensure t
  :defer t
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package rust-mode
  :ensure t)

(use-package zig-mode
  :ensure t)

(use-package ace-window
  :ensure t
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)) ; Use home row keys
  (setq aw-background nil)                      ; Don't dim other windows
  (setq aw-dispatch-always t)                   ; Enable action menu
  (setq aw-dispatch-alist
        '((?x aw-delete-window "Delete Window")
          (?m aw-swap-window "Swap Windows")
          (?M aw-move-window "Move Window")
          (?c aw-copy-window "Copy Window")
          (?j aw-switch-buffer-in-window "Select Buffer")
          (?n aw-flip-window)
          (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
          (?e aw-execute-command-other-window "Execute Command Other Window")))
  :bind
  (("M-o" . ace-window)
   ("C-x o" . ace-window)))

;;; init.el ends here
