;;; -*- mode: emacs-lisp -*-

(defconst emacs-start-time (current-time))
(defconst osx-p (eq system-type 'darwin))
(defconst linux-p (eq system-type 'gnu/linux))

(message "[Conjure] Emacs is coming online...")

(defvar conjure-user
  "Current system username."
  (getenv (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(defvar conjure-dir (file-name-directory user-init-file)
  "The root dir of the Emacs Conjure distribution.")
(defvar conjure-custom-file (expand-file-name "custom.el" conjure-dir)
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

(setq custom-file conjure-custom-file)

(unless (file-exists-p conjure-savefile-dir)
  (make-directory conjure-savefile-dir))

;; Add MELPA repository for Emacs 29+
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Initialize the package system if not already initialized
(unless (bound-and-true-p package--initialized) ; Avoid initializing twice
  (package-initialize))

;; Ensure use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Enable verbose loading
(setq use-package-verbose t)

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


;; preload personal settings from `conjure-personal-preload-dir'
(when (file-exists-p conjure-personal-dir)
  (message "[Conjure] Loading personal configuration files in %s..." conjure-personal-preload-dir)
  (mapc 'load (directory-files conjure-personal-preload-dir 't "^[^#\.].*el$")))

(message "[Conjure] Loading Conjure's core modules...")
(require 'conjure-ui)
(require 'conjure-editor)
(require 'conjure-global-keybindings)


(require 'conjure-programming)
(require 'conjure-lisp)
(require 'conjure-python)
(require 'conjure-org)



;; Vertico
(use-package vertico
  :ensure t
  :config
  (vertico-mode))

;; Prefix the current candidate with “» ”. From
;; https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow
(advice-add #'vertico--format-candidate :around
            (lambda (orig cand prefix suffix index _start)
              (setq cand (funcall orig cand prefix suffix index _start))
              (concat
               (if (= vertico--index index)
                   (propertize "» " 'face 'vertico-current)
                 "  ")
               cand)))

;; Consult
(use-package consult
  :ensure t
  ;; Replace bindings with Consult commands

  :bind (
	     ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flycheck)
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s F" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines))
  :config
  ;; Use consult-xref to display xref results with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  (add-hook 'xref-after-jump-hook (lambda() (run-hooks 'consult-after-jump-hook))))


(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . 'consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . 'consult-dir)
         ("C-x C-j" . 'consult-dir-jump-file)))

(use-package consult-eglot
  :ensure t)

(use-package consult-flycheck
  :ensure t)

(use-package xref
  :ensure nil
  :init
  (setq xref-search-program (cond
			                 ((executable-find "ugrep") 'ugrep)
			                 ((executable-find "rg") 'ripgrep)
			                 (t 'grep))))

;; Corfu - completion
(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t) ;; Enables cycling for `corfu-next/previous`
  (corfu-auto t) ;; Enable auto completion
  :init
  (global-corfu-mode))

;; Marginalia
(use-package marginalia
  :ensure t
  :after corfu
  :init
  (marginalia-mode))

;; Orderless
(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

;; Optionally, enable richer annotations using the Marginalia package
(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `desc
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have embark-consult installed
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; Use the default face for kind icons
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package projectile
  :ensure t
  :config
  (setq projectile-cache-file (expand-file-name  "projectile.cache" conjure-savefile-dir))
  (setq projectile-completion-system 'auto) ;; Use default completion system
  (setq projectile-project-search-path '(("~/workspace/" . 3))) ;; Directories to search for projects
  (setq projectile-enable-caching t) ;; Enable caching for faster project switching
  
  (projectile-mode +1))

(use-package consult-projectile
  :ensure t
  :after (consult projectile)
  :bind (("C-x p p" . consult-projectile)
         ("C-x p s" . consult-projectile-switch-project)
         ("s-f" . consult-projectile-find-file)
         ;;("s-g d" . consult-projectile-find-file)
         ("s-e" . consult-projectile-recentf)
         ("s-b" . consult-projectile-switch-to-buffer)))

