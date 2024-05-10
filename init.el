(load (expand-file-name (concat user-emacs-directory "pre-init")))

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
(package-initialize)

(require 'use-package)
(setq use-package-verbose t)

(defun conjure-add-subfolders-to-load-path (parent-dir)
  "Add all level PARENT-DIR subdirs to the `load-path'."
  (dolist (f (directory-files parent-dir))
    (let ((name (expand-file-name f parent-dir)))
      (when (and (file-directory-p name)
                 (not (string-prefix-p "." f)))
        (add-to-list 'load-path name)
        (conjure-add-subfolders-to-load-path name)))))

(add-to-list 'load-path conjure-core-dir)
(add-to-list 'load-path conjure-modules-dir)
(add-to-list 'load-path conjure-vendor-dir)
(conjure-add-subfolders-to-load-path conjure-vendor-dir)

;; preload personal settings from `conjure-personal-preload-dir'
(when (file-exists-p conjure-personal-dir)
  (message "[Conjure] Loading personal configuration files in %s..." conjure-personal-preload-dir)
  (mapc 'load (directory-files conjure-personal-preload-dir 't "^[^#\.].*el$")))

(message "[Conjure] Loading Conjure's core modules...")

;; Core behaviors and configuration
(require 'conjure-custom)
(require 'conjure-ui)
(require 'conjure-editor)
(require 'conjure-global-keybindings)

;; General programming configuration
(require 'conjure-programming)

;; Language specific sonfigurations
(require 'conjure-lisp)
(require 'conjure-clojure)
(require 'conjure-coffee)
(require 'conjure-elixir)
(require 'conjure-emacs-lisp)
(require 'conjure-groovy)
(require 'conjure-python)
(require 'conjure-php)
(require 'conjure-rust)
(require 'conjure-svelte)
(require 'conjure-terraform)
(require 'conjure-vue)
(require 'conjure-zig)

;; ;; Specialty and derived modes
(require 'conjure-data-science)
(require 'conjure-nextflow)

;; Org Mode
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
  :bind (;; C-x bindings (ctl-x-map)
         ([remap bookmark-jump] . consult-bookmark)
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
         ("M-g r" . consult-recent-file)
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
  
  (setq consult-narrow-key "<"
        consult-line-numbers-widen t
        consult-async-min-input 2
        consult-async-refresh-delay 0.15
        consult-async-input-throttle 0.2
        consult-async-input-debounce 0.1
        xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  (add-hook 'xref-after-jump-hook (lambda() (run-hooks 'consult-after-jump-hook))))

(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . 'consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . 'consult-dir)
         ("C-x C-j" . 'consult-dir-jump-file)))

(use-package consult-eglot
  :ensure t
  :defer t
  :bind (:map eglot-mode-map
              ([remap xref-find-apropos] . consult-eglot-symbols)))

(use-package consult-flycheck :ensure t)

(use-package xref
  :ensure nil
  :init
  (setq xref-search-program (cond
			                 ((executable-find "ugrep") 'ugrep)
                             ((executable-find "rg") 'ripgrep)
			                 (t 'grep))))

(use-package tempel
  :ensure t
  :bind (("M-+" . tempel-complete)
         ("M-*" . tempel-insert)
         :map tempel-map
         ([tab] . tempel-next))
  :init
  (defun conjure-tempel-setup-capf()
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  :config
  (add-hook 'conf-mode-hook 'conjure-tempel-setup-capf)
  (add-hook 'prog-mode-hook 'conjure-tempel-setup-capf)
  (add-hook 'text-mode-hook 'conjure-tempel-setup-capf))

(use-package tempel-collection :ensure t)

;; Corfu - completion
(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-auto-delay 0.18)
  (corfu-auto-prefix 2)
  (corfu-auto t)
  (corfu-preselect 'prompt)
  (corfu-count 16)
  (corfu-quit-at-boundary 'separator t)
  :config
  (setq tab-always-indent 'complete)
  (global-corfu-mode))

(use-package cape
  :ensure t
  :init
  (add-hook 'prog-mode-hook
            (lambda()
              (add-hook 'completion-at-point-functions #'cape-file -10 t)))
  (mapcar (lambda (my-mode) 
            (add-hook my-mode
                      (lambda()
                        (add-hook 'completion-at-point-functions #'cape-dabbrev 20 t))))
          '(prog-mode-hook
            text-mode-hook
            conf-mode-hook
            eshell-mode-hook
            comint-mode-hook
            minibuffer-setup-hook)))

;; Marginalia
(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init
  (marginalia-mode))

;; Orderless
(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles orderless partial-completion)))
        orderless-component-separator #'orderless-escapable-split-on-space)
  (set-face-attribute 'completions-first-difference nil :inherit nil))

(use-package embark
  :ensure t
  :bind (([remap describe-bindings] . embark-bindings)
         ("C-;" . embark-act)
         :map minibuffer-local-map
         ("C-;" . embark-act)
         ("C-c C-;" . embark-export)
         ("C-c C-l" . embark-collect))
  :init
  (setq which-key-use-C-h-commands nil
        prefix-help-command #'embark-prefix-help-command))

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

(use-package consult-project-extra
  :ensure t
  :bind
  (("C-c p f" . consult-project-extra-find)
   ("C-c p o" . consult-project-extra-find-other-window)))
