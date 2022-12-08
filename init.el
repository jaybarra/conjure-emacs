;;; init.el --- Conjure Emacs Initialization
;;; Commentary:
;;; Code:
(defconst emacs-start-time (current-time))
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-linux* (eq system-type 'gnu/linux))

;; Bump GC during startup
(setq gc-cons-threshold (* 50 1024 1024))

(defvar conjure-user
  (getenv (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(defvar conjure-dir (file-name-directory load-file-name)
  "The root dir of the Emacs Conjure distribution.")

(defvar conjure-core-dir (expand-file-name "core" conjure-dir)
  "The home of Conjure's core functionality.")

(defvar conjure-modules-dir (expand-file-name  "modules" conjure-dir)
  "This directory houses all of the built-in Prelude modules.")

(defvar conjure-savefile-dir (expand-file-name "savefile" user-emacs-directory)
  "Folder for storing generated history files.")

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

(message "[Conjure] Emacs is coming online...")

(setq load-prefer-newer t)

;; Setup directories for splitting out individual configurations
(add-to-list 'load-path conjure-core-dir)
(add-to-list 'load-path conjure-modules-dir)

;; Save customization variables to a separate file
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

(message "[Conjure] Invoking the Deep Magic...")
(require 'conjure-packages)
(require 'conjure-custom)
(require 'conjure-ui)
(require 'conjure-editor)
(require 'conjure-global-keybindings)

(when *is-a-mac*
  (require 'conjure-macos))

(require 'vertico)
(setq enable-recursive-minibuffers t)
(setq vertico-cycle t
      vertico-count 15)

(vertico-mode)

;; (require 'vertico-posframe)
;; (vertico-posframe-mode nil)

(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion))))

(require 'marginalia)
(setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
(marginalia-mode)

(savehist-mode)

(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook 'cursor-intangible-mode)

(with-eval-after-load 'corfu
  (setq corfu-cycle t
        corfu-auto t
        corfu-auto-prefix 2
	corfu-auto-delay 0.0
        corfu-quit-at-boundary 'separator
        corfu-preview-current 'insert
	corfu-preselect-first nil)

  (define-key corfu-map (kbd "M-SPC") 'corfu-insert-separator)
  (define-key corfu-map (kbd "TAB") 'corfu-next)
  (define-key corfu-map [tab] 'corfu-next)
  (define-key corfu-map (kbd "S-TAB") 'corfu-previous)
  (define-key corfu-map [backtab] 'corfu-previous)
  (define-key corfu-map (kbd "S-<return>") 'corfu-insert)

  (require 'kind-icon)
  (setq kind-icon-default-face 'corfu-default)
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)

  (add-hook 'eshell-mode-hook (lambda ()
				(setq-local corfu-quit-at-boundary t
					    corfu-quit-no-match t
					    corfu-auto nil)
				(corfu-mode))))

(global-corfu-mode)
(corfu-history-mode)

(require 'tempel)
(defun tempel-setup-capf ()
  "Add tempel Capf to `completion-at-point-functions'."
  (setq-local completion-at-point-functions (cons #'tempel-expand completion-at-point-functions)))

(add-hook 'prog-mode-hook 'tempel-setup-capf)
(add-hook 'text-mode-hook 'tempel-setup-capf)

(global-set-key (kbd "M-+") 'tempel-insert)
(define-key tempel-map [remap keyboard-escape-quit] 'tempel-done)
(define-key tempel-map (kbd "TAB") 'tempel-next)
(define-key tempel-map [tab] 'tempel-next)
(define-key corfu-map (kbd "C-M-i") 'tempel-expand)

(require 'lsp-mode)
(setq lsp-completion-provider :none
      lsp-headerline-breadcrumb-icons-enable nil)
(defun conjure-lsp-mode-defaults ()
  "Sensible `lsp' configuration."
  (setf (alist-get 'style (alist-get 'lsp-capf completion-category-defaults))
	'(orderless)))

(add-hook 'lsp-completion-mode-hook 'conjure-lsp-mode-defaults)

(require 'cape)
(add-to-list 'completion-at-point-functions #'cape-file)
(add-to-list 'completion-at-point-functions #'cape-dabbrev)
(advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
(advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(setq flycheck-display-errors-delay 0.4)
(add-hook 'after-init-hook #'global-flycheck-mode)

(global-set-key (kbd "C-c f") 'conjure-hydra-flycheck/body)

(defhydra conjure-hydra-flycheck (:color blue)
  "
^Flycheck^            ^Errors^             ^Checker^
^--------^------------^------^-------------^-------^---
_q_ quit              _<_ previous         _?_ describe
_M_ manual            _>_ next             _d_ disable
_v_ verify setup      _f_ check            _s_ select
^^                    _l_ list
^^
"
  ("q" nil)
  ("M" flycheck-manual)
  ("v" flycheck-verify-setup)

  ("<" flycheck-previous-error :color pink)
  (">" flycheck-next-error :color pink)
  ("f" flycheck-buffer)
  ("l" flycheck-list-errors)

  ("?" flycheck-describe-checker)
  ("d" flycheck-disable-checker)
  ("s" flycheck-select-checker))

(with-eval-after-load 'magit
  (setq git-commit-fill-column 72
	git-commit-summary-max-length 50))

(global-set-key (kbd "C-x g") 'magit-status)

(projectile-mode)

(require 'conjure-clojure)
(require 'conjure-emacs-lisp)
(require 'conjure-lisp)
(require 'conjure-magit)
(require 'conjure-org)
(require 'conjure-svelte)
(require 'conjure-ts)

(conjure-require-packages '(lsp-mode lsp-java dap-mode))

(require 'dap-mode)
(with-eval-after-load 'lsp-mode
  (setq lsp-treemacs-symbols-sort-function '(lsp-treemacs-sort-by-kind lsp-treemacs-sort-by-name))
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (yas-global-mode)
  (dap-auto-configure-mode))

(add-hook 'java-mode-hook 'lsp-deferred)

(ef-themes-load-random 'dark)

(require 'server)
(unless (server-running-p)
  (server-start))

(provide 'init)
;;; init.el ends here
