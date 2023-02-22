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
(require 'conjure-common)
(require 'conjure-custom)
(require 'conjure-editor)

(when *is-a-mac*
  (require 'conjure-macos))

(require 'conjure-ui)
(require 'conjure-global-keybindings)

(require 'vertico)
(setq enable-recursive-minibuffers t)
(setq vertico-cycle t
      vertico-count 15)

(vertico-mode)

(require 'vertico-posframe)
(setq vertico-multiform-commands
      '((consult-projectile-switch-project posframe (vertico-posframe-handler . posframe-poshandler-frame-center))
	(consult-projectile-find-file posframe (vertico-posframe-handler . posframe-poshandler-frame-center))
        (consult-projectile-recentf posframe (vertico-posframe-handler . posframe-poshandler-frame-center))
	(consult-recent-file posframe (vertico-posframe-handler . posframe-poshandler-frame-center))
	(org-roam-node-find posframe (vertico-posframe-handler . posframe-poshandle-frame-center))
	(org-roam-node-insert posframe (vertico-posframe-handler . posframe-poshandle-frame-center))
	(org-roam-insert-node-immediate posframe (vertico-posframe-handler . posframe-poshandle-frame-center))))
(vertico-multiform-mode 1)

(require 'orderless)
(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles . (partial-completion)))))

(require 'affe)
(setq affe-regexp-compiler #'orderless-pattern-compiler
      affe-highlight-function #'orderless-highlight-matches)

(require 'marginalia)
(setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
(marginalia-mode)

(savehist-mode)

(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(require 'corfu)
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
(require 'svg-lib)
(unless (image-type-available-p 'svg) (setq kind-icon-use-icons nil))

(add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)

(add-hook 'eshell-mode-hook (lambda ()
			      (setq-local corfu-quit-at-boundary t
					  corfu-quit-no-match t
					  corfu-auto nil)
			      (corfu-mode)))

(global-corfu-mode)
(corfu-history-mode)

(require 'cape)
(add-to-list 'completion-at-point-functions #'cape-file)
(add-to-list 'completion-at-point-functions #'cape-dabbrev)
(advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
(advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(with-eval-after-load 'magit
  (setq git-commit-fill-column 72
	git-commit-summary-max-length 50))

(projectile-mode)

(require 'conjure-datatypes)
(require 'conjure-clojure)
(require 'conjure-emacs-lisp)
(require 'conjure-lisp)
(require 'conjure-magit)
(require 'conjure-org)
(require 'conjure-svelte)
(require 'conjure-c)
(require 'conjure-ts)
(require 'conjure-js)
(require 'conjure-python)
(require 'conjure-ruby)
(require 'conjure-web)
(require 'conjure-yaml)

(add-hook 'prog-mode-hook (lambda () (dap-mode +1)))

(require 'server)
(unless (server-running-p)
  (server-start))

(provide 'init)
;;; init.el ends here
