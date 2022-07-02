;;; init.el --- Conjure Emacs Initialization File
;;; Commentary:
;;; Code:
(defconst emacs-start-time (current-time))
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-linux* (eq system-type 'gnu/linux))

(setq gc-cons-threshold (* 50 1024 1024)
      gc-cons-percentage 0.6)

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

(message "[conjure] Emacs is coming online...")

(setq load-prefer-newer t)

;; Setup directories for splitting out individual configurations
(add-to-list 'load-path conjure-core-dir)
(add-to-list 'load-path conjure-modules-dir)

;; Save customization variables to a separate file
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

(message "[conjure] Invoking the Deep Magic...")
;; Do not change order
(require 'init-packages)
(require 'init-custom)
(require 'init-ui)
(require 'init-common)
(require 'conjure-mode)
(require 'init-editor)
(require 'init-keybindings)

(when *is-a-mac*
  (require 'init-macos))

(when *is-linux*
  (require 'init-linux))

(message "[conjure] Configuring packages...")
;; Enable or disable as needed
(require 'init-company)
(require 'init-clojure)
(require 'init-emacs-lisp)
(require 'init-go)
(require 'init-ivy)
(require 'init-java)
(require 'init-js)
(require 'init-ts)
(require 'init-org)
(require 'init-python)
(require 'init-ruby)
(require 'init-rust)
(require 'init-terraform)
(require 'init-yaml)
(require 'init-xml)

;; One-offs that don't have their own setups yet
(conjure-require-packages '(darkroom
                            dashboard
                            elfeed
                            lorem-ipsum
                            uuidgen
                            yasnippet
                            yasnippet-snippets))

(require 'dashboard)
(setq dashboard-center-content t)
(dashboard-setup-startup-hook)
;;; init.el ends here
