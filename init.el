;;; -*- mode: emacs-lisp -*-

;;; * Debug

;; (setq debug-on-error t)

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

(when (eq system-type 'gnu/linux)
  (require 'conjure-linux))

(when (eq system-type 'darwin)
  (require 'conjure-macos))

;; WSL (windows subsystem for linux) specific setting
(when (and (eq system-type 'gnu/linux) (getenv "WSLENV"))
  (require 'conjure-wsl))

(when (eq system-type 'windows-nt)
  (require 'conjure-windows))

(message "[Conjure] Loading Conjure's additional modules...")

;; Conjure's modules
(if (file-exists-p conjure-modules-file)
    (load conjure-modules-file)
  (message "[Conjure] Missing personal modules file %s" conjure-modules-file)
  (message "[Conjure] Falling back to the default example file sample/conjure-modules.el")
  (message "[Conjure] Copy the sample to your personal configuration folder [%s] to change behaviors" conjure-modules-dir)
  (load (expand-file-name "sample/")))

(setq custom-file (expand-file-name "custom.el" conjure-personal-dir))

;; load personal settings
(when (file-exists-p conjure-personal-dir)
  (message "[Conjure] Loading personal configuration files in %s..." conjure-personal-dir)
  (mapc 'load (delete
               conjure-modules-file
               (directory-files conjure-personal-dir 't "^[^#\.].*\\.el$"))))

(message "[Conjure] Conjure is primed and ready...")
