;;; init.el --- Configurations for Conjure -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(message "[Conjure] Emacs is coming online...")

(defvar conjure-user (getenv (if (equal system-type 'windows-nt) "USERNAME" "USER"))
  "Current system username.")
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
(defvar conjure-custom-file (expand-file-name "custom.el" conjure-savefile-dir)
  "The root dir of the Emacs Conjure distribution.")

;; Load the core setup
(load (expand-file-name "conjure-custom.el" conjure-core-dir))
(load (expand-file-name "conjure-core.el" conjure-core-dir))
(load (expand-file-name "conjure-ui.el" conjure-core-dir))
(load (expand-file-name "conjure-editor.el" conjure-core-dir))

(defun load-directory (dir)
  "Load all Emacs Lisp files in DIR."
  (dolist (file (directory-files-recursively dir "\\.el$"))
    (load file)))

;; Load modules - make it more fun
(dolist (subdir '("os" "tools" "language"))
  (load-directory (expand-file-name subdir conjure-modules-dir)))

;; Set custom-file and load it
(setq custom-file (expand-file-name "custom.el" conjure-savefile-dir))
(when (file-exists-p custom-file)
  (load custom-file))

;; init.el ends here
