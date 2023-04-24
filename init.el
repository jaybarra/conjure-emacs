;;; init.el --- Conjure Emacs Initialization
;;; Commentary:
;;; Code:
(defconst emacs-start-time (current-time))
(defconst osx-p (eq system-type 'darwin))
(defconst linux-p (eq system-type 'gnu/linux))

(message "[Conjure] Emacs is coming online...")

(setq load-prefer-newer t)

(defvar conjure-user
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

;; Setup directories for splitting out individual configurations
(add-to-list 'load-path conjure-core-dir)
(add-to-list 'load-path conjure-modules-dir)
(add-to-list 'load-path conjure-vendor-dir)
(conjure-add-subfolders-to-load-path conjure-vendor-dir)

;; let flymake know where things are
(with-eval-after-load 'flymake
  (setq elisp-flymake-byte-compile-load-path load-path))

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

(message "[Conjure] Invoking the Deep Magic...")

(require 'conjure-packages)
(require 'conjure-custom)
(require 'conjure-ui)
(require 'conjure-common)
(require 'conjure-editor)
(require 'conjure-global-keybindings)

(when osx-p (require 'conjure-macos))

(message "[Conjure] Loading Conjure's modules...")

(if (file-exists-p conjure-modules-file)
    (load conjure-modules-file)
  (message "[Conjure] Missing personal modules file %s" conjure-modules-file)
  (message "[Conjure] Falling back to default")
  (message "[Conjure] You should copy the default to your personal configuration folder and tweak to your liking")
  (load (expand-file-name "sample/conjure-modules.el" conjure-dir)))

;; Save customization variables to a separate file
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

(when (file-exists-p conjure-personal-dir)
  (message "[Conjure] Loading personal configuration files in %s..." conjure-personal-dir)
  (mapc 'load (delete conjure-modules-file (directory-files conjure-personal-dir 't "^[^#\.].*\\.el$"))))

(message "[Conjure] Conjure is ready to make magic!")

;;; init.el ends here
