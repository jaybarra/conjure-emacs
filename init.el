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

;; Load modules
(dolist (module '("os" "tools" "language"))
  (let ((module-dir (expand-file-name module conjure-modules-dir)))
    (when (file-directory-p module-dir)
      (conjure-load-directory module-dir t))))

;; Set and load custom file
(setq custom-file conjure-custom-file)
(when (file-exists-p custom-file)
  (conjure-load-file custom-file))

(provide 'init)
;;; init.el ends here
