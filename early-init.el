;;; early-init.el --- Early Init for Conjure -*- lexical-binding: t -*-
;;; Commentary:
;;; Early initialization file with optimizations for Alpaca package manager
;;; and startup performance improvements.
;;; Code:

;; Disable package.el in favor of alpaca
(setq package-enable-at-startup nil)

;; Garbage collection optimizations
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

;; Native comp settings
(when (featurep 'native-compile)
  (setq native-comp-deferred-compilation nil)
  (setq native-comp-speed 2))

;; Faster startup by preventing Emacs from loading UI elements early
(setq frame-inhibit-implied-resize t)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq frame-resize-pixelwise t)

;; Prevent loading of X resources
(setq inhibit-x-resources t)

;; Faster startup by disabling bidirectional text scanning
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)

;; Disable UI elements as early as possible
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message user-login-name)
(setq inhibit-splash-screen t)
(setq use-dialog-box nil)

;; File handler optimizations
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Restore file handlers after init
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist default-file-name-handler-alist)
            (setq gc-cons-threshold (* 2 1024 1024)))) ; 2MB

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:

;;; early-init.el ends here
