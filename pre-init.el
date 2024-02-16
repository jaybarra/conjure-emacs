;;; pre-init.el --- Configurations for Pre-Init in Conjure -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defconst emacs-start-time (current-time))

(defconst osx-p (eq system-type 'darwin))
(defconst linux-p (eq system-type 'gnu/linux))
(defconst win-p (eq system-type 'windows-nt))

;; Set these during startup and reset after
(setq load-prefer-newer noninteractive)
(setq gc-cons-threshold most-positive-fixnum)

(add-hook 'after-init-hook
          (lambda ()
            (setq load-prefer-newer t)
            (setq gc-cons-threshold (* 16 1024 1024))))

(provide 'pre-init)
;;; pre-init.el ends here
