;;; conjure-macos.el --- Mac specific settings
;;; Commentary:
;;; Code:
(require 'conjure-packages)
(conjure-require-packages '(exec-path-from-shell))

(require 'exec-path-from-shell)
(dolist (var '("JAVA_HOME"))
  (add-to-list 'exec-path-from-shell-variables var))
(exec-path-from-shell-initialize)

;; Allow GPG to decrypt gpg file
(setf epa-pinentry-mode 'loopback)

(setq ns-function-modifier 'hyper)

;; OSX ls doesn't support --dired
(setq dired-use-ls-dired nil)

;; smoother scrolling
(setq mouse-wheel-scroll-amount '(1
                                  ((shift) . 5)
                                  ((control))))

(when (fboundp 'set-fontset-font)
  (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))

;; don't bother hiding the menu-bar
(when (display-graphic-p) (menu-bar-mode +1))

(message "[Conjure] Configuring MacOS specific settings...")

(provide 'conjure-macos)

;;; conjure-macos.el ends here
