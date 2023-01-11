;;; conjure-macos.el --- macos specific functions
;;; Commentary:
;;; Code:
(conjure-require-packages '(exec-path-from-shell))

(require 'exec-path-from-shell)


(dolist (var '("GEM_ROOT" "GEM_HOME" "GEM_PATH"))
  (add-to-list 'exec-path-from-shell-variables var))
(exec-path-from-shell-initialize)

;; Allow GPG to decrypt gpg file
(setf epa-pinentry-mode 'loopback)

(setq ns-function-modifier 'hyper
      dired-use-ls-dired nil)

(setq mouse-wheel-scroll-amount '(1
                                  ((shift) . 5)
                                  ((control))))

;; don't bother hiding the menu-bar for Mac
(menu-bar-mode +1)

(when (fboundp 'set-fontset-font)
  (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))

(message "[Conjure] Configuring MacOS specific settings...")

(provide 'conjure-macos)
;;; conjure-macos.el ends here
