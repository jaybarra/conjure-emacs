;;; conjure-macos.el --- macos specific functions
;;; Commentary:
;;; Code:
(conjure-require-packages '(exec-path-from-shell))

(require 'exec-path-from-shell)

(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match
that used by the user's shell.

This is particularly useful under Mac OS X and macOS, where GUI
apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string
			  "[ \t\n]*$" "" (shell-command-to-string
					  "$SHELL --login -c 'echo $PATH'"))))
    ;;(message "Handle custom PATH here")
    ))

(set-exec-path-from-shell-PATH)

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
