;;; conjure-linux.el --- *Nix specific settings
;;; Commentary:
;;; Code:
(require 'conjure-packages)
(conjure-require-packages '(exec-path-from-shell))

(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

(message "[Conjure] Configuring Linux specific settings...")

(provide 'conjure-linux)

;;; conjure-linux.el ends here
