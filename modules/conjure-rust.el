;;; conjure-rust.el --- Rust configuration
;;; Commentary:
;;; Code:

(require 'conjure-programming)

(conjure-require-packages '(cargo eglot rust-mode))

(with-eval-after-load 'rust-mode
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

(add-hook 'rust-mode-hook 'eglot-ensure)

(provide 'conjure-rust)

;;; conjure-rust.el ends here
