;;; conjure-rust.el --- Rust configuration
;;; Commentary:
;;; Code:

(add-hook 'rust-mode-hook #'eglot-ensure)
(add-to-list 'apheleia-formatters '(rustfmt "rustfmt" "--quiet" "--edition" "2021" "--emit" "stdout"))

(provide 'conjure-rust)
;;; conjure-rust.el ends here
