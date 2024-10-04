;;; conjure-rust.el --- Configurations for Rust in Conjure -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package rust-mode
  :hook ((rust-mode rust-ts-mode) . eglot-ensure)
  :config
  (setq rust-format-on-save t))

(provide 'conjure-rust)
;;; conjure-rust.el ends here
