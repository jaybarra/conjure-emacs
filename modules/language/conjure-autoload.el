;;; conjure-autoload.el --- Configurations for Autoload in Conjure -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(add-to-list 'major-mode-remap-alist '(json-mode . json-ts-mode))
(add-to-list 'major-mode-remap-alist '(yaml-mode . yaml-ts-mode))

(add-to-list 'auto-mode-alist '("Dockerfile\\(?:\\..*\\)?\\'" . dockerfile-ts-mode))

(elpaca adoc-mode)
(elpaca coffee-mode)
(elpaca elm-mode)
(elpaca markdown-mode)
(elpaca svelte-mode)
(elpaca tuareg) ;; ocaml
(elpaca vue-mode)
(elpaca zig-mode)
(elpaca yaml-mode)

(provide 'conjure-autoload)
;;; conjure-autoload.el ends here
