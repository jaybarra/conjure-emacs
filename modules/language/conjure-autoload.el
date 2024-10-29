;;; conjure-autoload.el --- Configurations for Autoload in Conjure -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq major-mode-remap-alist
      '((yaml-mode . yaml-ts-mode)
        ;; (json-mode . json-ts-mode) ;; being picked up by jsmode
        ))

;; auto-mode list
(add-to-list 'auto-mode-alist '(".json\\'" . json-ts-mode))
(add-to-list 'auto-mode-alist '("Dockerfile\\(?:\\..*\\)?\\'" . dockerfile-ts-mode))

(use-package adoc-mode)
(use-package coffee-mode)
(use-package elm-mode)
(use-package markdown-mode)
(use-package svelte-mode)
(use-package tuareg) ;; ocaml
(use-package vue-mode)
(use-package zig-mode)
(use-package yaml-mode)
(use-package git-modes)

(provide 'conjure-autoload)
;;; conjure-autoload.el ends here
