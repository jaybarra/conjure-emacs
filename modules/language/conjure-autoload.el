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

(use-package adoc-mode :ensure t)
(use-package coffee-mode :ensure t)
(use-package elm-mode :ensure t)
(use-package markdown-mode :ensure t)
(use-package svelte-mode :ensure t)
(use-package tuareg :ensure t) ;; ocaml
(use-package vue-mode :ensure t)
(use-package zig-mode :ensure t)
(use-package yaml-mode :ensure t)
(use-package git-modes :ensure t)

(provide 'conjure-autoload)
;;; conjure-autoload.el ends here
