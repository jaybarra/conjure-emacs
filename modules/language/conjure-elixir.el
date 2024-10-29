;;; conjure-elixir.el --- Configurations for Elixir in Conjure -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; NOTE: `alchemist' is outdated and relies on `company'

(use-package elixir-ts-mode)
(use-package heex-ts-mode)

(use-package mix
  :hook ((elixir-mode elixir-ts-mode) . mix-minor-mode))

(use-package exunit
  :hook ((elixir-mode elixir-ts-mode) . exunit-mode))

(defun conjure--setup-elixir-defaults ()
  "Setup sensible defaults for `elixir-mode'."
  (setq-local comment-start "# "
              comment-end ""
              fill-column 98)

  (smartparens-strict-mode -1)
  (subword-mode +1))

(add-hook 'elixir-ts-mode-hook #'conjure--setup-elixir-defaults)
(add-hook 'elixir-mode-hook #'conjure--setup-elixir-defaults)

(require 'eglot)
(add-to-list 'eglot-server-programs '(elixir-mode "~/workspace/elixir-ls/out/language_server.sh"))
(add-to-list 'eglot-server-programs '(elixir-ts-mode "~/workspace/elixir-ls/out/language_server.sh"))
(add-to-list 'eglot-server-programs '(heex-ts-mode "~/workspace/elixir-ls/out/language_server.sh"))

(add-hook 'elixir-ts-mode-hook #'eglot-ensure)
(add-hook 'elixir-mode-hook #'eglot-ensure)
(add-hook 'heex-ts-mode-hook #'eglot-ensure)


;; Make specific Elixir keywords appear in italics
(defface elixir-script-face
  '((t (:inherit font-lock-keyword-face :slant italic)))
  "Face for Elixir keywords that should appear in script/italics.")

;; Define different faces for different types of keywords
(defface elixir-module-face
  '((t (:inherit font-lock-keyword-face :slant italic)))
  "Face for module-related keywords like defmodule, import, alias")

(defface elixir-definition-face
  '((t (:inherit font-lock-keyword-face :slant italic)))
  "Face for definition keywords like def, defp")

;; Apply different faces to different keyword groups
(font-lock-add-keywords
 'elixir-ts-mode
 '(("\\<\\(import\\|alias\\|use\\|require\\|defmodule\\)\\>" 1 'elixir-module-face)
   ("\\<\\(def\\|defp\\|defmacro\\|defmacrop\\)\\>" 1 'elixir-definition-face)))

(provide 'conjure-elixir)
;;; conjure-elixir.el ends here
