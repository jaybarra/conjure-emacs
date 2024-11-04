;;; conjure-elixir.el --- Configurations for Elixir in Conjure -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package elixir-ts-mode
  :ensure t)
(use-package heex-ts-mode
  :ensure t)

(use-package mix
  :ensure t
  :hook ((elixir-mode elixir-ts-mode) . mix-minor-mode))

(use-package exunit
  :ensure t
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
(add-to-list 'eglot-server-programs '(elixir-mode "~/.local/share/elixir-ls/language_server.sh"))
(add-to-list 'eglot-server-programs '(elixir-ts-mode "~/.local/share/elixir-ls/language_server.sh"))
(add-to-list 'eglot-server-programs '(heex-ts-mode "~/.local/share/elixir-ls/language_server.sh"))

(add-hook 'elixir-ts-mode-hook #'eglot-ensure)
(add-hook 'elixir-mode-hook #'eglot-ensure)
(add-hook 'heex-ts-mode-hook #'eglot-ensure)

(provide 'conjure-elixir)
;;; conjure-elixir.el ends here
