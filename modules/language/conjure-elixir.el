;;; conjure-elixir.el --- Configurations for Elixir in Conjure -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(elpaca elixir-ts-mode)

(defun conjure--setup-elixir-defaults ()
  "Setup sensible defaults for `elixir-mode'."
  (smartparens-strict-mode -1))

(add-hook 'elixir-ts-mode-hook #'conjure--setup-elixir-defaults)
(add-hook 'elixir--mode-hook #'conjure--setup-elixir-defaults)

(require 'eglot)
(add-to-list 'eglot-server-programs '(elixir-mode "~/.local/elixir-ls/language_server.sh"))
(add-to-list 'eglot-server-programs '(elixir-ts-mode "~/.local/elixir-ls/language_server.sh"))
(add-to-list 'eglot-server-programs '(heex-ts-mode "~/.local/elixir-ls/language_server.sh"))

(add-hook 'elixir-ts-mode-hook #'eglot-ensure)
(add-hook 'elixir-mode-hook #'eglot-ensure)
(add-hook 'heex-ts-mode-hook #'eglot-ensure)

(provide 'conjure-elixir)
;;; conjure-elixir.el ends here
