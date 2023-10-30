;;; conjure-elixir.el --- Elixir configuration
;;; Commentary:
;;; Code:

(require 'eglot)

(use-package elixir-mode
  :defer t
  :hook (elixir-mode . eglot-ensure)
  :init
  (let ((elixir-ls (executable-find "language_server.sh")))
    (when elixir-ls (add-to-list 'eglot-server-programs '(elixir-mode elixir-lsp)))))

(provide 'conjure-elixir)

;;; conjure-elixir.el ends here
