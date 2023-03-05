;;; conjure-elixir.el --- Elixir configuration
;;; Commentary:
;;; Code:
(conjure-require-packages '(elixir-mode))

(require 'eglot)

(when (executable-find "elixir-ls")
  (add-to-list 'eglot-server-programs '(elixir-mode "elixir-ls"))
  (add-hook 'elixir-mode-hook #'eglot-ensure))

(add-hook 'elixir-mode-hook (lambda () (apheleia-mode -1)))
(add-hook 'before-save-hook #'elixir-format)

(provide 'conjure-elixir)
;;; conjure-elixir.el ends here
