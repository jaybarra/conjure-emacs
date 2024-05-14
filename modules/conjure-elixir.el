;;; conjure-elixir.el --- Configurations for Elixir in Conjure
;;; Commentary:
;;; Code:

(use-package elixir-ts-mode
  :ensure t
  :defer t
  :hook
  (elixir-ts-mode . (lambda ()
                      (subword-mode +1)
                      (show-smartparens-mode -1)))
  :config
  (require 'eglot)
  (when (executable-find "language_server.sh")
    (add-to-list 'eglot-server-programs
                 '(elixir-ts-mode "language_server.sh"))))

(use-package mix
  :ensure t
  :hook (elixir-ts-mode . mix-minor-mode)
  :bind (:map elixir-ts-mode-map
              ("<f7>" . mix-execute-task)))

(provide 'conjure-elixir)
;;; conjure-elixir.el ends here
