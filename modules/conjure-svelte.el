;;; conjure-svelte.el --- Svelte configuration
;;; Commentary:
;;; Code:
(conjure-require-packages '(svelte-mode))

(require 'svelte-mode)
(add-hook 'svelte-mode-hook #'lsp-deferred)

(provide 'conjure-svelte)
;;; conjure-svelte.el ends here
