;;; conjure-svelte.el --- Svelte configuration
;;; Commentary:
;;; Code:

(require 'conjure-programming)

(conjure-require-packages '(svelte-mode))

(require 'svelte-mode)
(add-hook 'svelte-mode-hook 'eglot-ensure)

(provide 'conjure-svelte)
;;; conjure-svelte.el ends here
