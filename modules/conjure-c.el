;;; conjure-c.el --- cc-mode configuration
;;; Commentary:

;; Handles cc-derived modes, Java, C, PHP...

;;; Code:

(require 'conjure-programming)
(require 'eglot)

(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'java-mode-hook 'eglot-ensure)

(provide 'conjure-c)

;;; conjure-c.el ends here
