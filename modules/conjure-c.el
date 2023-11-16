;;; conjure-c.el --- cc-mode configuration
;;; Commentary:

;; Handles cc-derived modes, Java, C, PHP...

;;; Code:

(require 'conjure-programming)

(defun conjure-c-mode-common-defaults ()
  "Sensible defaults for programming in C languages."
  (message "[Conjure] looky here, we're editing C!")
  (c-set-offset 'substatement-open 0))

(setq conjure-c-mode-common-hook 'conjure-c-mode-common-defaults)

(mapcar (lambda (mode-hook) (add-hook mode-hook (lambda () (run-hooks 'conjure-c-mode-common-hook))))
        '(c-mode-common-hook c-ts-mode-hook c++-ts-mode-hook))

;; Makefiles
(defun conjure-makefile-mode-defaults ()
  "Sensible defaults for working with Makfiles."
  (whitespace-toggle-options '(tabs))
  (setq indent-tabs-mode t))

(setq conjure-makefile-mode-hook 'conjure-makefile-mode-defaults)

(add-hook 'makefile-mode-hook (lambda () (run-hooks 'conjure-makefile-mode-hook)))

;; use treesitter
(when (treesit-available-p)
  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
  (add-to-list 'major-mode-remap-alist
               '(c-or-c++-mode . c-or-c++-ts-mode)))

(provide 'conjure-c)

;;; conjure-c.el ends here
