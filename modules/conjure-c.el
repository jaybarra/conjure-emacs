;;; conjure-c.el --- cc-mode configuration
;;; Commentary:

;; Handles cc-derived modes, Java, C, PHP...

;;; Code:
(conjure-require-packages '(lsp-mode
			    flycheck
			    dap-mode
			    lsp-java))

(defun conjure-c-mode-common-defaults ()
  "Sensible defaults for `c-mode' buffers."

  (setq c-default-style "k&r"
	c-basic-offset 4)
  (c-set-offset 'substatement-open 0))

(setq conjure-c-mode-coommon-hook 'conjure-c-mode-common-defaults)

(add-hook 'c-mode-common-hook (lambda () (run-hooks 'conjure-c-mode-common-hook)))

;; LSP is better for Java than eglot
(add-hook 'java-mode-hook 'lsp-deferred)
(add-hook 'lsp-mode-hook 'lsp-lens-mode)

(with-eval-after-load 'lsp-mode
  (dap-auto-configure-mode))

(provide 'conjure-c)
;;; conjure-c.el ends here
