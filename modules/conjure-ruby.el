;;; conjure-ruby.el --- Ruby configuration
;;; Commentary:
;;; Code:

(require 'conjure-programming)

(conjure-require-packages '(inf-ruby))

;; ignore ruby bytecode
(add-to-list 'completion-ignored-extensions ".rbc")

(with-eval-after-load 'ruby-mode
  (defun conjure-ruby-mode-defaults ()
    "Sensible defaults for `ruby-mode'."
    (setq ruby-insert-encoding-magic-comment nil)

    (inf-ruby-minor-mode +1)
    ;; camel-case aware editing
    (subword-mode +1))

  (setq conjure-ruby-mode-hook 'conjure-ruby-mode-defaults)

  (add-hook 'ruby-mode-hook
            (lambda ()
	      (run-hooks 'conjure-ruby-mode-hook))))

(provide 'conjure-ruby)

;;; conjure-ruby.el ends here
