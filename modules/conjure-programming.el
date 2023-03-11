;;; conjure-programming.el --- Configurations for Programming with Conjure
;;; Commentary:
;;; Code:
(conjure-require-packages '(editorconfig))

(add-hook 'prog-mode-hook 'smartparens-mode)
(add-hook 'prog-mode-hook 'guru-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(with-eval-after-load 'guru-mode
  (diminish 'guru-mode))

;; highlight diffs in VCS
(global-diff-hl-mode)

(require 'hl-todo)
(global-hl-todo-mode)

;; Less strict guru
(setq guru-warn-only t)

;; use settings from .editorconfig file when present
(require 'editorconfig)
(editorconfig-mode 1)
(diminish 'editorconfig-mode)

;; Colorize compilation buffers like JUnit output
(defun conjure-colorize-compilation-buffer ()
  "Colorize a compilation mode buffer."
  (interactive)
  ;; we don't want to mess with child modes such as grep-mode, ack, ag, etc
  (when (eq major-mode 'compilation-mode)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max)))))

(require 'ansi-color)
(add-hook 'compilation-filter-hook #'conjure-colorize-compilation-buffer)

(provide 'conjure-programming)
;;; conjure-programming.el ends here
