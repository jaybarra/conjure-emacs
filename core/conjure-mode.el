;;; conjure-mode.el --- Emacs Conjure: minor mode
;;; Commentary:
;;; Code:

(require 'easymenu)
(use-package imenu-anywhere)
(use-package crux)

(defvar conjure-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c i") 'imenu-anywhere)
    map)
  "Keymap for Conjure mode")

(define-minor-mode conjure-mode
  "Minor mode to consolidate extensions.

\\{conjure-mode-map}"
  :lighter " Conj"
  :keymap conjure-mode-map
  :global t)

(provide 'conjure-mode)

;;; conjure-mode.el ends here
