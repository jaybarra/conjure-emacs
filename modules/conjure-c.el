;;; conjure-c.el --- cc-mode configuration
;;; Commentary:

;; Handles cc-derived modes, Java, C, PHP...

;;; Code:
(defun conjure-c-mode-common-defaults ()
  "Sensible defaults for `c-mode' buffers."

  (setq c-default-style "k&r"
	c-basic-offset 4)
  (c-set-offset 'substatement-open 0))

(setq conjure-c-mode-common-hook 'conjure-c-mode-common-defaults)

(add-hook 'c-mode-common-hook (lambda () (run-hooks 'conjure-c-mode-common-hook)))

(add-hook 'java-mode-hook 'eglot-ensure)
 
(provide 'conjure-c)
;;; conjure-c.el ends here
