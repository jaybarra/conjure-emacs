;;; conjure-c.el --- cc-mode configuration
;;; Commentary:

;; Handles cc-derived modes, Java, C, PHP...

;;; Code:
(require 'conjure-packages)
(conjure-require-packages '(eglot-java))

(require 'eglot)
(require 'eglot-java)
(require 'cc-vars)

(defun conjure-c-mode-common-defaults ()
  "Sensible defaults for `c-mode' buffers."

  (setq c-default-style "k&r"
	c-basic-offset 4)
  (c-set-offset 'substatement-open 0))

(defvar conjure-c-mode-common-hook nil)
(setq conjure-c-mode-common-hook 'conjure-c-mode-common-defaults)

(add-hook 'c-mode-common-hook (lambda () (run-hooks 'conjure-c-mode-common-hook)))

(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)

;; special java configs
(add-hook 'java-mode-hook 'eglot-java-mode)
(add-hook 'eglot-java-mode-hook
	  (lambda ()
	    (define-key eglot-java-mode-map (kbd "C-c l n") #'eglot-java-file-new)
	    (define-key eglot-java-mode-map (kbd "C-c l x") #'eglot-java-run-main)
	    (define-key eglot-java-mode-map (kbd "C-c l t") #'eglot-java-run-test)
	    (define-key eglot-java-mode-map (kbd "C-c l N") #'eglot-java-project-new)
	    (define-key eglot-java-mode-map (kbd "C-c l T") #'eglot-java-project-build-task)
	    (define-key eglot-java-mode-map (kbd "C-c l R") #'eglot-java-project-build-refresh)))
 
(provide 'conjure-c)
;;; conjure-c.el ends here
