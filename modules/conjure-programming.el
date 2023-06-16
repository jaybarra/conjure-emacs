;;; conjure-programming.el --- Configurations for Programming with Conjure
;;; Commentary:
;;; Code:

(require 'conjure-packages)
(conjure-require-packages '(rainbow-delimiters))

(require 'eglot)
(defun conjure-eglot-format-on-save ()
  "Format buffer using eglot."
  ;; TODO prettier and eglot can sometimes fight
  (when conjure-format-on-save (eglot-format-buffer)))

(defun conjure-local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t))

;; show the current function def in the modeline
(require 'which-func)
(which-function-mode 1)

;; font-lock annotations like TODO
(require 'hl-todo)
(global-hl-todo-mode 1)

;; Less strict guru
(require 'guru-mode)
(setq guru-warn-only t)

(defun conjure-prog-mode-defaults ()
  "Default coding hook actions."
  (when (and (executable-find ispell-program-name)
             conjure-flyspell)
    ;; only spell-check inside comments
    (flyspell-prog-mode))

  (when conjure-guru
    (guru-mode +1)
    (diminish 'guru-mode))

  (smartparens-mode +1)
  (rainbow-delimiters-mode +1)
  (conjure-enable-whitespace)
  (conjure-local-comment-auto-fill)

  (add-hook 'eglot-managed-mode-hook
	    (lambda ()
	      (add-hook 'before-save-hook 'conjure-eglot-format-on-save) t t)))

(setq conjure-prog-mode-hook 'conjure-prog-mode-defaults)

(add-hook 'prog-mode-hook (lambda () (run-hooks 'conjure-prog-mode-hook)))
(add-hook 'prog-mode-hook 'flymake-mode)

;; flycheck style commands for flymake
(define-key prog-mode-map (kbd "C-c ! n") 'flymake-goto-next-error)
(define-key prog-mode-map (kbd "C-c ! p") 'flymake-goto-prev-error)
(define-key prog-mode-map (kbd "C-c ! c") 'flymake-start)

(define-key text-mode-map (kbd "C-c ! n") 'flymake-goto-next-error)
(define-key text-mode-map (kbd "C-c ! p") 'flymake-goto-prev-error)
(define-key text-mode-map (kbd "C-c ! c") 'flymake-start)

(require 'pulsar)
(add-to-list 'pulsar-pulse-functions 'flymake-goto-next-error)
(add-to-list 'pulsar-pulse-functions 'flymake-goto-prev-error)

(provide 'conjure-programming)

;;; conjure-programming.el ends here
