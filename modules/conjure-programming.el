;;; conjure-programming.el --- Configurations for Programming with Conjure
;;; Commentary:
;;; Code:

(conjure-require-packages '(rainbow-delimiters))

(defun conjure-local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t))

;; show the current function def in the modeline
(require 'which-func)
(which-function-mode 1)

;; font-lock annotations like TODO
(require 'hl-todo)
(global-hl-todo-mode 1)

;; Less strict guru
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
  (conjure-local-comment-auto-fill))

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

(add-to-list 'pulsar-pulse-functions 'flymake-goto-next-error)
(add-to-list 'pulsar-pulse-functions 'flymake-goto-prev-error)

(provide 'conjure-programming)

;;; conjure-programming.el ends here
