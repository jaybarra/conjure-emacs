;;; conjure-programming.el --- Configurations for Programming with Conjure
;;; Commentary:
;;; Code:

(set-language-environment 'utf-8)

(use-package rainbow-mode
  :diminish
  :hook prog-mode)

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

;; (use-package prettier
;;   :hook prog-mode)

(use-package rainbow-delimiters
  :hook prog-mode)

(use-package guru-mode
  :diminish
  :hook after-init)

(use-package smartparens
  :diminish
  :hook prog-mode)

(defun conjure-prog-mode-defaults ()
  "Default coding hook actions."
  (when (and (executable-find ispell-program-name)
             conjure-flyspell)
    ;; only spell-check inside comments
    (flyspell-prog-mode))
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

(use-package pulsar
  :config
  (add-to-list 'pulsar-pulse-functions 'flymake-goto-next-error)
  (add-to-list 'pulsar-pulse-functions 'flymake-goto-prev-error))

(use-package treesit-auto
  :config
  (global-treesit-auto-mode))

(provide 'conjure-programming)

;;; conjure-programming.el ends here
