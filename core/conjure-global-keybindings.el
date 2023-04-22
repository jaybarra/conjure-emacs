;;; conjure-global-keybindings.el --- Conjure Global Keybindings
;;; Commentary:
;;; Code:

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "<f5>") 'revert-buffer)

(global-set-key (kbd "M-z") 'zop-up-to-char)
(global-set-key (kbd "M-Z") 'zop-to-char)

(global-set-key (kbd "s-w") 'ace-window)
(global-set-key [remap other-window] 'ace-window)

(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

(global-set-key (kbd "s-y") 'browse-kill-ring)

(with-eval-after-load 'easy-kill
  (global-set-key [remap kill-ring-save] 'easy-kill))

(with-eval-after-load 'easy-mark
  (global-set-key [remap mark-sexp] 'easy-mark))

(with-eval-after-load 'expand-region
  (global-set-key (kbd "C-=") 'er/expand-region)
  (global-set-key (kbd "C--") 'er/contract-region))

(with-eval-after-load 'avy
  (global-set-key (kbd "C-;") 'avy-goto-char))

(with-eval-after-load 'ibuffer
  (global-set-key (kbd "C-x C-b") 'ibuffer))

(with-eval-after-load 'projectile
  (when conjure-super-keybindings
    (global-set-key(kbd "s-p") 'projectile-command-map))

  (global-set-key (kbd "C-c p") 'projectile-command-map)
  (define-key projectile-mode-map [remap projectile-switch-project] 'consult-projectile-switch-project)
  (define-key projectile-mode-map [remap projectile-find-file] 'consult-projectile-find-file))

(with-eval-after-load 'embark
  (global-set-key (kbd "C-.") 'embark-act)
  (global-set-key (kbd "M-.") 'embark-dwim))

(global-set-key (kbd "C-x C-SPC") 'consult-global-mark)

;; prefer ripgrep
(cond ((executable-find "rg") (global-set-key (kbd "C-s-f") 'consult-ripgrep))
      (t (global-set-key (kbd "C-s-f") 'consult-grep)))

(define-key smartparens-mode-map (kbd "C-)") 'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-(") 'sp-forward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-M-)") 'sp-backward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-M-(") 'sp-backward-barf-sexp)

(define-key smartparens-mode-map (kbd "C-M-k") 'sp-kill-sexp)
(define-key smartparens-mode-map (kbd "M-<delete>") 'sp-unwrap-sexp)
(define-key smartparens-mode-map (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)
(define-key smartparens-mode-map (kbd "M-D") 'sp-splice-sexp)

(with-eval-after-load 'magit
  (global-set-key (kbd "C-x g") 'magit-status))

(require 'smartrep)
(smartrep-define-key
    global-map "C-x"
  '(("{" . shrink-window-horizontally)
    ("}" . enlarge-window-horizontally)))

(provide 'conjure-global-keybindings)
;;; conjure-global-keybindings.el ends here
