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

(global-set-key [remap kill-ring-save] 'easy-kill)

(global-set-key [remap mark-sexp] 'easy-mark)

(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C--") 'er/contract-region)

(global-set-key (kbd "C-;") 'avy-goto-char)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(when conjure-super-keybindings
  (global-set-key(kbd "s-p") 'projectile-command-map))
(global-set-key (kbd "C-c p") 'projectile-command-map)

(global-set-key (kbd "C-.") 'embark-act)
(global-set-key (kbd "M-.") 'embark-dwim)

(global-set-key (kbd "C-x C-SPC") 'consult-global-mark)



(define-key smartparens-mode-map (kbd "C-)") 'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-(") 'sp-forward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-M-)") 'sp-backward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-M-(") 'sp-backward-barf-sexp)

(define-key smartparens-mode-map (kbd "C-M-k") 'sp-kill-sexp)
(define-key smartparens-mode-map (kbd "M-<delete>") 'sp-unwrap-sexp)
(define-key smartparens-mode-map (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)
(define-key smartparens-mode-map (kbd "M-D") 'sp-splice-sexp)

(global-set-key (kbd "C-x g") 'magit-status)

(require 'smartrep)
(smartrep-define-key
    global-map "C-x"
  '(("{" . shrink-window-horizontally)
    ("}" . enlarge-window-horizontally)))

(provide 'conjure-global-keybindings)
;;; conjure-global-keybindings.el ends here
