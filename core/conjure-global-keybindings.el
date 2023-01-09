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

(browse-kill-ring-default-keybindings)
(global-set-key (kbd "s-y") 'browse-kill-ring)

(global-set-key [remap kill-ring-save] 'easy-kill)
(global-set-key [remap mark-sexp] 'easy-mark)

(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C--") 'er/contract-region)

(global-set-key (kbd "C-;") 'avy-goto-char)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(require 'projectile)
(when conjure-super-keybindings
  (global-set-key(kbd "s-p") 'projectile-command-map))
(global-set-key (kbd "C-c p") 'projectile-command-map)
(define-key projectile-mode-map [remap projectile-switch-project] 'consult-projectile-switch-project)
(define-key projectile-mode-map [remap projectile-find-file] 'consult-projectile-find-file)

(global-set-key (kbd "C-.") 'embark-act)
(global-set-key (kbd "M-.") 'embark-dwim)

(global-set-key (kbd "C-x C-SPC") 'consult-global-mark)
(global-set-key (kbd "C-s-f") 'consult-ag)
(global-set-key (kbd "C-s") 'consult-line)
(global-set-key (kbd "C-x b") 'consult-buffer)
(global-set-key (kbd "C-x r j") 'consult-register)
(global-set-key (kbd "C-x r l") 'consult-bookmark)
(global-set-key (kbd "C-x m") 'consult-yasnippet)
(global-set-key (kbd "C-x c i") 'consult-imenu)
(global-set-key (kbd "C-x j") 'consult-recent-file)
(global-set-key (kbd "C-c h") 'consult-history)
(global-set-key (kbd "M-g g") 'consult-goto-line)
(global-set-key (kbd "M-g M-g") 'consult-goto-line)
(global-set-key (kbd "M-g m") 'consult-mark)
(global-set-key (kbd "M-g i") 'consult-imenu)

;; IntelliJ style keybindings
(global-set-key (kbd "s-E") 'consult-projectile-recentf)
(global-set-key (kbd "s-e") 'consult-projectile-find-file)

(define-key smartparens-mode-map (kbd "C-)") 'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-(") 'sp-forward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-M-)") 'sp-backward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-M-(") 'sp-backward-barf-sexp)

(define-key smartparens-mode-map (kbd "C-M-k") 'sp-kill-sexp)
(define-key smartparens-mode-map (kbd "M-<delete>") 'sp-unwrap-sexp)
(define-key smartparens-mode-map (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)
(define-key smartparens-mode-map (kbd "M-D") 'sp-splice-sexp)

(global-set-key (kbd "C-x g") 'magit-status)

(global-set-key (kbd "C-M-s") 'conjure-smartparens-hydra/body)
(defhydra conjure-smartparens-hydra ()
  "Smartparens"

  ("d" sp-down-sexp "Down")
  ("e" sp-up-sexp "Up")
  ("u" sp-backward-up-sexp "Up")
  ("a" sp-backward-down-sexp "Down")
  ("f" sp-forward-sexp "Forward")
  ("b" sp-backward-sexp "Backward")
  ("k" sp-kill-sexp "Kill" :color blue)
  ("q" nil "Quit" :color blue))

(provide 'conjure-global-keybindings)
;;; conjure-global-keybindings.el ends here
