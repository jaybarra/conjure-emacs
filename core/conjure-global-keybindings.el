;;; conjure-global-keybindings.el --- Conjure Global Keybindings
;;; Commentary:
;;; Code:
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "<f5>") 'revert-buffer)

(global-set-key (kbd "M-z") 'zop-up-to-char)
(global-set-key (kbd "M-Z") 'zop-to-char)

(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

(global-set-key (kbd "s-y") 'browse-kill-ring)

(global-set-key [remap kill-ring-save] 'easy-kill)

(global-set-key [remap mark-sexp] 'easy-mark)

(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C--") 'er/contract-region)

;; Avy keybindings
(global-set-key (kbd "C-:") 'avy-goto-char)
(global-set-key (kbd "C-'") 'avy-goto-char-2)
(global-set-key (kbd "M-g f") 'avy-goto-line)
(global-set-key (kbd "M-g w") 'avy-goto-word-1)
(global-set-key (kbd "M-g e") 'avy-goto-word-0)

(global-set-key (kbd "s-w") 'ace-window)
(global-set-key [remap other-window] 'ace-window)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-x g") 'magit-status)

(provide 'conjure-global-keybindings)
;;; conjure-global-keybindings.el ends here
