;;; conjure-editor.el --- Conjure default editor configuration
;;; Commentary:
;;; Code:

(setq require-final-newline t)

(define-coding-system-alias 'UTF-8 'utf-8)

(global-auto-revert-mode +1)

(show-smartparens-global-mode +1)
(smartparens-global-mode)
(diminish 'smartparens-mode)

(volatile-highlights-mode t)
(diminish 'volatile-highlights-mode)

(global-anzu-mode)
(diminish 'anzu-mode)

(global-diff-hl-mode)
(global-hl-todo-mode)

(which-key-mode)
(diminish 'which-key-mode)

(apheleia-global-mode 1)
(winner-mode 1)

(require 'yasnippet)
(yas-global-mode 1)

(require 'dired-x)
(setq dired-listing-switches "-lahF"
      dired-dwim-target t
      dired-deletion-confirmer 'y-or-n-p
      dired-omit-files "\\`[.]?#\\|\\`[.][.]?\\'\\|*\\.o\\`\\|*\\.log\\`")

(require 'recentf)
(add-to-list 'recentf-exclude "\\roam.*\\'")
(recentf-mode)

(add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

(require 'embark-consult)
(add-hook 'embark-collect-mode-hook 'consult-preview-at-point-mode)

(add-hook 'dired-mode-hook 'diff-hl-dired-mode)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

(require 'ag)
(setq ag-highlight-search t
      ag-ignore-list (append ag-ignore-list
                             '(".git" ".build" "log" "node_modules")))

(autoload 'wgrep-ag-setup "wgrep-ag")
(add-hook 'ag-mode-hook 'wgrep-ag-setup)

(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

(setq lsp-headerline-breadcrumb-icons-enable nil
      lsp-modeline-code-actions-enable t
      lsp-lens-enable nil
      lsp-completion-provider :none)

(provide 'conjure-editor)
;;; conjure-editor.el ends here
