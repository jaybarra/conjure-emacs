;;; conjure-editor.el --- Conjure default editor configuration
;;; Commentary:
;;; Code:

(require 'diminish)
(setq require-final-newline t)

(global-auto-revert-mode +1)

(require 'volatile-highlights)
(volatile-highlights-mode t)
(diminish 'volatile-highlights-mode)

(require 'anzu)
(global-anzu-mode)
(diminish 'anzu-mode)

(require 'which-key)
(which-key-mode)
(which-key-setup-minibuffer)
(diminish 'which-key-mode)

(require 'apheleia)
(diminish 'apheleia-mode)
(when conjure-format-on-save (apheleia-global-mode +1))
(with-eval-after-load 'apheleia
  (diminish 'apheleia-mode))

(winner-mode 1)

(require 'yasnippet)
(yas-global-mode 1)
(diminish 'yas-minor-mode)

;; dired - reuse current buffer by pressing 'a'
(put 'dired-find-alternate-file 'disabled nil)

(require 'dired-x)
;; always delete and copy recursively
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

;; if there is a dired buffer displayed in the next window, use its
;; current subdir, instead of the current subdir of this dired buffer
(setq dired-dwim-target t)

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

(require 'consult)

(require 'consult-xref)
(setq xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)

(require 'embark-consult)
(add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode)

(require 'diff-hl-dired)
(add-hook 'dired-mode-hook #'diff-hl-dired-mode)

(require 'ag)
(setq ag-highlight-search t
      ag-ignore-list (append ag-ignore-list
                             '(".git" ".cpcache" ".build" "log" "node_modules")))

(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

(with-eval-after-load 'subword
  (diminish 'subword-mode))

(provide 'conjure-editor)
;;; conjure-editor.el ends here
