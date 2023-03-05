;;; conjure-editor.el --- Conjure default editor configuration
;;; Commentary:
;;; Code:

(require 'diminish)
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

(when conjure-format-on-save (apheleia-global-mode 1))
(diminish 'apheleia-mode)

(winner-mode 1)

(require 'yasnippet)
(yas-global-mode 1)
(diminish 'yas-minor-mode)

;; dired - reuse current buffer by pressing 'a'
(put 'dired-find-alternate-file 'disabled nil)

;; always delete and copy recursively
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

;; if there is a dired buffer displayed in the next window, use its
;; current subdir, instead of the current subdir of this dired buffer
(setq dired-dwim-target t)

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

(require 'consult)
(add-hook 'completion-list-mode-hook #'consult-preview-at-point-mode)
(setq register-preview-delay 0.2
      register-preview-function #'consult-register-format)
(advice-add #'register-preview :override #'consult-register-window)

(setq xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)

(require 'embark-consult)
(add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode)

(add-hook 'dired-mode-hook #'diff-hl-dired-mode)
(add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)

(require 'ag)
(setq ag-highlight-search t
      ag-ignore-list (append ag-ignore-list
                             '(".git" ".cpcache" ".build" "log" "node_modules")))

(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

;; Compilation from Emacs
(defun conjure-colorize-compilation-buffer ()
  "Colorize a compilation mode buffer."
  (interactive)
  ;; we don't want to mess with child modes such as grep-mode, ack, ag, etc
  (when (eq major-mode 'compilation-mode)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max)))))

(require 'ansi-color)
(add-hook 'compilation-filter-hook #'conjure-colorize-compilation-buffer)

;; use settings from .editorconfig file when present
(require 'editorconfig)
(editorconfig-mode 1)
(diminish 'editorconfig-mode)

(provide 'conjure-editor)
;;; conjure-editor.el ends here
