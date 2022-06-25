;;; init-editor.el -- editor configuration
;;; Commentary:
;;; Code:
(setq-default indent-tabs-mode nil) ;; no tabs
(setq-default tab-width 8)          ;; fake it with tabs

(setq require-final-newline t)

(customize-set-variable 'global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)

(setq auto-mode-alist
      (append '(("\\.native\\'" . nxml-mode)
                ("\\.echo10\\'" . nxml-mode)
                ("\\.dif\\'" . nxml-mode)
                ("\\.dif10\\'" . nxml-mode)
                ;; ISO may need pre-processing to not open in so-long-mode
                ;; ("\\.iso\\'" . nxml-mode)
                ;; ("\\.iso19115\\'" . nxml-mode)
                ("\\.umm_json\\'" . js-mode))
              auto-mode-alist))

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(require 'smartparens-config)
(setq sp-base-key-bindings 'paredit)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)
(sp-use-paredit-bindings)

(show-smartparens-global-mode +1)

(require 'diminish)

(require 'uniquify)
(setq uniqify-buffer-name-style 'forward)
(setq uniqify-separator "/")
(setq uniqify-after-kill-buffer-p t) ; rename after killing unique
(setq uniquify-ignore-buffers-re "^\\*") ; ignore special buffers

(setq save-place-file (expand-file-name "saveplace" conjure-savefile-dir))
(save-place-mode 1)

(require 'recentf)
(setq recentf-save-file (expand-file-name "recentf" conjure-savefile-dir))

(require 'bookmark)
(setq bookmark-default-file (expand-file-name "bookmarks" conjure-savefile-dir)
      bookmark-save-flag 1)

(require 'projectile)
(setq projectile-cache-file (expand-file-name "projectile.cache" conjure-savefile-dir))
(projectile-mode t)

;; Better support for files with long lines
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)
(global-so-long-mode 1)

(require 'avy)
(setq avy-background t)
(setq avy-style 'at-full)

(require 'anzu)
(diminish 'anzu-mode)
(global-anzu-mode)

(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

(put 'dired-find-alternate-file 'disabled nil)

(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies'always)

(setq dired-dwim-target t)

(require 'dired-x)

(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(require 'midnight)

(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)
(global-set-key (kbd "s-y") 'browse-kill-ring)

(customize-set-variable 'kill-do-not-save-duplicates t)

(require 'flyspell)
(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))

(require 'eshell)
(setq eshell-directory-name (expand-file-name "eshell" conjure-savefile-dir))

(defun conjure-cleanup-maybe ()
  "Invoke `whitespace-cleanup' if `conjure-clean-whitespace-on-save' is not nil."
  (when conjure-clean-whitespace-on-save
    (whitespace-cleanup)))

(defun conjure-enable-whitespace ()
  "Enable `whitespace-mode' if `conjure-whitespace' is not nil."
  (when conjure-whitespace
    ;; keep the whitespace decent all the time (in this buffer)
    (add-hook 'before-save-hook 'conjure-cleanup-maybe nil t)
    (whitespace-mode +1)))

(defun conjure-enable-flyspell ()
  "Enable command 'flyspell-mode' if 'conjure-flyspell' is not nil."
  (when (and conjure-flyspell (executable-find ispell-program-name))
    (flyspell-mode +1)))

(add-hook 'text-mode-hook 'conjure-enable-flyspell)

(global-diff-hl-mode +1)
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

(setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)

(conjure-mode 1)

(require 'undo-tree)
;; autosave the undo-tree history
(setq undo-tree-history-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq undo-tree-auto-save-history t)
(global-undo-tree-mode)
(diminish 'undo-tree-mode)

(winner-mode +1)

(global-set-key [remap kill-ring-save] 'easy-kill)
(global-set-key [remap mark-sexp] 'easy-mark)

(provide 'init-editor)
;;; init-editor.el
