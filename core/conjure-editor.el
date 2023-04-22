;;; conjure-editor.el --- Conjure default editor configuration
;;; Commentary:
;;; Code:

;; dont' use tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)

;; newlines at the end of files
(setq require-final-newline t)

;; store all backups and autosave files in tmp
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; revert buffers when the underlying files are changed
(global-auto-revert-mode t)

;; smart tab behavior
(setq tab-always-indent 'complete)

;; smart pairing
(require 'smartparens-config)
(show-smartparens-global-mode t)

;; now tidy up the modeline
(require 'diminish)

;; give better names for buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      uniquify-separator "/"
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

;; saveplace remembers where you were
(setq save-place-file (expand-file-name "saveplace" conjure-savefile-dir))
(save-place-mode 1)

;; savehist keeps track of history
(require 'savehist)
(setq save-hist-additional-variables '(search-ring regexp-search-ring)
      savehist-autosave-interval 60
      savehist-file (expand-file-name "savehist" conjure-savefile-dir))
(savehist-mode t)

;; save recent files
(require 'recentf)
(setq recentf-save-file (expand-file-name "recentf" conjure-savefile-dir)
      recentf-max-saved-items 500
      recentf-max-menu-items 15
      ;; disable on startup for better performance, and don't try to cleanup remotes
      recentf-auto-cleanup 'never)

(defun conjure-recentf-exclude-p (file)
  "A predicate to decide whether to exclude FILE from recentf."
  (let ((file-dir (file-truename (file-name-directory file))))
    (cl-some (lambda (dir)
	       (string-prefix-p dir file-dir))
	     (mapcar 'file-truname (list conjure-savefile-dir package-user-dir)))))

(add-to-list 'recentf-exclude "\\roam.*\\'")
(add-to-list 'recentf-exclude 'conjure-recentf-exclude-p)
(recentf-mode t)

;; highlight the current line
(global-hl-line-mode t)

(require 'volatile-highlights)
(volatile-highlights-mode t)
(diminish 'volatile-highlights-mode)

;; on-the-fly spell-checking
(require 'flyspell)
(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))

(defun conjure-enable-flyspell ()
  "Enable command `flyspell-mode' if `conjure-flyspell' is not nil."
  (when (and conjure-flyspell (executable-find ispell-program-name))
    (flyspell-mode t)))

(defun conjure-cleanup-maybe ()
  "Invoke `whitespace-cleanup' if `conjure-clean-whitespace-on-save' is not nil."
  (when conjure-cleanup-whitespace-on-save
    (whitespace-cleanup)))

(defun conjure-enable-whitespace ()
  "Enable `whitespace-mode' if `conjure-whitespace' is not nil."
  (when conjure-whitespace
    ;; keep the whitespace decent
    (add-hook 'before-save-hook 'conjure-cleanup-maybe nil t)
    (whitespace-mode t)))

(add-hook 'text-mode-hook 'conjure-enable-flyspell)
(add-hook 'text-mode-hook 'conjure-enable-whitespace)

;; enable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; enabled change region case commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; enable erase-buffer command
(put 'erase-buffer 'disabled nil)

(require 'expand-region)

;; bookmarks
(require 'bookmark)
(setq bookmark-default-file (expand-file-name "bookmarks" conjure-savefile-dir)
      bookmark-save-flag 1)

;; Projectile <-- this is an important one
(require 'projectile)
(setq projectile-cache-file (expand-file-name "projectile.cache" conjure-savefile-dir)
      projectile-ignored-projects '("~/"))
(projectile-mode t)

;; enhance isearch & query-replace
(require 'anzu)
(global-anzu-mode)
(diminish 'anzu-mode)

;; show key completions as they're typed
(require 'which-key)
(setq which-key-add-column-padding 4
      which-key-max-description-length 36)
(which-key-mode)
(diminish 'which-key-mode)

;; dired - reuse current buffer by pressing 'a'
(put 'dired-find-alternate-file 'disabled nil)

;; always delete and copy recursively
(setq dired-recursive-deletes 'always
      dired-recursive-copies 'always)

;; if there is a dired buffer displayed in the next window, use its
;; current subdir, instead of the current subdir of this dired buffer
(setq dired-dwim-target t)

;; enable extra dired functionality
(require 'dired-x)

;; ediff - don't open a new frame
(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; clean up obsolete buffers
(require 'midnight)

;; enhance kill-ring navigation
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)


(defadvice exchange-point-and-mark (before deactivate-mark activate compile)
  "When called with no active region, do not activate mark."
  (interactive
   (list (not (region-active-p)))))

(require 'tabify)
(defmacro with-region-or-buffer (func)
  "When called with no active region, call FUNC on current buffer."
  `(defadvice ,func (before with-region-or-buffer activate compile)
     (interactive
      (if mark-active
          (list (region-beginning) (region-end))
        (list (point-min) (point-max))))))

(with-region-or-buffer indent-region)
(with-region-or-buffer untabify)

;; automatically indenting yanked text if in programming-modes
(defun yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) prelude-yank-indent-threshold)
      (indent-region beg end nil)))

(defmacro advise-commands (advice-name commands class &rest body)
  "Apply advice named ADVICE-NAME to multiple COMMANDS.

The body of the advice is in BODY."
  `(progn
     ,@(mapcar (lambda (command)
                 `(defadvice ,command (,class ,(intern (concat (symbol-name command) "-" advice-name)) activate)
                    ,@body))
               commands)))

(advise-commands "indent" (yank yank-pop) after
  "If current mode is one of `conjure-yank-indent-modes',
indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (not (member major-mode conjure-indent-sensitive-modes))
           (or (derived-mode-p 'prog-mode)
               (member major-mode conjure-yank-indent-modes)))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function (region-beginning) (region-end)))))

;; abbrev config
(add-hook 'text-mode-hook 'abbrev-mode)
(diminish 'abbrev-mode)

;; make shell scripts exectuable automatically
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; whitespace-mode config
(require 'whitespace)
(setq whitespace-line-column 80
      whitespace-style '(face tabs empty trailing lines-tail))

;; better regex syntax
(require 're-builder)
(setq reb-re-syntax 'string)

(require 'eshell)
(setq eshell-directory-name (expand-file-name "eshell" conjure-savefile-dir))

(setq semanticdb-default-save-directory (expand-file-name "semanticdb" conjure-savefile-dir))

(require 'compile)
(setq compilation-ask-about-save nil
      compilation-always-kill t
      compilation-scroll-output 'first-error)

;; Colorize output of Compilation Mode, see
;; http://stackoverflow.com/a/3072831/355252
(require 'xterm-color)
(setq compilation-environment '("TERM=xterm-256color"))
(defun conjure-compilation-filter (f proc string)
  (funcall f proc (xterm-color-filter string)))
(advice-add 'compilation-filter :around #'conjure-compilation-filter)

;; better undo/redo
(require 'undo-tree)
(setq undo-tree-history-directory-alist `((".*" . ,temporary-file-directory))
      undo-tree-auto-save-history t)
(global-undo-tree-mode)
(diminish 'undo-tree-mode)

;; manage window configurations via winner-mode
(winner-mode t)

;; diff-hl
(global-diff-hl-mode t)
(add-hook 'dired-mode-hook #'diff-hl-dired-mode)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

;; operate-on-number
(require 'operate-on-number)
(require 'smartrep)

(smartrep-define-key global-map "C-c ."
  '(("+" . apply-operation-to-number-at-point)
    ("-" . apply-operation-to-number-at-point)
    ("*" . apply-operation-to-number-at-point)
    ("/" . apply-operation-to-number-at-point)
    ("\\" . apply-operation-to-number-at-point)
    ("^" . apply-operation-to-number-at-point)
    ("<" . apply-operation-to-number-at-point)
    (">" . apply-operation-to-number-at-point)
    ("#" . apply-operation-to-number-at-point)
    ("%" . apply-operation-to-number-at-point)
    ("'" . operate-on-number-at-point)))

;; pulse line when jumping locations
(pulsar-global-mode +1)

;; (add-to-list 'pulsar-pulse-functions 'flymake-goto-next-error)
;; (add-to-list 'pulsar-pulse-functions 'flymake-goto-prev-error)
(add-hook 'next-error-hook #'pulsar-pulse-line)

(require 'editorconfig)
(editorconfig-mode t)
(diminish 'editorconfig-mode)

(provide 'conjure-editor)

;;; conjure-editor.el ends here
