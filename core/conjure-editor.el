;;; conjure-editor.el --- Conjure default editor configuration
;;; Commentary:
;;; Code:

(setq-default tab-width 8)

;; newlines at the end of files
(setq require-final-newline t)

;; store all backups and auto-save files in tmp
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

;; now tidy up the mode-line
(require 'diminish)

;; give better names for buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      uniquify-separator "/"
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

;; saveplace remembers where you were
(require 'saveplace)
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
             (mapcar 'file-truename (list conjure-savefile-dir package-user-dir)))))

(add-to-list 'recentf-exclude "\\roam.*\\'")
(add-to-list 'recentf-exclude 'conjure-recentf-exclude-p)
(recentf-mode t)

;; automatically save buffers on switching
(require 'super-save)
(add-to-list 'super-save-triggers 'ace-window)
(super-save-mode +1)
(diminish 'super-save-mode)

(defadvice set-buffer-major-mode (after set-major-mode activate compile)
  "Set buffer major mode according to `auto-mode-alist'."
  (let* ((name (buffer-name buffer))
         (mode (assoc-default name auto-mode-alist 'string-match)))
    (when (and mode (consp mode))
      (setq mode (car mode)))
    (with-current-buffer buffer (if mode (funcall mode)))))

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
    (whitespace-mode +1)
    (diminish 'whitespace-mode)))

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

;; magit settings <-- this is important
(require 'magit)
(setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
      git-commit-summary-max-length 50)

;; Projectile <-- this is an important one
(require 'projectile)
(setq projectile-cache-file (expand-file-name "projectile.cache" conjure-savefile-dir)
      projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" conjure-savefile-dir)
      projectile-ignored-projects '("~/")
      projectile-create-missing-test-files t)
(projectile-mode t)
(diminish 'projectile-mode)

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
      dired-recursive-copies 'always
      ;; -a include dot files
      ;; -l long format, required for dired
      ;; -G colorize output
      ;; -F display entry type with ending e.g / *, @, % |
      dired-listing-switches (if osx-p
                                 "-alhFG"
                               "-alhFG --group-directories-first"))

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
  "Do indentation, as long as the region isn't too large between BEG and END."
  (if (<= (- end beg) conjure-yank-indent-threshold)
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
(setq whitespace-line-column conjure-column-fill
      whitespace-style '(face tabs empty trailing lines-tail))

(add-hook 'text-mode (lambda ()
                       (setq-local whitespace-line-column nil)))

;; better regex syntax
(require 're-builder)
(setq reb-re-syntax 'string)

(require 'eshell)
(setq eshell-directory-name (expand-file-name "eshell" conjure-savefile-dir))

(setq semanticdb-default-save-directory
      (expand-file-name "semanticdb" conjure-savefile-dir))

(require 'compile)
(setq compilation-ask-about-save nil
      compilation-always-kill t
      compilation-scroll-output 'first-error)

;; Colorize output of Compilation Mode, see
;; http://stackoverflow.com/a/3072831/355252
(require 'xterm-color)
(setq compilation-environment '("TERM=xterm-256color"))
;; Be aware of using rg.el or ag.el
;; See https://github.com/dajva/rg.el/issues/154
(add-hook 'compilation-filter-hook 'conjure-colorize-buffer-ansi-colors)

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
(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
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
(require 'pulsar)
(setq pulsar-face 'pulsar-green)
(pulsar-global-mode +1)
(when (fboundp 'ace-window)
  ;; pulsar doesn't detect the override because of ordering so we have to set it ourselves
  (add-to-list 'pulsar-pulse-functions 'ace-window))
(add-hook 'next-error-hook 'pulsar-pulse-line)

;; editorconfig
(require 'editorconfig)
(editorconfig-mode t)
(diminish 'editorconfig-mode)

;; hide embark completion buffers
(add-to-list 'display-buffer-alist
             '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
               nil
               (window-parameters (mode-line-format . none))))

;; integrate embark with which-key
(defun embark-which-key-indicator ()
  "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
  (lambda (&optional keymap targets prefix)
    (if (null keymap)
        (which-key--hide-popup-ignore-command)
      (which-key--show-keymap
       (if (eq (plist-get (car targets) :type) 'embark-become)
           "Become"
         (format "Act on %s '%s'%s"
                 (plist-get (car targets) :type)
                 (embark--truncate-target (plist-get (car targets) :target))
                 (if (cdr targets) "…" "")))
       (if prefix
           (pcase (lookup-key keymap prefix 'accept-default)
             ((and (pred keymapp) km) km)
             (_ (key-binding prefix 'accept-default)))
         keymap)
       nil nil t (lambda (binding)
                   (not (string-suffix-p "-argument" (cdr binding))))))))

(setq embark-indicators
      '(embark-which-key-indicator
        embark-highlight-indicator
        embark-isearch-highlight-indicator))

(defun embark-hide-which-key-indicator (fn &rest args)
  "Hide the which-key indicator immediately when using the `completing-read' FN using ARGS."
  (which-key--hide-popup-ignore-command)
  (let ((embark-indicators
         (remq #'embark-which-key-indicator embark-indicators)))
    (apply fn args)))

(advice-add #'embark-completing-read-prompter
            :around #'embark-hide-which-key-indicator)

(provide 'conjure-editor)

;;; conjure-editor.el ends here
