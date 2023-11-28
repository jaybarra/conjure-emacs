;;; conjure-editor.el --- Conjure default editor configuration
;;; Commentary:
;;; Code:

(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)

;; non-destructive y-or-n-p instead of yes-or-no-p
(defun yes-or-no-p-advice (_orig &rest args)
  "Advice to use `y-or-n-p' instead of `yes-or-no-p' and pass ARGS."
  (apply 'y-or-n-p args))
(advice-add 'yes-or-no-p :around 'yes-or-no-p-advice)

;; newlines at the end of files
(setq require-final-newline t)

;; allow overwrite of active region by typing
(delete-selection-mode t)

;; store all backups and auto-save files in tmp
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; revert buffers when the underlying files are changed
(global-auto-revert-mode t)

;; smart tab behavior
(setq tab-always-indent 'complete)

(use-package ace-window)

(use-package delight)

;; smart pairing
(use-package smartparens
  :hook (prog-mode text-mode markdown-mode)
  :bind (("C-M-f" . sp-forward-sexp)
         ("C-M-b" . sp-backward-sexp)

         ("C-M-f" . sp-forward-sexp)
         ("C-M-b" . sp-backward-sexp)

         ("C-M-d" . sp-down-sexp)
         ("C-M-a" . sp-backward-down-sexp)
         ("C-S-d" . sp-beginning-of-sexp)
         ("C-S-a" . sp-end-of-sexp)

         ("C-M-e" . sp-up-sexp)
         ("C-M-u" . sp-backward-up-sexp)
         ("C-M-t" . sp-transpose-sexp)

         ("C-M-k" . sp-kill-sexp)
         ("C-M-w" . sp-copy-sexp)

         ("M-<delete>" . sp-unwrap-sexp)
         ("M-<backspace>" . sp-backward-unwrap-sexp)

         ("C-)" . sp-forward-slurp-sexp)
         ("C-(" . sp-forward-barf-sexp)
         ("C-M-(" . sp-backward-slurp-sexp)
         ("C-M-)" . sp-backward-barf-sexp)

         ("M-D" . sp-splice-sexp)
         ("C-M-<delete>" . sp-splice-sexp-killing-forward)
         ("C-M-<backspace>" . sp-splice-sexp-killing-backward)
         ("C-S-<backspace>" . sp-splice-sexp-killing-around)

         ("C-]" . sp-select-next-thing-exchange)
         ("C-<left_bracket>" . sp-select-previous-thing)
         ("C-M-]" . sp-select-next-thing)

         ("M-F" . sp-forward-symbol)
         ("M-B" . sp-backward-symbol)

         ("C-\"" . sp-change-inner)
         ("M-i" . sp-change-enclosing))
  :config
  (require 'smartparens-config)

  (sp-with-modes '(js2-mode typescript-mode java-mode tsx-ts-mode typescript-ts-mode)
    (sp-local-pair "/**" "*/" :post-handlers '(("| " "SPC")
                                               ("* ||\n[i]" "RET"))))

  (sp-with-modes '(tsx-ts-mode typescript-ts-mode typescript-mode)
    (sp-local-pair "<" ">" :actions '(navigate)))

  (add-hook 'eval-expression-minibuffer-setup-hook 'turn-on-smartparens-strict-mode)
  
  (show-smartparens-global-mode))

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
(use-package savehist
  :straight (:type built-in)

  :custom
  (save-hist-additional-variables '(search-ring regexp-search-ring))
  (savehist-autosave-interval 60)
  (savehist-file (expand-file-name "savehist" conjure-savefile-dir))

  :config
  (savehist-mode +1))

;; save recent files
(use-package recentf
  :straight nil
  :config
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
  (recentf-mode t))

;; automatically save buffers on switching
(use-package ace-window)
(use-package super-save
  :delight
  :config
  (add-to-list 'super-save-triggers 'ace-window)
  (super-save-mode +1))

(defadvice set-buffer-major-mode (after set-major-mode activate compile)
  "Set buffer major mode according to `auto-mode-alist'."
  (let* ((name (buffer-name buffer))
         (mode (assoc-default name auto-mode-alist 'string-match)))
    (when (and mode (consp mode))
      (setq mode (car mode)))
    (with-current-buffer buffer (if mode (funcall mode)))))

(use-package volatile-highlights
  :delight
  :config
  (volatile-highlights-mode t))

;; ;; (use-package flyspell
;; ;;   :custom
;; ;;   (ispell-program-name "aspell")
;; ;;   (ispell-extra-args '("--sug-mode=ultra"))
;; ;;   :config
;; ;;   (defun conjure-enable-flyspell ()
;; ;; 	"Enable command `flyspell-mode' if `conjure-flyspell' is not nil."
;; ;; 	(when (and conjure-flyspell (executable-find ispell-program-name))
;; ;;       (flyspell-mode t))))

(use-package expand-region
  :config
  (put 'narrow-to-region 'disabled nil)
  (put 'narrow-to-page 'disabled nil)
  (put 'narrow-to-defun 'disabled nil))

;; enabled change region case commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; enable erase-buffer command
(put 'erase-buffer 'disabled nil)

;; bookmarks
(use-package bookmark
  :config
  (setq bookmark-default-file (expand-file-name "bookmarks" conjure-savefile-dir)
	bookmark-save-flag 1))

;; magit
(use-package magit
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
	git-commit-summary-max-length 50))

(use-package magit-todos
  :after magit
  :config
  (magit-todos-mode 1))

(use-package git-timemachine)
(use-package git-modes
  :config
  (add-to-list 'auto-mode-alist (cons "/.dockerignore\\'" 'gitignore-mode)))

;; Projectile
(use-package projectile
  :delight
  :config
  (setq projectile-cache-file (expand-file-name "projectile.cache" conjure-savefile-dir)
	projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" conjure-savefile-dir)
	projectile-ignored-projects '("~/")
	projectile-create-missing-test-files t)
  (projectile-mode t))

;; enhance isearch & query-replace
(use-package anzu
  :delight
  :config
  (global-anzu-mode))

;; show key completions as they're typed
(use-package which-key
  :delight
  :config
  (setq which-key-add-column-padding 4
	which-key-max-description-length 36))

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
(use-package midnight
  :config
  (midnight-delay-set 'midnight-delay "4:30am"))

;; enhance kill-ring navigation
(use-package browse-kill-ring
  :config
  (browse-kill-ring-default-keybindings))

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

;; make shell scripts exectuable automatically
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; whitespace-mode config
(use-package whitespace
  :straight nil
  :config
  (setq whitespace-style '(face tabs empty trailing lines-tail)))

;; better regex syntax
(use-package re-builder
  :config
  (setq reb-re-syntax 'string))

(require 'eshell)
(setq eshell-directory-name (expand-file-name "eshell" conjure-savefile-dir))

(setq semanticdb-default-save-directory
      (expand-file-name "semanticdb" conjure-savefile-dir))

(require 'compile)
(setq compilation-ask-about-save nil
      compilation-always-kill t
      compilation-scroll-output 'first-error)

;; ;; Colorize output of Compilation Mode, see
;; ;; http://stackoverflow.com/a/3072831/355252
;; (use-package xterm-color)
;; (require 'xterm-color)
;; (setq compilation-environment '("TERM=xterm-256color"))
;; (defun my/advice-compilation-filter (f proc string)
;;   (funcall f proc (xterm-color-filter string)))
;; (advice-add 'compilation-filter :around #'my/advice-compilation-filter)

;; better undo/redo
(use-package undo-tree
  :delight
  :config
  (setq undo-tree-history-directory-alist `((".*" . ,temporary-file-directory))
	undo-tree-auto-save-history t)
  (global-undo-tree-mode))

;; manage window configurations via winner-mode
(use-package winner)

;; ;; diff-hl
(use-package diff-hl
  :after magit
  :config
  (global-diff-hl-mode t)

  (add-hook 'dired-mode-hook #'diff-hl-dired-mode)
  (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

;; operate-on-number
(use-package operate-on-number)

;; pulse line when jumping locations
(use-package pulsar
  :config
  (setq pulsar-face 'pulsar-green)
  (pulsar-global-mode +1)

  (when (fboundp 'ace-window)
    ;; pulsar doesn't detect the override because of ordering so we have to set it ourselves
    (add-to-list 'pulsar-pulse-functions 'ace-window))

  (add-hook 'next-error-hook 'pulsar-pulse-line))

(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  ;; :init

  ;; ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; ;; strategy, if you want to see the documentation from multiple providers.
  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; ;;(setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package easy-kill)
(use-package rg)

(provide 'conjure-editor)

;;; conjure-editor.el ends here
