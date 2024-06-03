;;; conjure-editor.el --- Conjure default editor configuration
;;; Commentary:
;;; Code:


(setq-default indent-tabs-mode nil
              tab-width 4)
(setq-default tab-always-indent nil)
(setq tabify-regexp "^\t* [ \t]+")

(setq-default fill-column 80)
(setq-default word-wrap t)
(setq-default truncate-lines t)

(use-package async :ensure t)

(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(setq enable-recursive-minibuffers t)

(setopt use-short-answers t)

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

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("Dired" (mode . dired-mode))
               ("Emacs" (or (name . "^\\*scratch\\*$")
                            (name . "^\\*Messages\\*$")))
               ("Source" (derived-mode . prog-mode))
               ("Markdown" (mode . markdown-mode))
               ("Org" (mode . org-mode))
               ("Magit" (name . "^magit"))
               ("Help" (or (name . "^\\*Help\\*")
                           (name . "^\\*Apropos\\*")
                           (name . "^\\*info\\*$")))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-auto-mode 1)
            (ibuffer-switch-to-saved-filter-groups "default")))

(use-package smartparens
  :ensure t
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
  (setq sp-show-pair-delay 0.2)
  (setq sp-show-pair-from-inside t) ;; Show matching pair if inside pair
  (setq sp-cancel-autoskip-on-backward-movement nil) ;; Do not cancel autoskip on backward movement
  (setq sp-highlight-pair-overlay nil) ;; Do not highlight pair overlay
  (setq sp-highlight-wrap-overlay nil) ;; Do not highlight wrap overlay
  (setq sp-highlight-wrap-tag-overlay nil) ;; Do not highlight tag wrap

  (sp-with-modes '(js2-mode typescript-mode java-mode tsx-ts-mode typescript-ts-mode)
    (sp-local-pair "/**" "*/" :post-handlers '(("| " "SPC")
                                               ("* ||\n[i]" "RET"))))

  (sp-with-modes '(tsx-ts-mode typescript-ts-mode typescript-mode)
    (sp-local-pair "<" ">" :actions '(navigate)))

  (add-hook 'eval-expression-minibuffer-setup-hook 'turn-on-smartparens-strict-mode)

  (show-smartparens-global-mode))

(use-package lin
  :ensure t
  :config
  (lin-global-mode))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :config
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)

  (when (string= system-type "darwin")
    (setq dired-use-ls-dired t
          insert-directory-program "/opt/homebrew/bin/gls"
          dired-listing-switches "-aBhl --group-directories-first"))

  ;; easier copying/moving between dired panels
  (setq dired-dwim-target t)

  ;; auto-refresh dired on file change
  (add-hook 'dired-mode-hook 'auto-revert-mode))

;; Project switching
(require 'project)
(setq project-list-file (expand-file-name "projects" conjure-savefile-dir))

;; Bookmarks
(use-package bookmark
  :ensure nil
  :config
  (setq bookmark-default-file (expand-file-name "bookmarks" conjure-savefile-dir)
        bookmark-save-flag 1))

;; Magit
(use-package magit
  :ensure t
  :commands (magit-status magit-get-current-branch)
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
        git-commit-summary-max-length 50))

(use-package magit-todos
  :ensure t
  :after magit
  :config
  ;; limit scanning
  (setq magit-todos-depth 4)

  (magit-todos-mode 1))

(use-package git-timemachine :ensure t)
(use-package git-modes
  :ensure t
  :config
  (add-to-list 'auto-mode-alist (cons "/.dockerignore\\'" 'gitignore-mode)))

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode) ;; Enable diff-hl globally
  (diff-hl-flydiff-mode nil) ;; Optional: for real-time diff updates

  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package avy
  :ensure t
  :config
  (setq avy-background t)
  (setq avy-style 'at-full))

(use-package easy-kill :ensure t)

(use-package volatile-highlights
  :ensure t
  :delight
  :config
  (volatile-highlights-mode t))

(use-package anzu
  :ensure t
  :delight
  :config
  (global-anzu-mode))

(use-package which-key
  :ensure t
  :delight
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5)
  (setq which-key-max-description-length 40))

(require 'compile)
(setq compilation-ask-about-save nil
      compilation-always-kill t
      compilation-scroll-output 'first-error)

;; better undo/redo
(use-package undo-tree
  :ensure t
  :delight
  :config
  (setq undo-tree-history-directory-alist `((".*" . ,temporary-file-directory))
        undo-tree-auto-save-history t)
  (global-undo-tree-mode))

(use-package markdown-mode
  :ensure t
  :config
  (autoload 'markdown-mode "markdown-mode"
    "Major mode for editing Markdown files" t)
  (add-to-list 'auto-mode-alist
               '("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode))

  (autoload 'gfm-mode "markdown-mode"
    "Major mode for editing GitHub Flavored Markdown files" t)
  (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode)))

(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  (setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
  (setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers
  )

;; saveplace remembers your location in a file when saving files
(setq save-place-file (expand-file-name "saveplace" conjure-savefile-dir))
;; activate it for all buffers
(save-place-mode 1)

(use-package savehist
  :ensure nil
  :config
  (setq savehist-additional-variables
        ;; search entries
        '(search-ring regexp-search-ring)
        ;; save every minute
        savehist-autosave-interval 60
        ;; keep the home clean
        savehist-file (expand-file-name "savehist" conjure-savefile-dir))
  (savehist-mode +1))

(use-package recentf
  :ensure nil
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

(set-default 'imenu-auto-rescan t)

(add-hook 'text-mode-hook 'abbrev-mode)

;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; .zsh file is shell script too
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))

(setq reb-re-syntax 'string)

(use-package browse-kill-ring :ensure t)

;; Compilation from Emacs
(defun conjure-colorize-compilation-buffer ()
  "Colorize a compilation mode buffer."
  (interactive)
  ;; we don't want to mess with child modes such as grep-mode, ack, ag, etc
  (when (eq major-mode 'compilation-mode)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max)))))

(require 'compile)
(setq compilation-ask-about-save nil
      compilation-always-kill t
      compilation-scroll-output 'first-error)

;; Colorize output of Compilation Mode, see
;; http://stackoverflow.com/a/3072831/355252
(require 'ansi-color)
(add-hook 'compilation-filter-hook #'conjure-colorize-compilation-buffer)

(setq js-chain-indent t
      ;; These have become standard in the JS community
      js2-basic-offset 2
      ;; Don't mishighlight shebang lines
      js2-skip-preprocessor-directives t
      ;; let flycheck handle this
      js2-mode-show-parse-errors nil
      js2-mode-show-strict-warnings nil
      ;; Flycheck provides these features, so disable them: conflicting with
      ;; the eslint settings.
      js2-strict-missing-semi-warning nil
      ;; maximum fontification
      js2-highlight-level 3
      js2-idle-timer-delay 0.15)

(provide 'conjure-editor)
;;; conjure-editor.el ends here
