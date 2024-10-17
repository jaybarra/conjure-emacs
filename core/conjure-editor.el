;;; conjure-editor.el --- Configurations for Editor Configuration in Conjure -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

(setq-default indent-tabs-mode nil
              tab-width 2
              tab-always-indent nil)

(setq tabify-regexp "^\t* [ \t]+")

(setq reb-re-syntax 'string)

(setq-default fill-column 80)
(setq-default word-wrap t)
(setq-default truncate-lines t)

(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(setq enable-recursive-minibuffers t)

;; prefer y-n vs yes-no responses
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

;; load core packages
(elpaca browse-kill-ring)

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

(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*"))

(use-package which-key
  :init (which-key-mode)
  :custom
  (which-key-lighter "") ;; setting this manually because `delight' is being overruled
  :config
  (setq which-key-add-column-padding 1
        which-key-allow-imprecise-window-fit t
        which-key-idle-secondary-delay 0.05
        which-key-max-description-length 35
        which-key-max-description-length 40
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-prefix-prefix "+"
        which-key-separator " → "
        which-key-show-remaining-keys t
        which-key-side-window-max-width 0.33
        which-key-side-window-slot -10
        which-key-sort-uppercase-first nil
        which-key-idle-delay 0.3))

(defun conjure--setup-prog-mode-defaults ()
  "Sensible defaults for `prog-mode'."
  (hl-line-mode +1)
  (subword-mode +1)
  (column-number-mode +1)
  (prettify-symbols-mode +1)
  (setq-local whitespace-style '(face tabs tab-mark trailing))
  (whitespace-mode +1)
  (setq-local display-line-numbers 'relative))

(use-package whitespace
  :ensure nil
  :delight)

(use-package subword
  :ensure nil
  :delight)

(add-hook 'prog-mode-hook #'conjure--setup-prog-mode-defaults)

(defun conjure--setup-text-mode-defaults ()
  "Sensible defaults for `text-mode'"
  (whitespace-mode +1)
  (column-number-mode +1)
  (setq-local whitespace-style '(face tabs tab-mark trailing))
  (setq-local display-line-numbers 'relative))

(add-hook 'text-mode-hook #'conjure--setup-text-mode-defaults)

(require 'compile)
(setq compilation-ask-about-save nil
      compilation-always-kill t
      compilation-scroll-output 'first-error)

(use-package recentf
  :ensure nil
  :config
  (recentf-mode +1)
  (setq recentf-save-file (expand-file-name "recentf" conjure-savefile-dir)
        recentf-max-saved-items 500
        recentf-max-menu-items 15))

(set-default 'imenu-auto-rescan t)

;; Savehist
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

;; Project switching
(require 'project)
(setq project-list-file (expand-file-name "projects" conjure-savefile-dir))

;; Bookmarks
(use-package bookmark
  :ensure nil
  :config
  (setq bookmark-default-file (expand-file-name "bookmarks" conjure-savefile-dir)
        bookmark-save-flag 1))

;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; .zsh file is shell script too
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))

(use-package eldoc
  :ensure nil
  :delight)

(use-package eglot
  :ensure nil
  :config
  (setq eglot-autoshutdown t
        ;; Allow server-initiated edits
        eglot-confirm-server-initiated-edits nil)

  (defun conjure--eglot-format-buffer ()
    "Format the current buffer if managed by Eglot."
    (when (and (boundp 'eglot--managed-mode) eglot--managed-mode)
      (eglot-format-buffer)))

  (add-hook 'before-save-hook #'conjure--eglot-format-buffer))

(use-package smartparens
  :hook (prog-mode text-mode markdown-mode)
  :bind (("M-[" . sp-unwrap-sexp)
         ("M-]" . sp-backward-unwrap-sexp))
  :config
  ;; load default config
  (require 'smartparens-config)
  (setq sp-show-pair-delay 0.2)

  ;; Handle single quote and backticks appropriately
  (sp-local-pair '(emacs-lisp-mode lisp-data-mode clojure-mode clojure-ts-mode lisp-mode) "'" nil :actions nil)
  (sp-local-pair 'emacs-lisp-mode "`" "'" :when '(sp-in-string-p))

  (sp-use-paredit-bindings)

  (add-hook 'minibuffer-setup-hook 'turn-on-smartparens-strict-mode)
  (add-hook 'prog-mode-hook #'turn-on-smartparens-strict-mode))

(use-package undo-tree
  :delight
  :config
  (setq undo-tree-history-directory-alist `((".*" . ,temporary-file-directory))
        undo-tree-auto-save-history t)
  (global-undo-tree-mode))

(use-package corfu
  :init
  (global-corfu-mode)
  :bind (:map corfu-map
              ("<escape>" . corfu-quit)
	            ("TAB" . nil)
              ([tab] . nil)
	            ("C-y" . corfu-insert)
              ("M-d" . corfu-info-documentation)
              ("M-l" . corfu-info-location))
  :custom
  (corfu-auto nil)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.2)
  (corfu-quit-no-match 'separator)
  (corfu-popupinfo-delay '(2.0 . 1.0))
  (tab-always-indent 'complete)
  (completion-cycle-threshold nil)
  (corfu-separator ?\s)
  (corfu-quit-no-match 'separator)
  (corfu-preview-current 'insert)
  (lsp-completion-provider :none)
  :config
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))

  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)
  (add-hook 'global-corfu-mode-hook #'corfu-popupinfo-mode))

(use-package cape
  :init
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default)
  (kind-icon-blend-background nil)
  (kind-icon-blend-frac 0.08)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package embark)
(use-package embark-consult)

(use-package vertico
  :demand t                             ; Otherwise won't get loaded immediately
  :bind (:map vertico-map
              ("<tab>" . vertico-insert)
              ("<escape>" . minibuffer-keyboard-quit)
              ("?" . minibuffer-completion-help)
              ("C-M-n" . vertico-next-group)
              ("C-M-p" . vertico-previous-group)
              ;; Multiform toggles
              ("<backspace>" . vertico-directory-delete-char)
              ("C-w" . vertico-directory-delete-word)
              ("C-<backspace>" . vertico-directory-delete-word)
              ("RET" . vertico-directory-enter)
              ("C-i" . vertico-quick-insert)
              ("C-o" . vertico-quick-exit)
              ("M-o" . kb/vertico-quick-embark)
              ("M-G" . vertico-multiform-grid)
              ("M-F" . vertico-multiform-flat)
              ("M-R" . vertico-multiform-reverse)
              ("M-U" . vertico-multiform-unobtrusive)
              ("C-l" . kb/vertico-multiform-flat-toggle)
              )
  :hook ((rfn-eshadow-update-overlay . vertico-directory-tidy) ; Clean up file path when typing
         (minibuffer-setup . vertico-repeat-save) ; Make sure vertico state is saved
         )
  :custom
  (vertico-count 13)
  (vertico-resize t)
  (vertico-cycle nil)
  ;; Extensions
  (vertico-grid-separator "       ")
  (vertico-grid-lookahead 50)
  (vertico-buffer-display-action '(display-buffer-reuse-window))
  (vertico-multiform-categories
   '((file reverse)
     (consult-grep buffer)
     (consult-location)
     (imenu buffer)
     (library reverse indexed)
     (org-roam-node reverse indexed)
     (t reverse)
     ))
  (vertico-multiform-commands
   '(("flyspell-correct-*" grid reverse)
     (org-refile grid reverse indexed)
     (consult-yank-pop indexed)
     (consult-flycheck)
     (consult-lsp-diagnostics)
     ))
  :init
  (defun kb/vertico-multiform-flat-toggle ()
    "Toggle between flat and reverse."
    (interactive)
    (vertico-multiform--display-toggle 'vertico-flat-mode)
    (if vertico-flat-mode
        (vertico-multiform--temporary-mode 'vertico-reverse-mode -1)
      (vertico-multiform--temporary-mode 'vertico-reverse-mode 1)))
  (defun kb/vertico-quick-embark (&optional arg)
    "Embark on candidate using quick keys."
    (interactive)
    (when (vertico-quick-jump)
      (embark-act arg)))

  ;; Workaround for problem with `tramp' hostname completions. This overrides
  ;; the completion style specifically for remote files! See
  ;; https://github.com/minad/vertico#tramp-hostname-completion
  (defun kb/basic-remote-try-completion (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-try-completion string table pred point)))
  (defun kb/basic-remote-all-completions (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-all-completions string table pred point)))
  (add-to-list 'completion-styles-alist
               '(basic-remote           ; Name of `completion-style'
                 kb/basic-remote-try-completion kb/basic-remote-all-completions nil))
  :config
  (vertico-mode)
  ;; Extensions
  (vertico-multiform-mode)

  ;; Prefix the current candidate with “» ”. From
  ;; https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow
  (advice-add #'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index _start)
                (setq cand (funcall orig cand prefix suffix index _start))
                (concat
                 (if (= vertico--index index)
                     (propertize "» " 'face 'vertico-current)
                   "  ")
                 cand))))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package orderless
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)    ; I want to be in control!
  (completion-category-overrides
   '((file (styles basic-remote ; For `tramp' hostname completion with `vertico'
                   orderless
                   ))
     ))

  (orderless-component-separator 'orderless-escapable-split-on-space)
  (orderless-matching-styles
   '(orderless-literal
     orderless-prefixes
     orderless-initialism
     orderless-regexp
     ;; orderless-flex
     ;; orderless-strict-leading-initialism
     ;; orderless-strict-initialism
     ;; orderless-strict-full-initialism
     ;; orderless-without-literal          ; Recommended for dispatches instead
     ))
  (orderless-style-dispatchers
   '(prot-orderless-literal-dispatcher
     prot-orderless-strict-initialism-dispatcher
     prot-orderless-flex-dispatcher
     ))
  :init
  (defun orderless--strict-*-initialism (component &optional anchored)
    "Match a COMPONENT as a strict initialism, optionally ANCHORED.
The characters in COMPONENT must occur in the candidate in that
order at the beginning of subsequent words comprised of letters.
Only non-letters can be in between the words that start with the
initials.

If ANCHORED is `start' require that the first initial appear in
the first word of the candidate.  If ANCHORED is `both' require
that the first and last initials appear in the first and last
words of the candidate, respectively."
    (orderless--separated-by
        '(seq (zero-or-more alpha) word-end (zero-or-more (not alpha)))
      (cl-loop for char across component collect `(seq word-start ,char))
      (when anchored '(seq (group buffer-start) (zero-or-more (not alpha))))
      (when (eq anchored 'both)
        '(seq (zero-or-more alpha) word-end (zero-or-more (not alpha)) eol))))

  (defun orderless-strict-initialism (component)
    "Match a COMPONENT as a strict initialism.
This means the characters in COMPONENT must occur in the
candidate in that order at the beginning of subsequent words
comprised of letters.  Only non-letters can be in between the
words that start with the initials."
    (orderless--strict-*-initialism component))

  (defun prot-orderless-literal-dispatcher (pattern _index _total)
    "Literal style dispatcher using the equals sign as a suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
    (when (string-suffix-p "=" pattern)
      `(orderless-literal . ,(substring pattern 0 -1))))

  (defun prot-orderless-strict-initialism-dispatcher (pattern _index _total)
    "Leading initialism  dispatcher using the comma suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
    (when (string-suffix-p "," pattern)
      `(orderless-strict-initialism . ,(substring pattern 0 -1))))

  (defun prot-orderless-flex-dispatcher (pattern _index _total)
    "Flex  dispatcher using the tilde suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
    (when (string-suffix-p "." pattern)
      `(orderless-flex . ,(substring pattern 0 -1))))
  )

(use-package consult
  :bind
  (;; Buffer switching
   ("C-x b" . consult-buffer)
   ("C-x 4 b" . consult-buffer-other-window)
   ("C-x 5 b" . consult-buffer-other-frame)
   ("M-y" . consult-yank-pop)

   ;; File finding
   ;; ("C-x C-f" . consult-find-file) ;; don't override native

   ;; go-to bindings
   ("M-g g" . consult-goto-line)
   ("M-g M-g" . consult-goto-line)
   ("M-g e" . consult-compile-error)
   ("M-g o" . consult-outline)
   ("M-g m" . consult-mark)
   ("M-g k" . consult-global-mark)

   ;; Search
   ("C-s" . consult-line)
   ("C-r" . consult-line)
   ("C-c s f" . consult-find)
   ("C-c s g" . consult-grep)
   ("C-c s r" . consult-ripgrep)
   ("C-c s l" . consult-line)

   ;; Imenu
   ("M-g i" . consult-imenu)
   ("M-g I" . consult-imenu-multi)

   ;; Bookmark
   ("C-x r b" . consult-bookmark)

   ;; Command execution
   ;;("M-x" . consult-M-x)
   ("C-x M-:" . consult-complex-command)

   ;; Flymake/check
   ("M-g e" . consult-flymake)
   ;;("M-g e" . consult-flycheck)

   ;; Register
   ("C-x r i" . consult-register-load)
   ("C-x r s" . consult-register-store)
   ("C-x r x" . consult-register))

  :config
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<"))

(use-package consult-eglot)

(use-package hl-todo
  :config
  (global-hl-todo-mode))

(use-package colorful-mode
  :hook ((prog-mode text-mode) . colorful-mode)
  :config
  (setq colorful-use-prefix t))

(use-package avy
  :bind (("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-2)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)))

(use-package zop-to-char
  :bind (("M-z" . zop-up-to-char)
         ("M-Z" . zop-to-char)))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package nerd-icons-dired
  :delight
  :hook
  (dired-mode . nerd-icons-dired-mode))

(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)
(global-set-key [remap mark-sexp] 'easy-mark)
(global-set-key [remap kill-ring-save] 'easy-kill)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "s-y") 'browse-kill-ring)

(setq ibuffer-saved-filter-groups
      '(("default"
         ("Dired" (mode . dired-mode))
         ("Org" (mode . org-mode))
         ("Magit" (name . "^magit"))
         ("Shell" (or (mode . eshell-mode)
                      (mode . shell-mode)
                      (mode . term-mode)
                      (mode . vterm-mode)))
         ("Programming" (or (derived-mode . prog-mode)
                            (mode . python-mode)
                            (mode . c-mode)
                            (mode . elixir-mode)
                            (mode . java-mode)
                            (mode . js-mode)))
         ("Comint" (mode . comint-mode))
         ("Emacs" (or (name . "^\\*scratch\\*$")
                      (name . "^\\*Messages\\*$")
                      (name . "^\\*Help\\*$")
                      (name . "^\\*Warnings\\*$")))
         ("Planner" (or (name . "^\\*Calendar\\*$")
                        (name . "^\\*Org Agenda\\*"))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

(use-package lin
  :config
  (lin-global-mode))

;; Configure Tempel
(use-package tempel
  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))
  :init
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'conf-mode-hook #'tempel-setup-capf)
  (add-hook 'prog-mode-hook #'tempel-setup-capf)
  (add-hook 'text-mode-hook #'tempel-setup-capf)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
  )

(use-package rainbow-delimiters
  :hook prog-mode)

(use-package ace-window
  :bind (("M-o" . 'ace-window))
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package easy-kill)

(use-package goggles
  :hook ((prog-mode text-mode) . goggles-mode)
  :delight
  :config
  (setq-default goggles-pulse t))

(use-package anzu
  :delight
  :config
  (global-anzu-mode))

(use-package tempel-collection)

(elpaca expand-region)

;; handle large files
(elpaca vlf)

(add-hook 'prog-mode-hook (lambda () (setq truncate-lines t)))
(add-hook 'text-mode-hook #'visual-line-mode)

(setq-default ispell-program-name "aspell")
(setq-default ispell-extra-args '("--reverse"))

(provide 'conjure-editor)
;;; conjure-editor.el ends here
