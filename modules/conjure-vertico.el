;;; conjure-vertico.el --- Vertico configuration
;;; Commentary:
;;; Code:

(use-package vertico
  :custom
  (vertico-count 12)
  (vertico-resize nil)
  (vertico-cycle nil)
  :bind
  (:map vertico-map
        ("<tab>" . vertico-insert)
        ("<escape>" . minibuffer-keyboard-quit)
        ("C-M-n" . vertico-next-group)
        ("C-M-p" . vertico-previous-group))
  :init
  (vertico-mode))

;; Prefix the current candidate with “» ”. From
;; https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow
(advice-add #'vertico--format-candidate :around
            (lambda (orig cand prefix suffix index _start)
              (setq cand (funcall orig cand prefix suffix index _start))
              (concat
               (if (= vertico--index index)
                   (propertize "» " 'face 'vertico-current)
                 "  ")
               cand)))

(require 'emacs)
(defun crm-indicator (args)
  "Extend ARGS for a prompt indicator to `completing-read-multiple'."
  (cons (format "[CRM%s] %s"
                (replace-regexp-in-string
                 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                 crm-separator)
                (car args))
        (cdr args)))

(advice-add #'completing-read-multiple :filter-args #'crm-indicator)

(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(setq enable-recursive-minibuffers t)

(setq consult-flyspell-select-function 'flyspell-correct-at-point)

(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  (advice-add #'register-preview :override #'consult-register-window)
  
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :bind (;; C-c bindings
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r b" . consult-bookmark)
         ("C-x p b " . consult-project-buffer)
         ;; Custom bindings
         ("s-l" . consult-register-load)
         ("s-'" . consult-register-store)
         ("C-M-'" . consult-register)
         ;; Other
         ("M-y" . consult-yank-pop)
         ;; M-g (goto) bindings
         ("M-g e" . consult-compile-error)
         ("M-g g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ;; M-i bindings
         ("M-i" . consult-imenu)
         ("M-I" . consult-imenu-multi)
         ;; M-s bindings
         ("M-s f" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep) 
         ("s-a" . consult-ripgrep)
         ("M-s l" . consult-line) 
         ("M-s L" . consult-line-multi) 
         ("M-s k" . consult-keep-lines) 
         ;; ("C-x u" . consult-focus-lines) ;; s-s u;; conflicts with undo-tree
         ;; isearch integrations
         ("M-s e" . consult-isearch-history)
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("s-r" . consult-history))
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  (setq consult-narrow-key "<")

  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root))))

(use-package consult-dir
  :bind (("C-x C-d" . 'consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . 'consult-dir)
         ("C-x C-j" . 'consult-dir-jump-file)))

(use-package consult-eglot)

(use-package consult-flycheck)

(use-package affe)
(use-package orderless
  :config
  (defun affe-orderless-regexp-compiler (input _type _ignorecase)
    (setq input (orderless-pattern-compiler input))
    (cons input (apply-partially #'orderless--highlight input)))

  (setq affe-regex-compiler #'affe-orderless-regexp-compiler)
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package consult-projectile
  :straight (consult-projectile :type git :host gitlab :repo "OlMon/consult-projectile" :branch "master")
  :bind (("C-x p p" . consult-projectile)
         ("C-x p s" . consult-projectile-switch-project)
         ("s-f" . consult-projectile-find-file)
         ;;("s-g d" . consult-projectile-find-file)
         ("s-e" . consult-projectile-recentf)
         ("s-b" . consult-projectile-switch-to-buffer)))

(use-package xref
  :init
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref
	xref-search-program (cond
			     ((executable-find "ugrep") 'ugrep)
			     ((executable-find "rg") 'ripgrep)
			     (t 'grep))))

;; Setup annotations for Vertico
(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :config
  (setq marginalia-align 'left)
  (marginalia-mode))

;; Corfu Auto-complete
(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.2)
  (corfu-preselect-first nil)
  (corfu-preview-current 'insert)
  :init
  (global-corfu-mode))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block))

(define-key corfu-map (kbd "M-SPC") 'corfu-insert-separator)
(define-key corfu-map (kbd "TAB") 'corfu-next)
(define-key corfu-map [tab] 'corfu-next)
(define-key corfu-map (kbd "S-TAB") 'corfu-previous)
(define-key corfu-map [backtab] 'corfu-previous)
(define-key corfu-map (kbd "S-<return>") 'corfu-insert)

(provide 'conjure-vertico)

;;; conjure-vertico.el ends here
