;;; conjure-vertico.el --- Vertico configuration
;;; Commentary:

;; Uses a more minimalistic set of packages
;;; Code:

(require 'conjure-packages)
(conjure-require-packages '(affe
			    consult
			    consult-ag
			    consult-eglot
			    consult-flyspell
			    consult-projectile
			    consult-org-roam
			    corfu
                            embark-consult
                            flyspell-correct
                            kind-icon
                            marginalia
                            orderless
			    vertico))

(require 'vertico)
(setq vertico-cycle t
      vertico-count 18)

(vertico-mode)

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

(require 'embark-consult)
(add-hook 'embark-collect-mode-hook 'consult-preview-at-point-mode)

(require 'affe)
(require 'orderless)

(defun affe-orderless-regexp-compiler (input _type _ignorecase)
  (setq input (orderless-pattern-compiler input))
  (cons input (apply-partially #'orderless--highlight input)))

(setq affe-regexp-compiler #'affe-orderless-regexp-compiler)

(setq completion-styles '(orderless basic)
      completion-category-overrides '((file (styles basic partial-completion))))

;; C-c bindings
(global-set-key (kbd "C-c M-x") 'consult-mode-command)
(global-set-key (kbd "C-c h") 'consult-history)
(global-set-key (kbd "C-c k") 'consult-kmacro)
(global-set-key (kbd "C-c i") 'consult-info)
(global-set-key (kbd "C-c m") 'consult-man)
;; C-x bindings
(global-set-key (kbd "C-x M-:") 'consult-complex-command)
(global-set-key (kbd "C-x b") 'consult-buffer)
(global-set-key (kbd "C-x j") 'consult-recent-file)
(global-set-key (kbd "C-x f") 'affe-find)
(global-set-key (kbd "C-x r l") 'consult-bookmark)
(global-set-key (kbd "C-x r j") 'consult-register)
(global-set-key (kbd "C-x m") 'consult-yasnippet)
(global-set-key (kbd "C-x i") 'consult-imenu)
(global-set-key (kbd "C-x I") 'consult-imenu-multi)
;; M-g bindings go-to
(global-set-key (kbd "M-g g") 'consult-goto-line)
(global-set-key (kbd "M-g M-g") 'consult-goto-line)
(global-set-key (kbd "M-g f") 'consult-flymake)
(global-set-key (kbd "M-g m") 'consult-mark)
(global-set-key (kbd "M-g i") 'consult-imenu)
(global-set-key (kbd "M-g o") 'consult-outline)

;; prefer ripgrep
(cond ((executable-find "rg") (global-set-key (kbd "C-s-f") 'consult-ripgrep))
      (t (global-set-key (kbd "C-s-f") 'consult-grep)))

;; IntelliJ style keybindings
(global-set-key (kbd "s-E") 'consult-projectile-recentf)
(global-set-key (kbd "s-e") 'consult-projectile-find-file)
(global-set-key (kbd "C-s-e") 'consult-recent-file)

(global-set-key (kbd "C-s") 'consult-line)

(with-eval-after-load 'projectile
  (define-key projectile-mode-map [remap projectile-switch-project] 'consult-projectile-switch-project)
  (define-key projectile-mode-map [remap projectile-find-file] 'consult-projectile-find-file))

;; configure register formatting
(setq register-preview-delay 0.5
      register-preview-function #'consult-register-format)

(consult-customize affe-grep :preview-key "M-.")
(consult-customize consult-theme :preview-key '(:debounce 0.2 any)
                   :preview-key '(:debounce 0.4 any))

(setq consult-narrow-key "<"
      consult-goto-line-numbers nil)

(advice-add #'register-preview :override #'consult-register-window)

(require 'xref)
(setq xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)

(add-hook 'completion-list-mode-hook 'consult-preview-at-point-mode)

;; Setup annotations for Vertico
(require 'marginalia)
(customize-set-variable 'marginalia-annotators
                        '(marginalia-annotators-heavy
                          marginalia-annotators-light
                          nil))
(marginalia-mode)

;; Corfu Auto-complete
(require 'corfu)
(setq corfu-cycle nil
      corfu-auto t
      corfu-separator ?\s)

(define-key corfu-map (kbd "M-SPC") 'corfu-insert-separator)
(define-key corfu-map (kbd "TAB") 'corfu-next)
(define-key corfu-map [tab] 'corfu-next)
(define-key corfu-map (kbd "S-TAB") 'corfu-previous)
(define-key corfu-map [backtab] 'corfu-previous)
(define-key corfu-map (kbd "S-<return>") 'corfu-insert)

(global-corfu-mode)

(require 'cape)
(add-to-list 'completion-at-point-functions 'cape-dabbrev)
(add-to-list 'completion-at-point-functions 'cape-file)
(add-to-list 'completion-at-point-functions 'cape-elisp-block)

(require 'kind-icon)
(setq kind-icon-default-face 'corfu-default
      ;; TODO waiting for Emacs 29 to hopefully resolve svg issues on Mac/ventura
      kind-icon-use-icons (if osx-p nil t))
(add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)

(with-eval-after-load 'org-roam
  (global-set-key (kbd "C-c n f") 'consult-org-roam-file-find)
  ;;(global-set-key (kbd "C-c n l b") 'consult-org-roam-backlinks)
  ;;(global-set-key (kbd "C-c n l f") 'consult-org-roam-forward-links)

  (consult-org-roam-mode +1)
  (diminish 'consult-org-roam-mode))

(provide 'conjure-vertico)

;;; conjure-vertico.el ends here
