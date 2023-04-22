;;; conjure-vertico.el --- Vertico configuration
;;; Commentary:

;; Uses a more minimalistic set of packages
;;; Code:

(conjure-require-packages '(affe consult corfu marginalia orderless vertico))

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

(require 'consult)


(require 'affe)
(defun affe-orderless-regexp-compiler (input _type _ignorecase)
  (setq input (orderless-pattern-compiler input))
  (cons input (apply-partially #'orderless--highlight input)))
(setq affe-regex-compiler #'affe-orderless-regex-compiler)

(setq completion-styles '(orderless basic)
      completion-category-overrides '((file (styles partial-completion))))

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

;; IntelliJ style keybindings
(global-set-key (kbd "s-E") 'consult-projectile-recentf)
(global-set-key (kbd "s-e") 'consult-projectile-find-file)
(global-set-key (kbd "C-s-e") 'consult-recent-file)

(global-set-key (kbd "C-s") 'consult-line)

;; configure register formatting
(setq register-preview-delay 0.5
      register-preview-function #'consult-register-format)

(consult-customize affe-grep :preview-key "M-.")
(consult-customize consult-theme :preview-key '(:debounce 0.2 any)
                   :preview-key '(:debounce 0.4 any))

(setq consult-narrow-key "<")

(advice-add #'register-preview :override #'consult-register-window)

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
(setq corfu-cycle t
      corfu-auto t
      corfu-auto-prefix 2
      corfu-auto-delay 0.0
      corfu-quit-at-boundary 'separator
      corfu-preview-current 'insert
      corfu-preselect 'valid)

(define-key corfu-map (kbd "M-SPC") 'corfu-insert-separator)
(define-key corfu-map (kbd "TAB") 'corfu-next)
(define-key corfu-map [tab] 'corfu-next)
(define-key corfu-map (kbd "S-TAB") 'corfu-previous)
(define-key corfu-map [backtab] 'corfu-previous)
(define-key corfu-map (kbd "S-<return>") 'corfu-insert)

(global-corfu-mode)

(provide 'conjure-vertico)

;;; conjure-vertico.el ends here
