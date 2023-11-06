;;; conjure-vertico.el --- Vertico configuration
;;; Commentary:

;; Uses a more minimalistic set of packages
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

(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))


(use-package orderless
  :custom
  (defun affe-orderless-regexp-compiler (input _type _ignorecase)
    (setq input (orderless-pattern-compiler input))
    (cons input (apply-partially #'orderless--highlight input)))

  (setq affe-regex-compiler #'affe-orderless-regexp-compiler)
  
  (completion-styles '(orderless flex partial-completion)))

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

(use-package affe
  :config
  (consult-customize affe-grep :preview-key "M-."))

(consult-customize consult-theme :preview-key '(:debounce 0.2 any)
                   :preview-key '(:debounce 0.4 any))

(setq consult-narrow-key "<"
      consult-goto-line-numbers nil)

(advice-add #'register-preview :override #'consult-register-window)

(use-package xref
  :init
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref
	xref-search-program (cond
			     ((executable-find "ugrep") 'ugrep)
			     ((executable-find "rg") 'ripgrep)
			     (t 'grep))))

(add-hook 'completion-list-mode-hook 'consult-preview-at-point-mode)

;; Setup annotations for Vertico
(use-package marginalia
  :bind
  (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :config
  (setq marginalia-max-relative-age 0)
  (setq marginalia-align 'right)
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


(use-package dabbrev
  :bind
  (("M-/" . dabbrev-completion)
   ("C-M-/" . dabbrev-expand))
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(define-key corfu-map (kbd "M-SPC") 'corfu-insert-separator)
(define-key corfu-map (kbd "TAB") 'corfu-next)
(define-key corfu-map [tab] 'corfu-next)
(define-key corfu-map (kbd "S-TAB") 'corfu-previous)
(define-key corfu-map [backtab] 'corfu-previous)
(define-key corfu-map (kbd "S-<return>") 'corfu-insert)

;; (with-eval-after-load 'org-roam
;;   (global-set-key (kbd "C-c n f") 'consult-org-roam-file-find)

;;   (consult-org-roam-mode +1)
;;   (diminish 'consult-org-roam-mode))

(provide 'conjure-vertico)

;;; conjure-vertico.el ends here
