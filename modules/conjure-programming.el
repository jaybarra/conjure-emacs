;;; conjure-programming.el --- Configurations for Programming with Conjure
;;; Commentary:
;;; Code:

(require 'eglot)
(with-eval-after-load 'eglot
  (setq eglot-events-buffer-size 0
        eglot-ignored-server-capabilities '(:hoverProvider
                                            :documentHighlightProvider)
        eglot-autoshutdown t))

(set-language-environment 'utf-8)

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; show the current function def in the modeline
(require 'which-func)
(setq which-func-unknown "⊥")
(add-hook 'prog-mode-hook (lambda () (which-function-mode +1)))

(use-package apheleia
  :ensure t
  :hook prog-mode)

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode text-mode))

;; font-lock annotations like TODO
(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode +1))


;; flycheck style commands for flymake
(define-key prog-mode-map (kbd "C-c ! n") 'flymake-goto-next-error)
(define-key prog-mode-map (kbd "C-c ! p") 'flymake-goto-prev-error)
(define-key prog-mode-map (kbd "C-c ! c") 'flymake-start)

(define-key text-mode-map (kbd "C-c ! n") 'flymake-goto-next-error)
(define-key text-mode-map (kbd "C-c ! p") 'flymake-goto-prev-error)
(define-key text-mode-map (kbd "C-c ! c") 'flymake-start)


(defun conjure-prog-mode-defaults ()
  "Sensible defaults for `prog-mode'."
  (hl-line-mode +1)
  (setq-local display-line-numbers 'relative))

(add-hook 'prog-mode-hook #'conjure-prog-mode-defaults)


(use-package groovy-mode
  :ensure t
  :mode (("\\.groovy\\'" . groovy-mode)
         ("Jenkinsfile\\'" . groovy-mode))
  :config
  (setq groovy-indent-offset 4))

(define-derived-mode nextflow-mode groovy-mode "Nextflow"
  "A mode for Nextflow workflow scripts."

  (when '(fboundp 'nerd-icons-extension-icon-alist)
    (add-to-list 'nerd-icons-extension-icon-alist
                 '("nf" nerd-icons-octicon "nf-oct-workflow" :face nerd-icons-green)))

  (defvar nextflow-mode-font-lock-keywords
    (let ((nextflow-keywords `(
                               ;; Highlight `process` and `workflow` keywords and following names
                               ("\\<\\(process\\|workflow\\)\\>\\s-+\\(\\sw+\\)"
                                (1 font-lock-keyword-face)
                                (2 font-lock-type-face))

                               ;; Highlight built-in Nextflow keywords with a colon
                               ("\\<\\(input\\|output\\|script\\):"
                                . font-lock-builtin-face)
                               )))
      ;; Combine Nextflow keywords with groovy-mode's font-lock keywords
      (append groovy-font-lock-keywords nextflow-keywords))
    "Additional font-lock keywords for Nextflow mode.")
  
  (set (make-local-variable 'font-lock-defaults) '(nextflow-mode-font-lock-keywords)))

(add-to-list 'auto-mode-alist '("\\.nf\\'" . nextflow-mode))

(defvar nextflow-default-command "nextflow run main.nf")

(defun run-nextflow-pipeline (update-args)
  "Run the Nextflow pipeline with optional arguments.
With a prefix argument (C-u), prompt for new arguments to update."
  (interactive "P") ;; "P" makes the function receive the raw prefix argument.
  (let ((default-args (if (boundp 'nextflow-pipeline-args) nextflow-pipeline-args "")))
    ;; Check if called with C-u and prompt for new args if so
    (when update-args
      (setq default-args (read-string "Enter Nextflow arguments: " default-args))
      ;; Optionally update the local variable if you want the new args to persist for the session
      (when (boundp 'my-nextflow-args)
        (setq nextflow-pipline-args default-args)))
    ;; Run the compile command with the args
    (compile (format "%s %s" nextflow-default-command default-args))))

(defun run-nextflow-pipeline-resume (params)
  "Resume a Nextflow pipeline in the current directory."
  (interactive "P")
  (let* ((arg-list (if params
                       (split-string params)
                     '("")))
         (nextflow-command (format "nextflow run main.nf -resume %s" (mapconcat 'identity params " "))))
    (compile nextflow-command)))

(define-key nextflow-mode-map [f7] 'run-nextflow-pipeline)
(define-key nextflow-mode-map [f8] 'run-nextflow-pipeline-resume)

(use-package clojure-mode
  :ensure t
  :after smartparens
  :config
  (sp-local-pair '(clojure-mode) "'" nil :actions nil)

  (defun conjure-clojure-mode-defaults ()
    "Sensible defaults for `clojure-mode'."
    (subword-mode +1)
    (smartparens-strict-mode +1)
    (local-unset-key (kbd "C-:")))

  (add-hook 'clojure-mode-hook #'conjure-clojure-mode-defaults)
  
  (define-clojure-indent
   ;; Compojure defaults
   (defroutes 'defun)
   (GET 2)
   (POST 2)
   (PUT 2)
   (DELETE 2)
   (HEAD 2)
   (ANY 2)
   (OPTIONS 2)
   (PATCH 2)
   (rfn 2)
   (let-routes 1)
   (context 2)

   ;; Midje testing
   ;; https://github.com/marick/Midje
   (fact 1)
   (facts 1)))

(use-package cider
  :ensure t
  :after clojure-mode
  :config
  (setq cider-print-fn 'fipp
        cider-repl-display-help-banner nil
        cider-repl-result-prefix ";; =>"
        cider-print-options '(("print-length" 100))))

(use-package svelte-mode :ensure t)

(use-package terraform-mode
  :ensure t
  :config
  (when (executable-find "terraform-ls")
    (add-to-list 'eglot-server-programs '(terraform-mode "terraform-ls" "serve"))
    (add-hook 'terraform-mode-hook #'eglot-ensure)))

;; R support - emacs speaks statistics
(use-package ess :ensure t)

(provide 'conjure-programming)

;;; conjure-programming.el ends here
