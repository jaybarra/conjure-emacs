;;; conjure-nextflow.el --- Configurations for Nextflow in Conjure
;;; Commentary:
;;; Code:
(use-package groovy-mode :ensure t :defer t)

(define-derived-mode nextflow-mode groovy-mode "Nextflow"
  "A mode for Nextflow workflow scripts."

  (when '(fboundp 'nerd-icons-extension-icon-alist)
    (add-to-list 'nerd-icons-extension-icon-alist
                 '("nf" nerd-icons-octicon "nf-oct-workflow" :face nerd-icons-green)))

  (defvar nextflow-mode-font-lock-keywords
    (let ((nextflow-keywords `(
                               ;; Highlight `process` and `workflow` keywords and following names
                               ("\\<\\(process\\|workflow\\)\\>\\s-+\\([A-Za-z0-9_]+\\)"
                                (1 font-lock-keyword-face)
                                (2 font-lock-type-face))

                               ;; Highlight built-in Nextflow keywords with a colon
                               ("\\<\\(input\\|output\\|script\\|when\\):"
                                . font-lock-builtin-face)
                               )))
      ;; Combine Nextflow keywords with groovy-mode's font-lock keywords
      (append groovy-font-lock-keywords nextflow-keywords))
    "Additional font-lock keywords for Nextflow mode.")

  (set (make-local-variable 'font-lock-defaults) '(nextflow-mode-font-lock-keywords)))

(add-to-list 'auto-mode-alist '("\\.nf\\'" . nextflow-mode))

(require 'imenu)
(defun nextflow-imenu-create-index ()
  (let* ((processes-re "\\<\\(process\\|workflow\\)\\>\\s-+\\([A-Za-z0-9_]+\\)")
         (defs-re "\\<def\\>\\s-+\\([A-Za-z0-9_]+\\)")
         (input-re "^\\s-+\\(val\\|path\\|tuple\\|stdin\\|set\\)\\s-+\\(\\(val\\|path\\)(?\\(\\w+\\))?,\\s-*\\(\\(val\\|path\\)\\)(?\\(\\w+\\))?\\|\\w+\\)")
         (inputs '())
         (processes '())
         (variables '())
         (index '()))
    (save-excursion
      ;; Process/Workflow
      (goto-char (point-min))
      (while (re-search-forward processes-re nil t)
        (let ((match (match-string 2)))
          (push (cons match (match-beginning 2)) processes)))
      ;; Inputs
      (goto-char (point-min))
      (while (re-search-forward input-re nil t)
        (cond
         ((match-string 7)
          (push (cons (concat (format "[tuple] %s: " (match-string 3))) (match-beginning 7)) inputs)
          (push (cons (concat (format "[tuple] %s: " (match-string 6))) (match-beginning 7)) inputs))
         ((match-string 2) (push (cons (concat (format "%s:" (match-string 1)) (match-string 2)) (match-beginning 0)) inputs))))
      ;; Variables
      (goto-char (point-min))
      (while (re-search-forward defs-re nil t)
        (let ((match (match-string 1)))
          (push (cons match (match-beginning 1)) variables))))
    ;; Construct the index
    (when processes
      (push (cons "Processes" (nreverse processes)) index))
    (when inputs
      (push (cons "Inputs" (nreverse inputs)) index))
    (when variables
      (push (cons "Variables" (nreverse variables)) index))
    (nreverse index)))

(defun setup-nextflow-imenu ()
  "Setup `imenu' for Nextflow files."
  (setq imenu-create-index-function 'nextflow-imenu-create-index))

(add-hook 'nextflow-mode-hook 'setup-nextflow-imenu)

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

(provide 'conjure-nextflow)
;;; conjure-nextflow.el ends here
