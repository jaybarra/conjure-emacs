;;; terragrunt-mode.el --- Terragrunt editing mode -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defgroup terragrunt nil
  "Group for Terragrunt."
  :prefix "terragrunt-")

(defface terragrunt-highlight-keywords-face
  '((t :inherit (default)))
  "Face for functions"
  :group 'terragrunt-group)

(require 'terraform-mode)


(defvar terragrunt-keywords
  '(("\\(generate\\|include\\|terraform\\|inputs\\)" . 'font-lock-keyword-face)))

(defvar terragrunt-functions
  '(("\\(find_in_parent_folders\\|path_relative_to_include\\|path_relative_from_include\\|get_env\\|get_platform\\|get_repo_route\\|get_aws_account\\)'" . 'font-lock-function-name-face)))

;;;###autoload
(define-minor-mode terragrunt-mode
  "Minor mode for working with Terragrunt files."
  :lighter " terragrunt"
  :group 'terragrunt-group
  (when (bound-and-true-p terragrunt-mode)
    (font-lock-add-keywords nil terragrunt-keywords)
    (font-lock-add-keywords nil terragrunt-functions)
    (font-lock-fontify-buffer))
  (when (not (bound-and-true-p terragrunt-mode))
    (font-lock-remove-keywords nil terragrunt-keywords)
    (font-lock-remove-keywords nil terragrunt-functions)
    (font-lock-fontify-buffer)))

;;;###autoload
(add-hook 'hcl-mode-hook
	  (lambda ()
            (when (string= (file-name-base buffer-file-name) "terragrunt")
	      (terragrunt-mode +1))))

(provide 'terragrunt-mode)

;;; terragrunt-mode.el ends here
