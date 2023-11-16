;;; conjure-markdown.el --- Markdown configuration
;;; Commentary:
;;; Code:

(require 'conjure-programming)

(use-package markdown-mode
  :config
  ;; markdown doesn't have autoloads, manually add them
  (autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
  
  (add-to-list 'auto-mode-alist
               '("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode))
  
  ;; GithubFlavored markdown
  (autoload 'gfm-mode "markdown-mode"
    "Major mode for editing GitHub Flavored Markdown files" t)
  (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode)))





(provide 'conjure-markdown)

;;; conjure-markdown.el ends here
