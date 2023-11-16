;;; conjure-yaml.el --- YAML configuration
;;; Commentary:
;;; Code:

(when (executable-find "yaml-language-server")
  (add-hook 'yaml-ts-mode-hook 'eglot-ensure))

(defun conjure-yaml-mode-defaults ()
  (setq-local tab-width 2)
  (whitespace-mode +1))

(setq conjure-yaml-mode-hook #'conjure-yaml-mode-defaults)

(add-hook 'yaml-ts-mode-hook (lambda () (run-hooks 'conjure-yaml-mode-hook)))

(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))

(use-package apheleia
  :hook yaml-ts-mode)

(require 'eglot)
(setq eglot-workspace-configuration
      '((yaml . ((:customTags . ["!Base64 scalar"
				 "!Cidr scalar"
				 "!And sequence"
				 "!Equals sequence"
				 "!If sequence"
				 "!Not sequence"
				 "!Or sequence"
				 "!Condition scalar"
				 "!FindInMap sequence"
				 "!GetAtt scalar"
				 "!GetAtt sequence"
				 "!GetAZs scalar"
				 "!ImportValue scalar"
				 "!Join sequence"
				 "!Select sequence"
				 "!Split sequence"
				 "!Sub scalar"
				 "!Transform mapping"
				 "!Ref scalar"])))))

(provide 'conjure-yaml)

;;; conjure-yaml.el ends here
