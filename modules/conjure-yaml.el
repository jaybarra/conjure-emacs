;;; conjure-yaml.el --- YAML configuration
;;; Commentary:
;;; Code:
(conjure-require-packages '(lsp-mode))

(add-hook 'yaml-mode-hook 'lsp-deferred)

;; adds AWS cloudformation tag support
(setq lsp-yaml-custom-tags ["!Base64 scalar"
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
			    "!Ref scalar"])

(provide 'conjure-yaml)
;;; conjure-yaml.el ends here
