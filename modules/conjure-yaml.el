;;; conjure-yaml.el --- YAML configuration
;;; Commentary:
;;; Code:
(require 'conjure-packages)

(conjure-require-packages '(yaml-mode))

(when (executable-find "yaml-language-server")
  (add-hook 'yaml-mode-hook 'eglot-ensure))


(setq eglot-workspace-configuration '((yaml . ((:customTags . ["!Base64 scalar"
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
