;;; conjure-copilot.el --- GitHub Co-Pilot integration
;;; Commentary:

;; This module requires a Copilot subscription

;;; Code:

(use-package editorconfig)

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)
              ("C-S-TAB" . 'copilot-next-completion)
              ("C-S-<tab>" . 'copilot-next-completion))
  ;; :config
  ;; (add-to-list 'copilot-major-mode-alist '("enh-ruby" . "ruby"))
  )

;; To enable
;; m-x copilot-login

(provide 'conjure-copilot)

;;; conjure-copilot.el ends here
