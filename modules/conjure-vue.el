;;; conjure-vue.el --- Vue Initialization
;;; Commentary:
;;; Code:

(use-package vue-mode
  :ensure t
  :defer t
  :config
  (add-hook 'vue-mode-hook 'apheleia-mode))

(provide 'conjure-vue)
;;; conjure-vue.el ends here
