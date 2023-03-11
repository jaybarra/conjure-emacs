;;; conjure-flymake.el --- Flymake Initialization
;;; Commentary:
;;; Code:

(add-hook 'prog-mode-hook 'flymake-mode)
(add-hook 'text-mode-hook 'flymake-mode)

(provide 'conjure-flymake)
;;; conjure-flymake.el ends here
