;;; conjure-ivy.el --- Ivy configuration
;;; Commentary:
;;; Code:

(conjure-require-packages '(ivy swiper counsel))

(require 'ivy)
(require 'diminish)

(ivy-mode 1)
(setq ivy-use-virtual-buffers t
      enable-recursive-minibuffers t)

(diminish 'ivy-mode)

(provide 'conjure-ivy)
;;; conjure-ivy.el ends here
