;;; conjure-data-science.el --- Data Science Packages
;;; Commentary:
;;; Code:

(use-package julia-mode :ensure t :defer t)

;; CSV manipulation in emacs-lisp
(use-package csv :ensure t :defer t)

;; Be able to manipulate CSV files
(use-package csv-mode :ensure t :defer t)

;; Handle R
(use-package ess :ensure t :defer t)

(provide 'conjure-data-science)
;;; conjure-data-science.el ends here
