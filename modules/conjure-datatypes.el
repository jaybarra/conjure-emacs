;;; conjure-datatypes.el --- Map data-types to modes
;;; Commentary:
;;; Code:

;; Data Formats
(add-to-list 'auto-mode-alist '("\\.echo10\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.iso-smap\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.iso19115\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.kml\\'" . nxml-mode))

;; handle "common" js and "module" js syntax
(add-to-list 'auto-mode-alist '("\\.[cm]js\\'" . js-mode))

;; Shell Scripts
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))

(provide 'conjure-datatypes)

;;; conjure-datatypes.el ends here
