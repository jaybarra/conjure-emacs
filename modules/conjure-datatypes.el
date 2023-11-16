;;; conjure-datatypes.el --- Map data-types to modes
;;; Commentary:
;;; Code:

;; Data Formats
(add-to-list 'auto-mode-alist '("\\.echo10\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.iso-smap\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.iso19115\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.kml\\'" . nxml-mode))

;; Shell Scripts
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . bash-ts-mode))

;; Development
(add-to-list 'auto-mode-alist '("\\.env(\.(local|prod|production|development))?\\'" . conf-mode))

(provide 'conjure-datatypes)

;;; conjure-datatypes.el ends here
