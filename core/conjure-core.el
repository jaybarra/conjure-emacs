;;; conjure-core.el --- Conjure core configurations
;;; Commentary:
;;; Code:

;; Straight Configuration ====================================================

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package '(use-package :type built-in))

(setq straight-use-package-by-default t)
(setq package-enable-at-startup nil)

(provide 'conjure-core)

;;; conjure-core.el ends here
