;;; conjure-org.el --- Org Mode Initialization
;;; Commentary:
;;; Code:

(use-package org
  :config
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

  (setq org-src-fontify-natively t
        org-confirm-babel-evaluate nil
        ob-async-no-async-languages-alist '("ipython"))
  
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((js . t)
     (python . t)
     (shell . t))))

(use-package ob-async)

(defvar org-roam-directory (expand-file-name "roam" "~"))

(use-package org-roam
  :bind (("C-c n r" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n I" . org-roam-insert-node-immediate)
         ("C-c n c" . org-roam-capture)
         ("C-c n d d" . org-roam-dailies-goto-today)
         ("C-c n d y" . org-roam-dailies-goto-yesterday)
         ("C-c n d t" . org-roam-dailies-goto-tomorrow)
         ("C-c n d b" . org-roam-dailies-goto-previous-note)
         ("C-c n d c" . org-roam-dailies-goto-date))
  
  :init
  (unless (file-exists-p org-roam-directory)
    (make-directory org-roam-directory))

  :config
  (defun org-roam-insert-node-immediate (arg &rest args)
    "Insert a node without prompting for additional information.
Takes ARG and optionally ARGS as pass-thrus."
    (interactive "P")
    (let ((args (cons arg args))
	  (org-roam-capture-templates (list (append (car org-roam-capture-templates)
						    '(:immediate-finish t)))))
      (apply #'org-roam-node-insert args)))
  
  (setq org-roam-directory (expand-file-name "roam" (file-truename "~"))
        org-roam-db-location (expand-file-name "org-roam.db" conjure-savefile-dir))
  
  (org-roam-db-autosync-mode))

(provide 'conjure-org)

;;; conjure-org.el ends here
