;;; conjure-org.el --- Org Mode Initialization
;;; Commentary:
;;; Code:

(require 'conjure-packages)

(conjure-require-packages '(org
			    org-roam
			    org-roam-ui
                            ob-restclient
			    ox-reveal))

;;; ORG MODE;
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(conjure-require-packages '(ob-async
			    ob-coffee
                            ob-typescript))

(require 'org)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((coffee . t)
   (js . t)
   (python . t)
   (typescript . t)
   (shell . t)))

(setq org-src-fontify-natively t
      org-confirm-babel-evaluate nil
      ob-async-no-async-languages-alist '("ipython"))

;; ignore white space cleanup since it interferes with org-roam node insertions
;; cleanup can still be run manually
(add-hook 'org-mode-hook
          (lambda ()
	    (whitespace-mode -1)
            (setq-local conjure-cleanup-whitespace-on-save nil)))

;;; ORG-ROAM:

(require 'org-roam)
(setq org-roam-directory (expand-file-name "roam" (file-truename "~"))
      org-roam-db-location (expand-file-name "org-roam.db" conjure-savefile-dir))

(unless (file-exists-p org-roam-directory)
  (make-directory org-roam-directory))

(defun org-roam-insert-node-immediate (arg &rest args)
  "Insert a node without prompting for additional information.
fTakes ARG and optionally ARGS as pass-thrus."
  (interactive "P")
  (let ((args (cons arg args))
	(org-roam-capture-templates (list (append (car org-roam-capture-templates)
						  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

(global-set-key (kbd "C-c n r") 'org-roam-buffer-toggle)
(global-set-key (kbd "C-c n f") 'org-roam-node-find)
(global-set-key (kbd "C-c n i") 'org-roam-node-insert)
(global-set-key (kbd "C-c n I") 'org-roam-insert-node-immediate)
(global-set-key (kbd "C-c n c") 'org-roam-capture)
(global-set-key (kbd "C-c n d d") 'org-roam-dailies-goto-today)
(global-set-key (kbd "C-c n d y") 'org-roam-dailies-goto-yesterday)
(global-set-key (kbd "C-c n d t") 'org-roam-dailies-goto-tomorrow)
(global-set-key (kbd "C-c n d b") 'org-roam-dailies-goto-previous-note)
(global-set-key (kbd "C-c n d c") 'org-roam-dailies-goto-date)

(org-roam-db-autosync-mode)

(require 'org-roam-ui)
(setq org-roam-ui-open-on-start nil)

(provide 'conjure-org)

;;; conjure-org.el ends here
