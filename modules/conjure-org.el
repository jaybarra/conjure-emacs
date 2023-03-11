;;; conjure-org.el --- Org Mode Initialization
;;; Commentary:
;;; Code:
(require 'conjure-packages)
(conjure-require-packages '(org
			    org-roam
			    org-roam-ui
			    ob-graphql
			    ob-restclient
			    ox-reveal
			    restclient))

;;; ORG MODE;

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (clojure . t)
     (graphql . t)
     (shell . t)
     (ruby . t)
     (python . t)
     (restclient . t))))

;;; ORG-ROAM:

(require 'org-roam)
(setq org-roam-directory (expand-file-name "roam" (file-truename "~")))

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

(when (fboundp 'consult-org-roam-mode)
  (global-set-key (kbd "C-c n f") 'consult-org-roam-file-find)
  ;;(global-set-key (kbd "C-c n l b") 'consult-org-roam-backlinks)
  ;;(global-set-key (kbd "C-c n l f") 'consult-org-roam-forward-links)
  
  (consult-org-roam-mode +1)
  (diminish 'consult-org-roam-mode))

(org-roam-db-autosync-mode)

(require 'org-roam-ui)
(setq org-roam-ui-open-on-start nil)

(message "[Conjure] org-roam powering up...")

(provide 'conjure-org)
;;; conjure-org.el ends here
