;;; conjure-org.el --- Org Mode Initialization
;;; Commentary:
;;; Code:
(conjure-require-packages '(org
			    org-roam
			    org-roam-ui
			    ob-graphql
			    ob-restclient
			    ox-reveal
			    restclient))

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

(require 'org-roam)
(setq org-roam-directory (expand-file-name "org/roam" (file-truename "~")))

(defun org-roam-insert-node-immediate (arg &rest args)
  "Insert a node without prompting for additional information.
Takes ARG and optionally ARGS as pass-thrus."
  (interactive "P")
  (let ((args (cons arg args))
	(org-roam-capture-templates (list (append (car org-roam-capture-templates)
						  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

(defun conjure-org-roam-defaults ()
  "Configure sensible defaults for `org-roam'."
  (global-set-key (kbd "C-c m l") 'org-roam-buffer-toggle)
  (global-set-key (kbd "C-c m f") 'org-roam-node-find)
  (global-set-key (kbd "C-c m i") 'org-roam-node-insert)
  (global-set-key (kbd "C-c m I") 'org-roam-insert-node-immediate)
  (global-set-key (kbd "C-c m c") 'org-roam-capture)
  (global-set-key (kbd "C-c m d d") 'org-roam-dailies-goto-today)
  (global-set-key (kbd "C-c m d y") 'org-roam-dailies-goto-yesterday)
  (global-set-key (kbd "C-c m d t") 'org-roam-dailies-goto-tomorrow)
  (global-set-key (kbd "C-c m d b") 'org-roam-dailies-goto-previous-note)
  (global-set-key (kbd "C-c m d c") 'org-roam-dailies-goto-date)

  (define-key org-mode-map (kbd "C-M-i") 'completion-at-point)

  (setq org-roam-ui-open-on-start nil)

  (org-roam-db-autosync-mode)
  (message "[Conjure] org-roam powering up..."))

(setq conjure-org-roam-hook 'conjure-org-roam-defaults)
(add-hook 'after-init-hook (lambda ()
			     (run-hooks 'conjure-org-roam-hook)))

(provide 'conjure-org)
;;; conjure-org.el ends here
