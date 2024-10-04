;;; conjure-org.el --- Configurations for Org in Conjure -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(elpaca org-roam-ui)
(elpaca org-modern)

(with-eval-after-load 'org
  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

  (setq
   ;; Core behavior
   org-directory "~/org"
   org-default-notes-file (concat org-directory "/notes.org")
   org-agenda-files '("~/org/agenda.org")

   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t

   ;; Agenda styling
   org-agenda-tags-column 0
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "◀── now ─────────────────────────────────────────────────")

  ;; Ellipsis styling
  (setq org-ellipsis "…")
  (set-face-attribute 'org-ellipsis nil :inherit 'default :box nil))

(use-package org-roam
  :custom
  (org-roam-db-location (expand-file-name "org-roam.db" conjure-savefile-dir))
  :bind (("C-c n i" . org-roam-node-insert)
         ("C-c n I" . org-roam-node-insert-immediate)
         ("C-c n f" . org-roam-node-find)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today)
         ("C-c n t" . org-roam-dailies-goto-today)
         ("C-s-["   . org-roam-dailies-goto-previous-note)
         ("C-s-]"   . org-roam-dailies-goto-next-note))
  :config
  (require 'org-roam-dailies)

  (defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

  (org-roam-db-autosync-mode)

  (defun conjure--org-roam-filter-by-tag (tag-name)
    (lambda (node)
      (member tag-name (org-roam-node-tags node))))

  (defun conjure--org-roam-list-notes-by-tag (tag-name)
    (mapcar #'org-roam-node-file
            (seq-filter
             (conjure--org-roam-filter-by-tag tag-name)
             (org-roam-node-list))))

  (defun conjure-org-roam-refresh-agenda-list ()
    (interactive)
    (setq org-agenda-files (append org-agenda-files (conjure--org-roam-list-notes-by-tag "Project"))))
  (conjure-org-roam-refresh-agenda-list))

(provide 'conjure-org)
;;; conjure-org.el ends here
