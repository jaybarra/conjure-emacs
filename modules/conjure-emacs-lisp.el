;;; init-emacs-lisp.el --- emacs-lisp mode initialization
;;; Commentary:
;;; Code:
(conjure-require-packages '(elisp-slime-nav))

(defun conjure-recompile-elc-on-save ()
  "Recompile elc when saving an elisp file."
  (add-hook 'after-save-hook
            (lambda ()
              (when (and
                     (string-prefix-p conjure-dir (file-truename buffer-file-name))
                     (file-exists-p (byte-compile-dest-file buffer-file-name)))
                (emacs-lisp-byte-compile)))
            nil
            t))

(defun conjure-conditional-emacs-lisp-checker ()
  "Don't check doc style in Emacs Lisp test files."
  (let ((file-name (buffer-file-name)))
    (when (and file-name (string-match-p ".*-tests?\\.el\\'" file-name))
      (setq-local flycheck-checkers '(emacs-lisp)))))

(require 'smartparens-config)
(defun conjure-emacs-lisp-mode-defaults ()
  "Sensible defaults for `emacs-lisp-mode'."
  (run-hooks 'conjure-lisp-coding-hook)
  (eldoc-mode +1)

  (smartparens-strict-mode +1)
  (sp-local-pair '(emacs-lisp-mode) "'" "'" :actions nil)

  (conjure-recompile-elc-on-save)
  (conjure-conditional-emacs-lisp-checker)
  (prettify-symbols-mode +1)
  (checkdoc-minor-mode +1))

(setq conjure-emacs-lisp-mode-hook 'conjure-emacs-lisp-mode-defaults)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (run-hooks 'conjure-emacs-lisp-mode-hook)))

(add-to-list 'auto-mode-alist '("Cask\\'" . emacs-lisp-mode))

(provide 'conjure-emacs-lisp)
;;; conjure-emacs-lisp.el ends here
