;;; conjure-python.el --- Python configuration
;;; Commentary:
;;; Code:

(require 'conjure-programming)
(conjure-require-packages '(eglot))

(when (fboundp 'exec-path-from-shell-copy-env)
  (exec-path-from-shell-copy-env "PYTHONPATH"))

(defun conjure-python-mode-defaults ()
  "Sensible defaults for `python-mode'."
  (subword-mode +1)
  (anaconda-mode +1)
  (eldoc-mode +1)

  (setq python-shell-interpreter "python3"
        ;;python-shell-interpreter-args "console --simple-prompt"
	python-shell-prompt-detect-failure-warning nil)

  (add-to-list 'python-shell-completion-native-disabled-interpreters "python3"))

(setq conjure-python-mode-hook 'conjure-python-mode-defaults)
(add-hook 'python-mode-hook (lambda ()
                              (run-hooks 'conjure-python-mode-hook)))

(provide 'conjure-python)
;;; conjure-python.el ends here
