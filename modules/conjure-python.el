;;; conjure-python.el --- Python configuration
;;; Commentary:
;;; Code:

(require 'conjure-programming)
(conjure-require-packages '(eglot))

(when (fboundp 'exec-path-from-shell-copy-env)
  (exec-path-from-shell-copy-env "PYTHONPATH"))

(require 'python)
(defun conjure-python-mode-defaults ()
  "Sensible defaults for `python-mode'."
  (subword-mode +1)
  (eldoc-mode +1)

  (setq python-shell-interpreter "jupyter"
        python-shell-interpreter-args "console --simple-prompt"
        python-shell-completion-native-enable nil))

(setq conjure-python-mode-hook 'conjure-python-mode-defaults)
(add-hook 'python-mode-hook (lambda ()
                              (run-hooks 'conjure-python-mode-hook)))
(add-hook 'python-mode-hook 'eglot-ensure)

(provide 'conjure-python)
;;; conjure-python.el ends here
