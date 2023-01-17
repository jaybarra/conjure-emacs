;;; conjure-python.el --- Python configuration
;;; Commentary:
;;; Code:
(conjure-require-packages '(pipenv))

(setq python-shell-interpreter "python3")

(add-hook 'python-mode-hook 'eglot-ensure)

(require 'pipenv)
(add-hook 'python-mode-hook #'pipenv-mode)

(with-eval-after-load 'pipenv
  (setq pipenv-with-flycheck nil))

(defun find-pipenv-venv-at (directory)
  "Find out a virtual environment that is associated with DIRECTORY.
If pipenv finds one, returns the path to the virtual environment.
If DIRECTORY is nil or it cannot, return nil."
  (when directory
    (let* (
         ;; with the following, we avoid using the current environment variables,
         ;; since pipenv uses VIRTUAL_ENV env. variable if it exists and pyvenv sets it
         (process-environment initial-environment)
         (candidate
          (replace-regexp-in-string "\n\\'" ""
                                    (shell-command-to-string
                                     (concat
                                      "cd " directory
                                      " && pipenv --venv 2>/dev/null")))))
      (unless (string-empty-p candidate) candidate))))

(defun find-pipenv-venv-for (buffer-or-name)
  "Find out a virtual environment that works with BUFFER-OR-NAME created by pipenv."
  (interactive "bThe buffer to find out a virtualenv by pipenv for: ")
  (let* ((default-venv (concat (file-name-as-directory (getenv "HOME")) "venv"))
         (buffer (get-buffer buffer-or-name))
         (buf-file-name (buffer-file-name buffer))
         (pipenv-file-dir
          (let ((file-dir (when buf-file-name (file-name-directory buf-file-name))))
            (find-pipenv-venv-at file-dir)))
         (pipenv-projectile-root (when (projectile-project-p)
                                   (find-pipenv-venv-at (projectile-project-root))))
         (target-venv (or pipenv-file-dir pipenv-projectile-root default-venv)))
    target-venv))

(defun advice-pipenv-venv (fun &rest r)
  "Set enviroment variables so as to use a virtual environment associated with the current buffer,
then call FUN with R. This function is supposed to be passed to advice-add with :around argument."
  (let* ((target-venv-dir (find-pipenv-venv-for (current-buffer)))
         (venv-bin-dir (concat (file-name-as-directory target-venv-dir) "bin"))
         (bufname (concat "Python@" target-venv-dir))
         (old-path (getenv "PATH"))
         (old-virtualenv (getenv "VIRTUAL_ENV"))
         (old-pythonhome (getenv "PYTHONHOME"))
         (exec-path (append `(,venv-bin-dir) exec-path)))
    ;; in python-mode, python-shell-buffer-name is used to determine the buffer name of the python inferior process.
    ;; Since function find-pipenv-venv-for (usually) returns the same venv if two files/buffers come from the same project,
    ;; files from the same project share the same python process/environment
    ;; in org-babel, we need to set the session name to python-shell-buffer-name
    ;; e.g., we need to have '#+PROPERTY: header-args:python :session (concat "Python@" (find-pipenv-venv-for (current-buffer)))'
    (unwind-protect
      (progn
        (setq-local python-shell-buffer-name bufname)
        (message "using the virtual environment at %s" target-venv-dir)

        (setenv "VIRTUAL_ENV" target-venv-dir)
        (setenv "PATH" (mapconcat 'identity `(,venv-bin-dir ,old-path) ":"))
        (setenv "PYTHONHOME" nil)

        (apply fun r))
      (progn
        (setenv "PATH" old-path)
        (setenv "VIRTUAL_ENV" old-virtualenv)
        (setenv "PYTHONHOME" old-pythonhome)))))

(advice-add #'run-python :around #'advice-pipenv-venv)

(provide 'conjure-python)
;;; conjure-python.el ends here
