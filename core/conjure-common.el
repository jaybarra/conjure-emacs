;;; conjure-common.el --- Conjure functionality and functions
;;; Commentary:
;;; Code:

(defun display-ansi-colors ()
  "Colorize buffer containining ANSI color strings."
  (interactive "r")
  (ansi-color-apply-on-region (point-min) (point-max)))

(defun conjure-pretty-print-xml-region (begin end)
  "Pretty-print XML in region betwen BEGIN and END in a sensible manner."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (backward-char) (insert "\n"))
    (indent-region begin end)))

(provide 'conjure-common)
;;; conjure-common.el ends here
