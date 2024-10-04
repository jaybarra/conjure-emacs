;;; conjure-sql.el --- Configurations for SQL in Conjure -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package sqlformat
  :bind (:map sql-mode-map
              ("C-c C-f" . sqlformat-buffer))
  :config
  (let* ((sqlformat-bin (executable-find "sqlformat"))
         (pgformatter-bin (executable-find "pgformatter"))
         (sqlfluff-bin (executable-find "sqlfluff"))
         (sql-formatter-bin (executable-find "sql-formatter"))
         (sqlfmt-bin (executable-find "sqlfmt"))
         (first-available (cond (sqlformat-bin 'sqlformat)
                                (pgformatter-bin 'pgformat)
                                (sqlfluff-bin 'sqlfluff)
                                (sql-formatter-bin 'sql-formatter)
                                (sqlfmt-bin 'sqlfmt))))
    (when first-available
      (setq sqlformat-command first-available)))
  (add-hook 'sql-mode-hook 'sqlformat-on-save-mode))

(provide 'conjure-sql)
;;; conjure-sql.el ends here
