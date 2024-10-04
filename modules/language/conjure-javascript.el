;;; conjure-javascript.el --- Configurations for Javascript in Conjure -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq js-chain-indent t
      ;; These have become standard in the JS community
      js2-basic-offset 2
      js-indent-level 2
      ;; Don't mishighlight shebang lines
      js2-skip-preprocessor-directives t
      ;; let flycheck handle this
      js2-mode-show-parse-errors nil
      js2-mode-show-strict-warnings nil
      ;; Flycheck provides these features, so disable them: conflicting with
      ;; the eslint settings.
      js2-strict-missing-semi-warning nil
      ;; maximum fontification
      js2-highlight-level 3
      js2-idle-timer-delay 0.15)


(add-to-list 'auto-mode-alist '("\\.ts$"  . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.cts$" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.mts$" . typescript-ts-mode))

(add-to-list 'auto-mode-alist '("\\.tsx$"  . tsx-ts-mode))

(provide 'conjure-javascript)
;;; conjure-javascript.el ends here
