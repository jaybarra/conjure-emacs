;;; conjure-gleam.el --- Configurations for Gleam in Conjure -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(elpaca (gleam-ts-mode
         :host github
         :repo "gleam-lang/gleam-mode"
         :files (:defaults "gleam-ts-mode.el")))

(provide 'conjure-gleam)
;;; conjure-gleam.el ends here
