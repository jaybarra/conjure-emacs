;;; conjure-custom.el --- Conjure configuration and toggles
;;; Commentary:
;;; Code:

(defgroup conjure nil
  "Emacs Conjure configuration."
  :prefix "conjure-"
  :group 'convenience)

(defcustom conjure-super-keybindings t
  "Non-nil values enable super-key."
  :type 'boolean
  :group 'conjure)

(defcustom conjure-guru t
  "Non-nil values enable `guru-mode'."
  :type 'boolean
  :group 'conjure)

(defcustom conjure-indent-sensitive-modes
  '(conf-mode coffee-mode haml-mode python-mode slim-mode yaml-mode)
  "Modes to ignore auto-indenting."
  :type 'list
  :group 'conjure)

(defcustom conjure-yank-indent-modes '(LaTeX-mode TeX-mode)
  "Modes in which to indent regions that are yanked.
Only modes that don't derive from `prog-mode' should be in this list."
  :type 'list
  :group 'conjure)

(defcustom conjure-yank-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur."
  :type 'number
  :group 'conjure)

(defcustom conjure-auto-save t
  "Non-nil values enable Conjure's auto-save."
  :type 'boolean
  :group 'conjure)

(defcustom conjure-flyspell t
  "Non-nil values enable aspell."
  :type 'boolean
  :group 'conjure)

(defcustom conjure-whitespace t
  "Non-nil values enable whitespace visualization."
  :type 'boolean
  :group 'conjure)

(defcustom conjure-cleanup-whitespace-on-save t
  "Non-nil values enable whitespace cleanup in a buffer on save.
Requires `conjure-whitespace' to also be enabled."
  :type 'boolean
  :group 'conjure)

(defcustom conjure-format-on-save t
  "Non-nil will attempt to format buffers on save, when supported by the mode."
  :type 'boolean
  :group 'conjure)

(defcustom conjure-use-ligatures t
  "Non-nil will enable font-ligatures if supported by the font."
  :type 'boolean
  :group 'conjure)

(provide 'conjure-custom)

;;; conjure-custom.el ends here
