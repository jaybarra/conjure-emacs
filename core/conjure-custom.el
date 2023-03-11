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

(defcustom conjure-whitespace nil
  "Non-nil values enable whitespace visualization."
  :type 'boolean
  :group 'conjure)

(defcustom conjure-clean-whitespace-on-save t
  "Non-nil values will clean whitespace from file before it's saved."
  :type 'boolean
  :group 'conjure)

(defcustom conjure-format-on-save t
  "Non-nil will attempt to format buffers on save, when supported.
May require external formatters be present such as:
- `black'
- `prettier'
- `ls-format'"
  :type 'boolean
  :group 'conjure)

(defcustom conjure-use-ligatures t
  "Non-nil will enable font-ligatures if supported by the font."
  :type 'boolean
  :group 'conjure)

(provide 'conjure-custom)
;;; conjure-custom.el ends here
