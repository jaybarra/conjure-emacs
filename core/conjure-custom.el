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
  "Non-nil values enable 'guru-mode'."
  :type 'boolean
  :group 'conjure)

(defcustom conjure-whitespace nil
  "Non-nil values enable whitespace visualization."
  :type 'boolean
  :group 'conjure)

(defcustom conjure-clean-whitespace-on-save t
  "Cleanup whitespace from file before it's saved.
Will only occur if `conjure-whitespace' is also enabled."
  :type 'boolean
  :group 'conjure)

(defcustom conjure-format-on-save t
  "When set and supported in the current mode, the file will be formatted on save."
  :type 'boolean
  :group 'conjure)

(defcustom conjure-use-ligatures t
  "Non-nil will enable font-ligatures if supported by the font."
  :type 'boolean
  :group 'conjure)

(provide 'conjure-custom)
;;; conjure-custom.el ends here
