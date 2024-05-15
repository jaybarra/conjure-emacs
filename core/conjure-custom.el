;;; conjure-custom.el --- Custom Configurations for Custom in Conjure
;;; Commentary:
;;; Code:

(defgroup conjure nil
  "Conjure Emacs settings."
  :prefix "conjure-"
  :group 'convenience)

;; ===========================================================================
;; Appearance
;; ===========================================================================
(defcustom conjure-fonts '("MonoLisa" "Fira Code" "Cascadia Code")
  "Preferred Fonts in order of preference to use."
  :type 'list
  :group 'conjure)

(defcustom conjure-font-size 120
  "Controls default-font-size."
  :type 'number
  :group 'conjure)

(defcustom conjure-ligatures t
  "Controls whether or not to use ligatures in fonts."
  :type 'boolean
  :group 'conjure)

(defcustom conjure-theme 'ef-dream
  "The default color theme."
  :type 'symbol
  :group 'conjure)

;; ===========================================================================
;; Behavior
;; ===========================================================================

(defcustom conjure-whitespace nil
  "Non-nil values enable whitespace visualization."
  :type 'boolean
  :group 'conjure)

(provide 'conjure-custom)
;;; conjure-custom.el ends here
