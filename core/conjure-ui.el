;;; conjure-ui.el --- Conjure UI settings
;;; Commentary:
;;; Code:

(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

;; disable the menu-bar on non-Mac systems
(unless (eq system-type 'darwin)
  (menu-bar-mode -1))

;; disable cursor blink
(blink-cursor-mode -1)

(setq frame-title-format
      '(""
        (:eval
         (let* ((system-name (or (file-remote-p default-directory 'host) system-name))
                (project (project-current))
                (project-name (when project (project-name project)))
                (project-root (when project (project-root project)))
                (buffer-name (buffer-name))
                (buffer-file-name (buffer-file-name))
                (relative-path (when (and buffer-file-name project)
                                 (file-relative-name buffer-file-name project-root))))
           (if (and project-name project-root)
               (format "(%s) [%s] - %s" system-name project-name relative-path)
             (format "(%s) %s" system-name buffer-name))))))

;; disable the bell
(setq ring-bell-function 'ignore)

;; Themes
(use-package zenburn-theme :ensure t)
(use-package ef-themes
  :ensure t
  :config
  (when conjure-theme (ef-themes-select conjure-theme)))

(use-package delight :ensure t)

(use-package nerd-icons :ensure t)
(use-package nerd-icons-dired
  :ensure t
  :hook (dired-mode . nerd-icons-dired-mode))
(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package ace-window
  :ensure t
  :bind (("M-o" . 'ace-window)))

(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defvar conjure-default-fonts
  '("MonoLisa" "Cascadia Code" "Fira Code"
    "Jetbrains Mono" "SF Mono" "Hack" "Source Code Pro"
    "Menlo" "Monaco" "DejaVu Sans Mono" "Consolas"
    "Hasklig" "Monoid")
  "List of default fonts to use.")

(defun conjure-setup-fonts ()
  "Setup fonts."
  (let ((fonts-list (delete-dups (append conjure-fonts conjure-default-fonts))))
    (when (display-graphic-p)
      ;; Set default font
      (cl-loop for font in fonts-list
               when (font-installed-p font)
               return (set-face-attribute 'default nil
                                          :font font
                                          :height conjure-font-size
                                          :weight 'regular)))))

(conjure-setup-fonts)
(add-hook 'window-setup-hook #'conjure-setup-fonts)
(add-hook 'server-after-make-frame-hook #'conjure-setup-fonts)

(defvar monolisa-v2-ligatures
  '(;; coding ligatures
    "<!---" "--->" "|||>" "<!--" "<|||" "<==>" "-->" "->>" "-<<" "..=" "!=="
    "#_(" "/==" "||>" "||=" "|->" "===" "==>" "=>>" "=<<" "=/=" ">->" ">=>"
    ">>-" ">>=" "<--" "<->" "<-<" "<||" "<|>" "<=" "<==" "<=>" "<=<" "<<-"
    "<<=" "<~>" "<~~" "~~>" ">&-" "<&-" "&>>" "&>" "->" "-<" "-~" ".=" "!="
    "#_" "/=" "|=" "|>" "==" "=>" ">-" ">=" "<-" "<|" "<~" "~-" "~@" "~="
    "~>" "~~"

    ;; whitespace ligatures
    "---" "'''" "\"\"\"" "..." "..<" "{|" "[|" ".?" "::" ":::" "::=" ":="
    ":>" ":<" "\;\;" "!!" "!!." "!!!"  "?." "?:" "??" "?=" "**" "***" "*>"
    "*/" "--" "#:" "#!" "#?" "##" "###" "####" "#=" "/*" "/>" "//" "/**"
    "///" "$(" ">&" "<&" "&&" "|}" "|]" "$>" ".." "++" "+++" "+>" "=:="
    "=!=" ">:" ">>" ">>>" "<:" "<*" "<*>" "<$" "<$>" "<+" "<+>" "<>" "<<"
    "<<<" "</" "</>" "^=" "%%"))

(defvar fira-code-cascadia-ligatures
  '(;; == === ==== => =| =>>=>=|=>==>> ==< =/=//=// =~
    ;; =:= =!=
    ("=" (rx (+ (or ">" "<" "|" "/" "~" ":" "!" "="))))
    ;; ;; ;;;
    (";" (rx (+ ";")))
    ;; && &&&
    ("&" (rx (+ "&")))
    ;; !! !!! !. !: !!. != !== !~
    ("!" (rx (+ (or "=" "!" "\." ":" "~"))))
    ;; ?? ??? ?:  ?=  ?.
    ("?" (rx (or ":" "=" "\." (+ "?"))))
    ;; %% %%%
    ("%" (rx (+ "%")))
    ;; |> ||> |||> ||||> |] |} || ||| |-> ||-||
    ;; |->>-||-<<-| |- |== ||=||
    ;; |==>>==<<==<=>==//==/=!==:===>
    ("|" (rx (+ (or ">" "<" "|" "/" ":" "!" "}" "\]"
                    "-" "=" ))))
    ;; \\ \\\ \/
    ("\\" (rx (or "/" (+ "\\"))))
    ;; ++ +++ ++++ +>
    ("+" (rx (or ">" (+ "+"))))
    ;; :: ::: :::: :> :< := :// ::=
    (":" (rx (or ">" "<" "=" "//" ":=" (+ ":"))))
    ;; // /// //// /\ /* /> /===:===!=//===>>==>==/
    ("/" (rx (+ (or ">"  "<" "|" "/" "\\" "\*" ":" "!"
                    "="))))
    ;; .. ... .... .= .- .? ..= ..<
    ("\." (rx (or "=" "-" "\?" "\.=" "\.<" (+ "\."))))
    ;; -- --- ---- -~ -> ->> -| -|->-->>->--<<-|
    ("-" (rx (+ (or ">" "<" "|" "~" "-"))))
    ;; *> */ *)  ** *** ****
    ("*" (rx (or ">" "/" ")" (+ "*"))))
    ;; www wwww
    ("w" (rx (+ "w")))
    ;; <> <!-- <|> <: <~ <~> <~~ <+ <* <$ </  <+> <*>
    ;; <$> </> <|  <||  <||| <|||| <- <-| <-<<-|-> <->>
    ;; <<-> <= <=> <<==<<==>=|=>==/==//=!==:=>
    ;; << <<< <<<<
    ("<" (rx (+ (or "\+" "\*" "\$" "<" ">" ":" "~"  "!"
                    "-"  "/" "|" "="))))
    ;; >: >- >>- >--|-> >>-|-> >= >== >>== >=|=:=>>
    ;; >> >>> >>>>
    (">" (rx (+ (or ">" "<" "|" "/" ":" "=" "-"))))
    ;; #: #= #! #( #? #[ #{ #_ #_( ## ### #####
    ("#" (rx (or ":" "=" "!" "(" "\?" "\[" "{" "_(" "_"
                 (+ "#"))))
    ;; ~~ ~~~ ~=  ~-  ~@ ~> ~~>
    ("~" (rx (or ">" "=" "-" "@" "~>" (+ "~"))))
    ;; __ ___ ____ _|_ __|____|_
    ("_" (rx (+ (or "_" "|"))))
    ;; Fira code: 0xFF 0x12
    ("0" (rx (and "x" (+ (in "A-F" "a-f" "0-9")))))
    ;; Fira code:
    "Fl"  "Tl"  "fi"  "fj"  "fl"  "ft"
    ;; The few not covered by the regexps.
    "{|"  "[|"  "]#"  "(*"  "}#"  "$>"  "^="))

(use-package ligature
  :ensure t
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures 'prog-mode fira-code-cascadia-ligatures)

  (global-ligature-mode conjure-ligatures))

(provide 'conjure-ui)
;;; conjure-ui.el ends here
