;;; conjure-ui.el --- Configurations for the UI in Conjure -*- lexical-binding: t -*-
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

(setq ring-bell-function 'ignore)

(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defvar conjure-default-fonts
  '("MonoLisa" "Cascadia Code" "Fira Code"
    "Jetbrains Mono" "SF Mono" "Hack" "Source Code Pro"
    "Menlo" "Monaco" "DejaVu Sans Mono" "Consolas"
    "Hasklig" "Monoid")
  "Ordered list of default fonts to use.")

(defun conjure-setup-fonts ()
  "Setup fonts."
  (let ((fonts-list (delete-dups (append '() conjure-default-fonts))))
    (when (display-graphic-p)
      ;; Set default font
      (cl-loop for font in fonts-list
               when (font-installed-p font)
               return (set-face-attribute 'default nil
                                          :font font
                                          :height 130
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
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures 'prog-mode fira-code-cascadia-ligatures)

  (global-ligature-mode 't))

(elpaca ef-themes
  ;;(ef-themes-select 'ef-owl)
  )
(elpaca modus-themes
  (modus-themes-select 'modus-vivendi-tinted)
  )
(elpaca zenburn-theme)
(elpaca catppuccin-theme
  ;; (load-theme 'catppuccin :no-confirm)
  ;;(setq catppuccin-flavor 'latte)     ;; - light
  ;;(setq catppuccin-flavor 'frappe)    ;; - dark(er)
  ;;(setq catppuccin-flavor 'macchiato) ;; - dark(erer)
  ;;(setq catppuccin-flavor 'mocha)     ;; - darkest - default
  ;;(catppuccin-reload))
  )

(provide 'conjure-ui)
;;; conjure-ui.el ends here
