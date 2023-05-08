;;; conjure-packages.el --- Conjure Emacs Core Packages and Package Management
;;; Commentary:
;;; Code:

(require 'cl-lib)
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(defvar conjure-packages
  '(ace-window
    affe
    apheleia
    anzu
    avy
    browse-kill-ring
    cape
    darkroom
    diff-hl
    diminish
    discover-my-major
    easy-kill
    editorconfig
    ef-themes
    embark
    exec-path-from-shell
    expand-region
    gist
    git-timemachine
    guru-mode
    helpful
    hl-todo
    ligature
    lin
    magit
    nlinum
    nlinum-relative
    operate-on-number
    projectile
    pulsar
    rainbow-delimiters
    rg
    smartparens
    smartrep
    super-save
    uuidgen
    undo-tree
    volatile-highlights
    which-key
    xterm-color
    zop-to-char))

(defun conjure-packages-installed-p ()
  "Check if packages are installed."
  (cl-every #'package-installed-p conjure-packages))

(defun conjure-require-package (package)
  "Install PACKAGE."
  (unless (memq package conjure-packages)
    (add-to-list 'conjure-packages package))
  (unless (package-installed-p package)
    (package-install package))
  (require package))

(defun conjure-require-packages (packages)
  "Install PACKAGES."
  (mapc #'conjure-require-package packages))

(defun conjure-install-packages ()
  "Install all core packages."
  (unless (conjure-packages-installed-p)
    (message "%s" "Conjure is updating the package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    (conjure-require-packages conjure-packages)))

(conjure-install-packages)

(defmacro conjure-auto-install (extension package mode)
  "When file with EXTENSION is opened triggers auto-install of PACKAGE.
PACKAGE is installed only if not already present.  The file is opened in MODE."
  `(add-to-list 'auto-mode-alist
                `(,extension . (lambda ()
                                 (unless (package-installed-p ',package)
                                   (package-install ',package))
                                 (,mode)))))

(defvar conjure-auto-install-alist
  '(("\\.adoc\\'" adoc-mode adoc-mode)
    ("\\.clj\\'" clojure-mode clojure-mode)
    ("\\.cljc\\'" clojure-mode clojurec-mode)
    ("\\.cljs\\'" clojure-mode clojurescript-mode)
    ("\\.edn\\'" clojure-mode clojure-mode)
    ("\\.cmake\\'" cmake-mode cmake-mode)
    ("CMakeLists\\.txt\\'" cmake-mode cmake-mode)
    ("\\.coffee\\'" coffee-mode coffee-mode)
    ("\\.css\\'" css-mode css-mode)
    ("\\.csv\\'" csv-mode csv-mode)
    ("Cask" cask-mode cask-mode)
    ("\\.d\\'" d-mode d-mode)
    ("\\.dart\\'" dart-mode dart-mode)
    ("\\.elm\\'" elm-mode elm-mode)
    ("\\.ex\\'" elixir-mode elixir-mode)
    ("\\.exs\\'" elixir-mode elixir-mode)
    ("\\.elixir\\'" elixir-mode elixir-mode)
    ("\\.erl\\'" erlang erlang-mode)
    ("\\.feature\\'" feature-mode feature-mode)
    ("\\.geojson\\'" json-mode json-mode)
    ("\\.go\\'" go-mode go-mode)
    ("\\.graphql\\'" graphql-mode graphql-mode)
    ("\\.groovy\\'" groovy-mode groovy-mode)
    ("\\.haml\\'" haml-mode haml-mode)
    ("\\.hs\\'" haskell-mode haskell-mode)
    ("\\.jl\\'" julia-mode julia-mode)
    ("\\.json\\'" json-mode json-mode)
    ("\\.kml\\'" nxml-mode nxml-mode)
    ("\\.kt\\'" kotlin-mode kotlin-mode)
    ("\\.kv\\'" kivy-mode kivy-mode)
    ("\\.latex\\'" auctex LaTeX-mode)
    ("\\.less\\'" less-css-mode less-css-mode)
    ("\\.log\\'" logview log-view-mode)
    ("\\.lua\\'" lua-mode lua-mode)
    ("\\.md\\'" markdown-mode markdown-mode)
    ("\\.ml\\'" tuareg tuareg-mode)
    ("\\.pp\\'" puppet-mode puppet-mode)
    ("\\.php\\'" php-mode php-mode)
    ("\\.proto\\'" protobuf-mode protobuf-mode)
    ("\\.pug\\'" pug-mode pug-mode)
    ("\\.pyd\\'" cython-mode cython-mode)
    ("\\.pyi\\'" cython-mode cython-mode)
    ("\\.pyx\\'" cython-mode cython-mode)
    ("PKGBUILD\\'" pkgbuild-mode pkgbuild-mode)
    ("\\.rkt\\'" racket-mode racket-mode)
    ("\\.rs\\'" rust-mode rust-mode)
    ("\\.sass\\'" sass-mode sass-mode)
    ("\\.scala\\'" scala-mode scala-mode)
    ("\\.scss\\'" scss-mode scss-mode)
    ("\\.slim\\'" slim-mode slim-mode)
    ("\\.styl\\'" stylus-mode stylus-mode)
    ("\\.svelte\\'" svelte-mode svelte-mode)
    ("\\.swift\\'" swift-mode swift-mode)
    ("\\.textile\\'" textile-mode textile-mode)
    ("\\.tf\\'" terraform-mode terraform-mode)
    ("\\.thrift\\'" thrift thrift-mode)
    ("\\.vue\\'" vue-mode vue-mode)
    ("\\.yml\\'" yaml-mode yaml-mode)
    ("\\.yaml\\'" yaml-mode yaml-mode)
    ("\\.zig\\'" zig-mode zig-mode)
    ("Dockerfile\\'" dockerfile-mode dockerfile-mode)))

;; nor adoc
(when (package-installed-p 'adoc-mode)
  (add-to-list 'auto-mode-alist '("\\.adoc\\'" . adoc-mode))
  (add-to-list 'auto-mode-alist '("\\.asciidoc\\'" . adoc-mode)))

;; nor pkgbuild
(when (package-installed-p 'pkgbuild-mode)
  (add-to-list 'auto-mode-alist '("PKGBUILD\\'" . pkgbuild-mode)))

;; now register
(mapc (lambda (entry)
        (let ((extension (car entry))
              (package (cadr entry))
              (mode (cadr (cdr entry))))
          (unless (package-installed-p package)
            (conjure-auto-install extension package mode))))
      conjure-auto-install-alist)

(provide 'conjure-packages)
;;; conjure-packages.el ends here
