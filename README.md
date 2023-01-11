# Emacs Conjure

Conjure is an Emacs distribution that takes inspiration from a number of excellent
Emacs configurations. 


+ [Prelude](https://raw.githubusercontent.com/bbatsov/prelude)
+ [Emacs From Scratch](https://github.com/daviwil/emacs-from-scratch)

## Installation

To install, clone this repo to `~/.emacs.d`

	cd ~
	git clone https://github.com/jaybarra/.emacs.d

Once downloaded, run Emacs. On first run it will attempt to install packages.

Some packages need additional interactive steps to configure. Specifically
any that require fonts and icons.

Run the following and restart Emacs.
	
    <M-x> all-the-icons-install-fonts
    
### Prerequisites

[conjure-ui](./core/conjure-ui.el) uses [Source Code Pro](https://fonts.google.com/specimen/Source+Code+Pro) as the preferred font.

### Language Server Protocol (LSP)

Conjure uses eglot as its LSP interface which relies on external applications.
Feel free to switch out `eglot` for `lsp` by replacing `ensure-eglot` with `lsp-deferred` in throughout the configurations or for unhandled languages. You will also need to add `lsp` to the list of [conjure-package](./core/conjure-packages.el)

It is suggested to install the following LSPs on your system:

* [Typescript Language Server](https://github.com/typescript-language-server/typescript-language-server) for JavaScript and TypeScript
  * `npm install --location=global typescript-language-server`
* [YAML Language Server](https://github.com/redhat-developer/yaml-language-server) for YAML
  * `brew install yaml-language-server`
* [JDTLS](https://github.com/eclipse/eclipse.jdt.ls) for Java (requires java 17+)
  * `brew install jdtls`
