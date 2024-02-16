# Emacs Conjure

_Conjure_ is an Emacs configuration that takes inspiration from a number of excellent Emacs repositories.

+ [Prelude](https://github.com/bbatsov/prelude)
+ [Spacemacs](https://www.spacemacs.org/)
+ [Emacs From Scratch](https://github.com/daviwil/emacs-from-scratch)

## Installation

[Download Emacs](https://www.gnu.org/software/emacs/download.html) from one of listed locations.

Make a backup of your existing `.emacs.d` folder if necessary, then clone this repo to `~/.emacs.d`.

``` shell
mv ~/.emacs.d ~/.emacs.d.bak
git clone https://github.com/jaybarra/.emacs.d ~/.emacs.d
```

Once downloaded, run Emacs. On first run it will attempt to install packages.

It is advisable to copy the `sample/conjure-modules.el` to the `personal` directory to begin modifying it to your own needs.

## Purpose

_Conjure_ was originally only my personal configuration. Interest on my teams had been expressed over Emacs and in my efforts to genericize my own configuration to make it appealing to others, the idea of publishing it generally occurred to me.

## License

Copyright © 2024 Jay Barra

Distributed under the GNU General Public License, version 3

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
