# ACGtk: an ACG development toolkit

# INSTALL

**ACGtk** is a software package ([2008-2017 INRIA](http://www.inria.fr)©) for the development of abstract categorial grammars. This distribution provides two executables (possibly with the `.opt` extension, see the (INSTALL)[INSTALL] file: `acgc` and `acg` (or, instead, their native counterparts: `acgc.opt` and `acg.opt`).

It is distributed with the *CeCILL* license (see the [LICENSE](LICENSE.en) file or http://www.cecill.info). Contributors are listed in the (AUTHORS)[AUTHORS] file.

A list of related publications is available at the [ACG web page](http://calligramme.loria.fr/acg).

## Prerequisites

In order to compile the ACG toolkit, you need:
* `ocaml` (>=3.07) installed (http://ocaml.org/)
* `dypgen` (>=20080925) installed (http://dypgen.free.fr/)
* `bolt` (>=1.4) installed (http://bolt.x9c.fr/downloads.html)
* `ANSITerminal` (>=0.6.5) installed (https://github.com/Chris00/ANSITerminal)
* `cairo2` (>=0.4.6) installed (https://github.com/Chris00/ocaml-cairo)
* `easy-format` installed (https://github.com/mjambon/easy-format). Required by biniou
* `biniou` (>=1.0.6) installed (https://github.com/mjambon/biniou). Required by yojson
* `yojson` (>=1.1.8) installed (https://github.com/mjambon/yojson). Required by ocf
* `ocf` (>=0.4.0) installed (http://zoggy.github.io/ocf/)

For best results (correct rendering of symbols in the graphical output), please also install the free DejaVu fonts (http://dejavu-fonts.org).


## Installation with OPAM (preferred)

A fast and easy way to install `dypgen` and all important ocaml libraries is to use [opam](http://opam.ocaml.org/).

The installation typically goes that way:
1. first install [OCaml](http://ocaml.org/) using your prefered distribution/packaging mode
2. then install opam
3. don't forget to [add the required library in your path](http://opam.ocaml.org/doc/Usage.html#opam-switch) running ``eval `opam config env` ``
4. then continue with the ACG toolkit installation running `opam install acgtk`


This will install in the `.opam/OCAML_VERSION/bin` directory (where `OCAML_VERSION` is the OCaml version that you are using with opam. It can be `system` in case you are not using a compiler installed by opam, or a version number (such as 4.05.0) otherwise) the binaries `acgc`, `acgc.opt`, `acg`, and `acg.opt`.


It will also install in the `.opam/OCAML_VERSION/share/acgtk` directory:
1. the [emacs](emacs) directory 
2. the [examples](examples) directory


## ACG emacs mode

There is an ACG emacs mode `acg.el` in the [emacs](emacs) directory.

### Quick way to have it work

Copy the following lines in your .emacs

```emacs
(setq load-path (cons "EMACS_DIR_PATH" load-path))
(setq auto-mode-alist (cons '("\\.acg" . acg-mode) auto-mode-alist))
(autoload 'acg-mode "acg" "Major mode for editing ACG definitions" t)
```
where `EMACS_DIR_PATH` is `.opam/OCAML_VERSION/share/acgtk/emacs`

### Site distribution
   
1. Copy `acg.el` under an `acg` directory in your site-lisp directory (typically `/usr/share/emacs/site-lisp/` or `/usr/local/share/emacs/site-lisp/`)

2. Create a `50acg.el` file into the `/etc/emacs/site-start.d` directory and copy the following lines in it:
	```emacs
   (setq load-path (cons "EMACS_DIR_PATH" load-path))
   (setq auto-mode-alist (cons '("\\.acg" . acg-mode) auto-mode-alist))
   (autoload 'acg-mode "acg" "Major mode for editing ACG definitions" t)
   ```
   where `EMACS_DIR_PATH` is the acg directory in your site-lisp directory (typically `/usr/share/emacs/site-lisp/acg`)

## Examples

Example files are given in the [examples](examples) directory. Read the [examples/README.md](examples/README.md) file.


## For developpers

You can get the documented API in all `./src` subdirectories getting there and running:
```bash
make doc
```
It will create a doc subdirectory with html files.
