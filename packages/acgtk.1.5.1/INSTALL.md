# ACGtk: an ACG development toolkit

# INSTALL

**ACGtk** is a software package ([2008-2021 INRIA](http://www.inria.fr)©) for the development of abstract categorial grammars. This distribution provides two executables file: `acgc` and `acg`.

It is distributed with the *CeCILL* license (see the [LICENSE](LICENSE.en) file or http://www.cecill.info). Contributors are listed in the (AUTHORS)[AUTHORS] file.

A list of related publications is available at the [ACG web page](http://calligramme.loria.fr/acg).

## Prerequisites

In order to compile the ACG toolkit, you need:
* `ocaml` (>=3.07) installed (http://ocaml.org/)
* `dune` installed (https://github.com/ocaml/dune)
* `menhir` installed (http://gallium.inria.fr/~fpottier/menhir/)
* `ANSITerminal` (>=0.6.5) installed (https://github.com/Chris00/ANSITerminal)
* `fmt` installed (http://erratique.ch/software/fmt)
* `cmdliner` installed (http://erratique.ch/software/https://erratique.ch/software/cmdliner)
* `logs` installed (http://erratique.ch/software/logs)
* `cairo2` (>=0.4.6) installed (https://github.com/Chris00/ocaml-cairo)
* `easy-format` installed (https://github.com/mjambon/easy-format). Required by biniou
* `biniou` (>=1.0.6) installed (https://github.com/mjambon/biniou). Required by yojson
* `yojson` (>=1.1.8) installed (https://github.com/mjambon/yojson).

For best results (correct rendering of symbols in the graphical output), please also install the free DejaVu fonts (http://dejavu-fonts.org).

**Note:** These packages contain Ocaml libraries or binaries. They may depend on external non-Ocaml libraries, in particular `cairo` and `freetype`. The way to install them depend on your OS. Unfortunately, their name also depend on your OS (for instance, what is required for `cairo` on Debian is `libcairo2-dev`, while it's `cairo` on Mac OS).

To know the required packages on your OS in order to install ACGtk, you can run:
```
opam list --rec --required-by acgtk --external
```
and look at the output corresponding to your system (on OSX, it seems that the package name is the same whether you use `homebrew` or `port` to install your packages).

Or you can use the `depexts` OPAM package to install them automatically:
```
opam install depext
opam depext acgtk
```

## Installation with OPAM (preferred)

A fast and easy way to install all important ocaml libraries is to use [opam](http://opam.ocaml.org/).

The installation typically goes that way:
1. first install [OCaml](http://ocaml.org/) using your prefered distribution/packaging mode
2. then install opam
3. don't forget to [add the required library in your path](http://opam.ocaml.org/doc/Usage.html#opam-switch) running ``eval `opam config env` ``
4. then continue with the ACG toolkit installation running `opam install acgtk`


This will install in the `.opam/OCAML_VERSION/bin` directory (where `OCAML_VERSION` is the OCaml version that you are using with opam. It can be `system` in case you are not using a compiler installed by opam, or a version number (such as 4.05.0) otherwise) the binaries `acgc` and `acg`.


It will also install in the `.opam/OCAML_VERSION/share/acgtk` directory:
1. the [emacs](emacs) directory
2. the [examples](examples) directory

(To get the actual path of the `share` directory, just run `opam var share`.)

### Building ACGtk (Skip this part if you installed acgtk as described above)

Alternatively, if you want to compile acgtk by yourself
1. first install the required libraries with the command
```opam install dune menhir ANSITerminal fmt logs cairo2 easy-format bibiou yojson ocf```
2. Download the ACGtk sources from http://acg.loria.fr/

To build the ACG toolkit, first run:
```dune build```
then
```dune install```


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
