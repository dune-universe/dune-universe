# Installing the Electrod compiler

Installation has been tested on GNU/Linux and Mac OS X. It most probably does
not work on MS Windows.

## Installation through OPAM (recommended)

The easiest way to install Electrod is to rely on the OPAM OCaml package
manager.

You need at least OCaml version 4.04.2.

Just type `opam install electrod` in a shell.

After installation, you will see a program called "electrod" in your PATH.

## Manual installation 

1. Clone the project or download the latest release and go into the cloned directory. Notice that *cloning* the latest release is the preferred way to go as it will allow the build manager to watermark the program with a version number (which could prove useful, should you wish to contact us about an issue specific to this version)
1. To see whether you need to install any third-party package, look at the section "depends" in the `electrod.opam` file.
1. Once you have all needed packages, type the following command to build Electrod:
```
$ jbuilder subst -p electrod && jbuilder build -p electrod
```
1. The resulting program can be found in `_build/default/src/electrod.exe`. Rename and install it in your PATH.
