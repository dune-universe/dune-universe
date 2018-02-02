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

1. Clone the project or download the latest release and go into the cloned directory. 
1. To see whether you need to install any third-party package, type `make check-config`.
1. Once you have all needed packages, type `make native` to build "electrod".
