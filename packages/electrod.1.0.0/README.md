# electrod - Formal analysis for the Electrod formal specification language

Electrod is a model finder inspired by Kodkod. It takes as input a
model expressed in a mixture of relational first-order logic (RFOL)
over bounded domains and linear temporal logic (LTL) over an unbounded
time horizon. Then it compiles the model to a problem for a solver (currently the NuSMV and nuXmv model-checkers). Electrod is mainly meant to be used as a backend for the [Electrum Analyzer](http://haslab.github.io/Electrum).

[Github homepage](https://github.com/grayswandyr/electrod)

[ONERA homepage](https://forge.onera.fr/projects/electrod)

## Installation instructions

Installation has only been tested on GNU/Linux and Mac OS X. It may not work on MS Windows.

The easiest way to install Electrod is to rely on the Opam OCaml package
manager. 
```
opam update
opam upgrade 
opam install electrod
```

After installation, you will see a program called "electrod" in your PATH. 


## External dependencies

As of now, Electrod relies on NuSMV (2.6+) or nuXmv (2.0+),
so you must at least install one of them. 


## Running

Electrod is primarily aimed at being called by external tools such as the Electrum Analyzer. 

However, it can also be run as a standalone tool by calling the `electrod` program.  
Type `electrod --help` to get some help on options.


## Copyright and license

(C) 2016-2020 ONERA

electrod is distributed under the terms of the Mozilla Public License v2.0.

See [LICENSE.md](LICENSE.md) for more information.
