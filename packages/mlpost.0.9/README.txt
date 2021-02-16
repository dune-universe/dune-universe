**************************************************************************
*                                                                        *
*  Copyright (C) Johannes Kanig, Stephane Lescuyer                       *
*  Jean-Christophe Filliatre, Romain Bardou and Francois Bobot           *
*                                                                        *
*  This software is free software; you can redistribute it and/or        *
*  modify it under the terms of the GNU Library General Public           *
*  License version 2.1, with the special exception on linking            *
*  described in file LICENSE.                                            *
*                                                                        *
*  This software is distributed in the hope that it will be useful,      *
*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *
*                                                                        *
**************************************************************************

This is MLPost !

Usage:
------

* Open the Mlpost pack:

         open Mlpost

* Define your figures in an Ocaml file fig.ml

	 let fig_a = ...
	 let fig_b = ...

  Each figure has type Command.t.

* Add some code to emit Metapost code, as follows


	let () = Metapost.emit "file_a" fig_a
	let () = Metapost.emit "file_b" fig_b

* Then run the mlpost program on this file

       mlpost fig.ml

  It will create PostScript figures in files file_a.1, file_b.1, etc.


Options:
--------

mlpost supports the following options:

-pdf
	creates .mps files instead of .1, for inclusion in LaTeX files
	compiled with pdflatex (the PostScript file is actually the
	same, but the suffix is used by pdflatex to identify
	PostScript produced by Metapost)

-latex main.tex
        indicates the main LaTeX file, from which the prelude is
        extracted to be passed to Metapost (this way you can use
        macros, fonts and packages from your LaTeX document in your
        figures).

-xpdf
        opens an xpdf viewer with the generated figure. Subsequent calls with
        the option -xpdf will refresh the viewer, if it is still open.

-native
        compile to native code. This is usually faster.

-eps
        produce standalone postscript files

-ocamlbuild
        use ocamlbuild to compile the source; this may be useful it there are
        a lot of dependencies

-ccopt <options>
        pass options to the ocaml compiler

-execopt <options>
        pass options to the compiled program


Cairo output:
-------------

The following functions are not supported in combination with the Concrete /
Cairo modules:

* Path.build_cycle
* Pen.square
* Pen.from_path
