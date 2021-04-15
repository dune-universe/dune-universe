PREREQUISITES
=============

You need OCaml 4.11 or higher to compile the sources,
including the following packages, available under opam:

        "dune" {>= "2.5"}
        "graphics" {>= "5.1.1"}
        "camlimages" {>= "5.0.4"}

This version is available as an opam package. 

You also need the `kpsewhich' utility of the `kpathsea' library
provided by many TeX distributions.

For bitmap based image inclusion using \includegraphics of 
the graphics macro package, you require the camlimages library,
with version 5.0.4, available under opam. 

To display postscript based drawings by PsTricks macro package,
you need ghostscript (http://www.ghostscript.com). You need
the version 7.05 or later for the correct synchronization of
drawing of the TeX glyphs and postscript graphics.

COMPILATION AND INSTALLATION UNDER OPAM
========================================

If you have opam installed, you may just say: 

        opam install advi

It will download and install the package.

The default installation, does not install the latex style files.
See the section *INSTALLATION OF LATEX SOURCE FILES* below. 


COMPILATION AND INSTALLATION USING MAKE
========================================

Use the standard procedure:

        make
        make install
    
The default installation, does not install the latex style files,
See the section *INSTALLATION OF LATEX SOURCE FILES* below. 


INSTALLATION OF LATEX SOURCE FILES
===================================

The default installation, does not install the latex style files, which are
needed to take advantage of the advance features of `advi`, including its
use as a backend for `whizzytex`.

To install them, you may use the command:

        advi-latex-files --install

which will tell you what to do.  You actually have a choice.

## No installation

Just leave the files where they are in the opam repository, i.e.
at this location:

       advi-latex-files --path

   and add this path (the output of the above command) in you 
   `TEXINPUTS` environment variable.
   
## Installation at the default location

Install the files at the standard location of the latex distribution.
Then try: 
   
        advi-latex-files --install default
        
This will attempt to guess the correct `<DEST>` folder, tell you
what it will do, and ask for confirmation.
    
It will probably tell you to run the command as root.

In case the command fails, then you must guess yourself the right location
as described next and follow this by running the command

        mktexlsr
   
## Installation at some other location

You may install those file at some location `<DEST>`
of your choice passed explicitly: 

        advi-latex-files --install <DEST>
        
You should then ensure that `<DEST>` is included in the 
latex search path `TEXINPUTS`. You may need to run the
command `mktexlsr` afterwards

Notice that you may also copy the files listed by

        advi-latex-files --list
        
and located at `advi-latex-files -path` by hand. 

## Checking your installation

The command

        kpsewhich advi.sty
        
will tell you whether (and where) it found the `advi.sty` style file.

JAPANESE USERS
==============

There is a configuration file for mapping Japanese TeX font names
to Japanese True Type fonts called "jpfonts.conf". It is placed in
the advi library directory (under opam prefix). 
If you want to use different TrueType fonts, you can edit this file,
or put your own jpfonts.conf in ~/.advi/ .

DOCUMENTATION
=============

If you wish to rebuild the documentation, you will need the following latex
latex packages:

        eepic
        pstricks
        graphics
        babel
        graphicx
        hyperref
        makeidx
        manual
        tabularx

as well as some latex tools: `latexmk`, `pandoc`, and `hevea`
(http://hevea.inria.fr/) also available under opam.
