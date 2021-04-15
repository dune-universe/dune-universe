# Version 2.0.0

- latex-source files not installed by default; requires runninig a post-installation
  command 
- The documentation is not built and installed by default, to avoid dependencies
- Available as an opam package
- Compilation via dune 
- Moved from CVS to git


# Version 1

## Version 1.11

- Fixed postsyncing flag, key, and documentation.

## Version 1.10.2

- Fixed packaging of documentation

## Version 1.10.1
- Prefetching fonts to group calls to kpsewhich (starts faster)
- Made scroll buttons preemptive (stop displaying)
- Fixed install path of dlladvi
- Fixed compatibility of event loop with new version of graphics

## Version 1.10
 - Updated the doc
 - Bytecode version uses a library dlladvi.so
 - Changed configuration to use 4.0 of camlimages
 - Dynamics libraries loaded last (provided by Stefan Potyra)

## Version 1.9
 - EPS files are read by advi and sent to gs
 - splash.dvi is now distributed and correctly installed
 - examples are distributed but not compiled by default
 - added conf/jpfonts.conf in the disbribution
 - gs path is now set by configure
 - Added support for gzip and bzip2 compressed files. 
 - Fixed advi.man  (provided mostly by Debian managers).
 - Included security patches (provided by Debian managers).

## Version 1.8
 - Rendering EPS files with gs by default.
 - PGF pactches for pstrick support of PDF macros in advi.sty
 - Bug fixes. 

## Version 1.7
 - 2005/09/02: 
   Having Active-DVI compatible with the new Graphics library.
 - 2004/12/07: 
   Getting rid of the new warnings of the Caml compiler.
   
## Version 1.6
 - 2004/03/04:
   + Man page rewritten and completed.
   + Introducing a laser pointer facility.

## Version 1.5+2

 - 2003/10/09: Background enhancement with specification of the geometry and
   colors.

- 2003/10/09: Added an HTML version of the manual with the help of HeVeA and
   Luc Maranget.

- 2003/10/08: Change the background command execution order:

    1. Solid background first,
    2. Then apply the gradient function,
    3. Then draw the image.

    This way you can use an (alpha blended) image on a gradient background.

- 2003/10/03: Long time standing bug of set_title has been fixed: the
  Active-DVI window is now allocated by the Window Manager with the expected
  correct name.

## Version 1.5

- 2003/09/30: Introducing page timing dump to designated files for
  synchronisation purposes.

## Version 1.4

- 2003/09/12: Extensively modified gr_reposition in grY11.c to correctly
  handle full screen operation.  This was done by incorporating most of the
  logic found in the excellent mplayer project.

  Seems to work on most Window Managers.
  - [X]  KDE 3.1: OK
  - [X]  Sawfish: OK
  - [X]  Fluxbox: OK
  - [ ]  MWM: NO (decorations are not removed)
  - [ ]  Fvwm95: Partial (control bar is still on front) [can be removed by
 	     WithDrawing and replacing the window, but this causes problems with
  - [ ] fvwm (no 95)]
  - [ ] Fvwm: Partial (virtual screen window still on front)
  - [X] Twm: OK
	
