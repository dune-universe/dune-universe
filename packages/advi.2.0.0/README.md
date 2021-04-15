
WARNING
=======

        advi -safer file.dvi

Active-DVI _plays_ the DVI files that is displays: during presentation
advi can launch arbitrary commands that were incorporated into the LaTeX
source of the DVI file to animate the presentation.  Hence, when playing a
DVI file from some untrusted source, you must use the `-safer` option that
prevents the execution of embedded commands:

For instance, if you choose advi as your default application to view
DVI files, the safe entry for your .mailcap file is
 
        application/x-dvi; advi -safer %s

It is also recommended to check before visualisation the list of
embedded commands that a suspicious DVI file could execute by calling

        advi -n file.dvi


Presentation
============

Active-DVI is a DVI previewer and presenter written in Objective Caml
(http://caml.inria.fr/ocaml/) with some eye candy effects for
presentation, support for interactive demonstrations, and embedding of
arbitrary applications within the presentation (hence the Active
adjective of the presenter :).

Active-DVI is largely based on Mldvi written by Alexandre Miquel
(http://www.pps.jussieu.fr/~miquel/).

Version: 2.0.0
available at http://advi.inria.fr/

CURRENTLY SUPPORTED FEATURES:
=============================

+ Comparitble with the pgf package and beamer class. 
+ Encapsulated Postscript File inclusion (using graphics package)
+ Effects for presentation (pause, delay, text color change) 
+ Embedded applications for interactive demonstration
+ Pictures visualization via gpic specials
+ Display of inlined Postscript using gs
+ Font antialiasing that takes background colors into account
+ Page display is interrupted on user input
+ Resizing the page adjusts the magnification
+ Start displaying at a given page or at a given link reference
+ The file is reloading on signal SIGUSR1
+ Hyperlinks
+ Active areas (execute an action when the mouse is over)
+ Background colors and images
+ Alpha blending for images

INVOCATION
==========

The invocation is

        advi [OPTIONS...] DVIFILE

where OPTIONS are:

      -safer                Safer mode: external applications are not launched
      -geometry GEOM        Sets the (maximum) geometry GEOM
      -g GEOM               Same as -geometry GEOM
      -crop                 Crop the window to the best size (default)
      -nocrop               Disable cropping
      -nomargins            Suppress horizontal and vertical margins
      -hmargin DIMEN        Horizontal margin  (default: 1cm)
      -vmargin DIMEN        Vertical margin    (default: 1cm)
      -A                    Set Postscript antialias
      -bg                   Draw in the background
      -fullwidth            Adjust size to width
      -html STRING          Make advi start at html reference of name STRING
      -nogs                 Turn off dipslay of inlined Postscript
      -nopauses             Swictch pauses off
      -page INT             Make advi start at page INT
      -pager STRING         Command to call the pager
      -safer                Safer mode: external applications are not launched
      -scalestep REAL       Scale step for '<' and '>' (default sqrt (sqrt (sqrt 2.0)))

Geometry GEOM is specified in pixels, using the standard format
for specifying geometries (i.e: "WIDTHxHEIGHT[+XOFFSET+YOFFSET]").

Dimensions (for options `-hmargin' and `-vmargin') are specified as
numbers optionally followed by two letters representing units.

When no units are given, dimensions are treated as numbers of pixels.

Currently supported units are the standard TeX units as specified in
the TeXbook (D. Knuth, Addison-Wesley, (C) 1986):

      `pt' (point), `pc' (pica), `in' (inch), `bp' (big point),
      `cm' (centimeter), `mm' (millimeter), `dd' (didot point),
      `cc' (cicero) and `sp' (scaled point).

Note that dimensions are specified w.r.t the original TeX document,
and do not correspond to what is actually shown on the screen, which
can be displayed at a different resolution than specified in the
original TeX source.


KEYSTROKES 
===========

Advi recognizes the following main keystrokes when typed in its window.
(See the documentation for the complete list of bindings.)
Each may optionally be preceded by a number, called ARG below, whose
interpretation is keystroke dependant. If ARG is unset, its value is
1, unless specified otherwise.

Advi maintains an history of previously visited pages organized as a stack. 
Additionnally, the history contains mark pages which are stronger than
unmarked pages.

Survival command kit
--------------------
 
      ?     info        - This quick info and key bindings help.
      q     quit        - End of show.
      space continue    - Move forward (arg pauses forward if any, or do as
                          return otherwise).
      ^X-^C quit        - End of show.

Moving between pages
--------------------

      n     next        - Move arg physical pages forward, leaving the
                          history unchanged.
      p     previous    - Move arg physical pages backward, leaving the
                          history unchanged.
      ,     begin       - Move to the first page.
      .     end         - Move to the last page.
      g     go          - If arg is unset move to the last page. If arg is
                          the current page do nothing. Otherwise, push the
                          current page on the history as marked, and move to
                          physical page arg .

Moving between pauses
---------------------

      N  next pause     - Move arg pauses forward (equivalent to continue).
      P  previous pause - Move arg pauses backward.


Adjusting the page size
-----------------------

      ^X-^F set         - Adjust the size of the page to fit the entire
            fullscreen    screen.
      ^F    toggle      - Adjust the size of the page to fit the entire
            fullscreen    screen or reset the page to the default size (this
                          is a toggle).
      <     smaller     - Scale down the resolution by scalestep (default
                          (((2)^1/2)^1/2)^1/2).
      >     bigger      - Scale up the resolution by scalestep (default
                          (((2)^1/2)^1/2)^1/2).
      #     fullpage    - Remove margins around the page and change the
                          resolution accordingly.
      c     center      - Center the page in the window, and resets the
                          default resolution.

Moving the page in the window
-----------------------------

      h     page left   - Moves one screen width toward the left of the
                          page. Does nothing if the left part of the page is
                          already displayed
      l     page right  - Moves one screen width toward the right of the
                          page. Does nothing if the right part of the page
                          is already displayed
      j     page down   - Moves one screen height toward the bottom of the
                          page. Jumps to the top of next page, if there is
                          one, and if the bottom of the page is already
                          displayed.
      k     page up     - Moves one screen height toward the top of the
                          page. Jumps to the bottom previous page, if there
                          is one, and if the top of the page is already
                          displayed.

      ^left  move page  - A black line draws the page borders; moving the
      button              mouse then moves the page in the window.
      ^C    toggle      - Toggles center-on-cursor flag, which when sets
            center on     moves the screen  automatically so that the cursor
            cursor        appears on the screen.

Switching views
---------------

      w     switch      - Switch view between master and client (if any).
      W     sync        - Goto page of client view corresponding to page of
                          master view.
      ^W    autoswitch  - Toggle autoswitch flag.

Redisplay commands
------------------

      r     redraw      - Redraw the current page to the current pause.
      R     reload      - Reload the file and redraw the current page.
      ^L    redisplay   - Redisplay the current page to the first pause of
                          the page.
      a  active/passive - toggle advi effects (so that reloading is silent).

Using the navigation history stack
----------------------------------

      return  forward   - Push the current page on the history stack, and
                          move forward n physical pages.
      tab    mark+next  - Push the current page on the history as marked,
                          and move forward n physical pages.
      backspace back    - Move arg pages backward according to the history.
                          The history stack is poped, accordingly.
      escape  find mark - Move arg marked pages backward according to the
                          history. Do nothing if the history does no contain
                          any marked page.

Table of contents
-----------------

      T     Thumbnails  - Process thumbnails.
      t     toc         - Display thumbnails if processed, or floating table
                          of contents if available, or else do nothing.

Writing and drawing on the page
-------------------------------

      s     write       - Give a pencil to scratch, typing characters on the
                          page.
      S     draw        - Give a spray can to scratch, drawing on the page.
      ?     info        - While in scratch mode, press ? for more info.

Using the laser pointer
-----------------------

      ^X-l  toggle laser - Toggle the laser beam to point on the page.
      ^G    laser off    - When laser is on turn it off.

  Saving the current page
-----------------------

      ^X-^S save page   - Save the current page as an image file.

Dealing with caches
-------------------

      f     load fonts  - Load all the fonts used in the document. By
                          default, fonts are loaded only when needed.
      F     make fonts  - Does the same as f, and precomputes the glyphs of
                          all the characters used in the document. This
                          takes more time than loading the fonts, but the
                          pages are drawn faster.
      C     clear       - Erase the image cache.


EYE CANDY MACROS
================

Using the LaTeX style advi.sty provided with the package, you can
embed some Active-DVI specials into your TeX documents. Advi
interprets those specials to provide some eye candy features for your
presentation:


        `\wait{sec}`

>  Delay the rendering at the point of the document for sec seconds. Wait
>  forever (i.e. stops) if parameter sec is omitted.

        \advirecord[play]{this}{material}

>  Define an "advi tag" named ``this'' to refer to the text enclosed in the
>  following brackets. The tag can be later on to play or replay the text. By
>  default, [material] is not display, unless option [play] is set.

        \adviplay[color]{this}

>  Replay the material previously bound to {this}. If [color] is provided
>  changes the drawing color to [color] before displaying.

Please look at doc/manula.dvi for more information.
Look in directory test for examples (try `advi test/*.dvi`).
Advanced examples are in directory examples.
(See `examples/seminar/clock/tools.dvi`, `examples/prosper/LL/ll.dvi`,
`examples/prosper/Join/join.dvi`.)

COPYRIGHT
=========

This program is distributed under the GNU LGPL.
See the enclosed file COPYING.


AUTHORS
=======

Alexandre Miquel, Jun Furuse Didier Rémy, Pierre Weis, Xavier Leroy, Roberto
Di Cosmo Didier Le Botlan, Alan Schmitt
