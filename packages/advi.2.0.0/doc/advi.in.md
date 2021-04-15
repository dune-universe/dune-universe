---
title: ADVI
header: Active DVI man page
section: 1
---

# NAME

**advi** \-- a DVI previewer and presenter

[http://caml.inria.fr/advi/]

with some eye candy effects for presentation.

# VERSION

Active-DVI version @@VERSION@@

# SYNOPSIS

**advi** \[*options\] primaryfile secondaryfile

# DESCRIPTION

The **advi** program is a viewer for DVI files that also recognizes a
new class of special\'s targeted to presentations via laptop computers:
various visual effects can easily be incorporated to the presentation,
via a companion *advi.sty* LaTeX package. If the input *dvifile* is a
DVI file that has been compressed with **gzip**(1) or **bzip2**(1) it
will be automatically decompressed.

**advi** is also a full-fledged graphical presenter that provides a
wealth of effects via **embedded** applications. Embedded applications
are arbitrary Unix programs that are launched on demand during the
presentation; an embedded application may display its material on a
devoted area of the slide, leading for example to live and/or
interactive demos during the presentation.

**advi** can take a secondary files and display both in alternation.

# CURRENTLY SUPPORTED FEATURES


- Encapsulated Postscript File inclusion (using graphics package)

- Effects for presentation (pause, delay, text color change)

- Embedded applications for interactive demonstration

- Pictures visualization via **gpic**(1) specials

- Display of inlined Postscript using **gs**(1)

- Font antialiasing that takes background colors into account

- Page display is interrupted on user input

- Resizing the page adjusts the magnification

- Start displaying at a given page or at a given link reference

- The file is reloading on signal SIGUSR1

- Hyperlinks to other pages or slides

- Active areas (execute an action when the mouse is over)

- Background colors and images

- Alpha blending for images

- Bubbles, active anchors, annotations

- Init files to set up options

- Safety policy for embedded application

- Scratching (interactive drawing and writing on slides during
presentation)

- Floating table of contents (either as hyperlinks on a regular LaTeX
table of contents or as small images set up on a page)

- Transitions (slide animations when changing pages)

- Text movements

- Integrated \`\`Laser pointer\'\' feature to point to the slide

Notice that **advi** needs the **kpsewhich**(1) tool in order to find
and recompile PK fonts. (This program is part of the \`kpathsea\'
library provided by many TeX distributions.)

# OPTIONS

Options are :

**\--debug**

:   General debug

**\--debug\_pages**

:   Debug page motion

**\--ignore\_background**

:   Ignore background for antialiasing

**\--showps**

:   Print a copy of Postscript sent to **gs**(1) to stdout

**\--verbose-image-access**

:   Change the cursor while loading images

**\--version**

:   Print the current Active-DVI version, sub-version and release date

**-A**

:   Set Postscript antialiasing (default is unset)

**-ask**

:    Ask mode: launching an external application requires confirmation
    (this is the default)

**-bbox**

:   Show the bounding box

**-bgcolor STRING**

:   Set default background color (Named or RGB)

**-browser STRING**

:   Command to call the browser (default netscape-communicator)

**-cache-dir STRING**

:   Set the cache directory (default */tmp*)

**-click-turn**

:   Turn pages with mouse clicks (see the doc)

**-crop**

:   Crop the window to the best size (default)

**-disable-image-anti-aliasing**

:   Disable EPS inclusion anti-aliasing

**-edit**

:   Start in edit mode

**-exec**

:   Exec mode: allow all external applications to be executed

**-fg**

:   Draw in the foreground

**-fgcolor STRING**

:   Set default foreground color (Named or RGB)

**-film-viewer STRING**

:   Command to display film files (default **mplayer**(1))

**-fullwidth**

:   Adjust size to width

**-g GEOM**

:   Same as -geometry GEOM

**-gamma FLOAT (\>0)**

:   Gamma correction of glyphs (default 1.000000)

**-geometry GEOM**

:   Sets the (maximum) geometry GEOM

**-hmargin DIMEN**

:   Horizontal margin (default: 1cm)

**-html STRING**

:   Make **advi** start at HTML reference of name STRING

**-image-viewer STRING**

:   Command to display image files (default xv)

**-inherit-background**

:   Background options are inherited from previous page

**-n**

:   Echoes commands, but does not execute them.

**-noautoresize**

:   Prevents scaling from resizing the window (done if geometry is
    provided)

**-noautoscale**

:   Prevents resizing the window from scaling (done if geometry is
    provided)

**-nocrop**

:   Disable cropping

**-nogs**

:   Turn off display of inlined Postscript

**-nomargins**

:   Suppress horizontal and vertical margins

**-nopauses**

:   Switch pauses off

**-nowatch**

:   Don\'t display a watch when busy

**-options-file STRING**

:   Load this file when parsing this option to set up options (to
    override the options of the default *\~/.advirc* or
    *\~/.advi/advirc* init file)

**-page INT**

:   Make **advi** start at page INT

**-page-number**

:   Ask **advi** to write the current page number in a file (default is
    no)

**-page-number-file STRING**

:   Set the name of the file where **advi** could write the current page
    number (default is file \'advi\_page\_number\' in the cache
    directory)

**-pager STRING**

:   Command to call the pager (default xterm -e less)

**-passive**

:   Cancel all Active-DVI effects

**-pdf-viewer STRING**

:   Command to view PDF files (default **xpdf**(1))

**-ps-viewer STRING**

:   Command to view PS files (default **gv**(1))

**-pstricks**

:   Show moveto

**-resolution REAL**

:   DPI resolution of the screen (min. 72.27)))

**-rv**

:   Reverse video is simulated by swapping the foreground and background
    colors

**-safer**

:    Safer mode: external applications are never launched

**-scalestep REAL**

:   Scale step for \'\<\' and \'\>\' (default sqrt (sqrt (sqrt 2.0)))

**-scratch-font STRING**

:   Set the font used when scratching slides (default times bold)

**-scratch-font-color STRING**

:   Set the color of the font used when scratching slides (default red)

**-scratch-line-color INT**

:   Set the color of the pen used when scratching slides (default red)

**-scratch-line-width INT**

:   Set the width of the pen used when scratching slides (default 2)

**-thumbnail-scale INT**

:   Set the number of thumbname per line and column to INT

**-thumbnails**

:   Create thumbnails for your slides and write them into cachedir

**-thumbnails-size STRING**

:   Fix thumbnails size (default 24x32)

**-v**

:   Print the current Active-DVI version

**-vmargin DIMEN**

:   Vertical margin (default: 1cm)

**-w STRING**

:   A/a enable/disable all warnings

**-watch FLOAT**

:   Delay before the watch cursor appears (default 0.500000s)

**-help**

:   Display this list of options

**\--help**

:   Display this list of options

Geometry GEOM is specified in pixels, using the standard format for
specifying geometries (i.e., \"WIDTHxHEIGHT\[+XOFFSET+YOFFSET\]\").

Dimensions (for options \`-hmargin\' and \`-vmargin\') are specified as
numbers optionally followed by two letters representing units. When no
units are given, dimensions are treated as numbers of pixels. Currently
supported units are the standard TeX units as specified in the TeXbook
(D. Knuth, Addison-Wesley, (C) 1986):

> \`pt\' (point), \`pc\' (pica), \`in\' (inch), \`bp\' (big point),
> \`cm\' (centimeter), \`mm\' (millimeter), \`dd\' (didot point), \`cc\'
> (cicero) and \`sp\' (scaled point).

Note that dimensions are specified w.r.t the original TeX document, and
do not correspond to what is actually shown on the screen, which can be
displayed at a different resolution than specified in the original TeX
source.

# KEYSTROKES

Advi recognizes the following keystrokes when typed in its window. Some
of them may optionally be preceded by a number, called ARG below, whose
interpretation is keystroke dependant. If ARG is unset, its value is 1.

Advi maintains an history of previously visited pages organized as a
stack. Additionally, the history contains mark pages which are stronger
than unmarked pages.

 **?**

 :   Quick info and key bindings help.

 ```{=html}
 <!-- -->
 ```

 **q**

 :   Quits the program.

 ```{=html}
 <!-- -->
 ```

 **\^X-\^C (Control-X Control-C)**

 :   Quits the program.

 ```{=html}
 <!-- -->
 ```

 **\^X-\^F (Control-X Control-F)**

 :   Turn to full screen mode.

 ```{=html}
 <!-- -->
 ```

 **\^F (Control-F)**

 :   Toggle to switch from full screen to normal mode and converse.

 ```{=html}
 <!-- -->
 ```

 **\^\<button\> (Control + left mouse button)**

 :   Allow moving the page into the window (useful in full screen
     mode).

 ```{=html}
 <!-- -->
 ```

 **return**

 :   If ARG is non zero, push the current page on the history stack,
     and move forward ARG physical pages.

 ```{=html}
 <!-- -->
 ```

 **n**

 :   Move ARG physical pages forward, leaving the history unchanged.

 ```{=html}
 <!-- -->
 ```

 **p**

 :   Move ARG physical pages backward, leaving the history unchanged.

 ```{=html}
 <!-- -->
 ```

 **\<tab\>**

 :   Push the current page on top of the history as a marked page, do
     no move.

 ```{=html}
 <!-- -->
 ```

 **\<space\>**

 :   Move to the next pause if any, or do as return otherwise.

 ```{=html}
 <!-- -->
 ```

 **\<backspace\>**

 :   Move ARG pages backward according to the history. The history
     stack is popped, accordingly.

 ```{=html}
 <!-- -->
 ```

 **\<escape\>**

 :   Move ARG marked pages backward according to the history. Do
     nothing if the history does no contain any marked page.

 ```{=html}
 <!-- -->
 ```

 **g**

 :   If ARG is unset move to the last page. If ARG is the current page
     do nothing. Otherwise, push the current page on the history as a
     marked page, and move to the physical page ARG.

 ```{=html}
 <!-- -->
 ```

 **,**

 :   Move to the first page.

 ```{=html}
 <!-- -->
 ```

 **.**

 :   Move to the last page.

 ```{=html}
 <!-- -->
 ```

 **c**

 :   Center the page in the window and resets the default resolution.

 ```{=html}
 <!-- -->
 ```

 **\<**

 :   Scale the resolution by 1/scalestep (default 1/sqrt (sqrt (sqrt
     2.0))).

 ```{=html}
 <!-- -->
 ```

 **\>**

 :   Scale the resolution by scalestep (default sqrt (sqrt (sqrt
     2.0))).

 ```{=html}
 <!-- -->
 ```

 **f**

 :   Load all the fonts used in the documents. By default, fonts are
     loaded only when needed.

 ```{=html}
 <!-- -->
 ```

 **F**

 :   Does the same as \`f\', and precomputes the glyphs of all
     characters used in the document. This takes more time than loading
     the fonts, but the pages are drawn faster.

 ```{=html}
 <!-- -->
 ```

 **r**

 :   Redraw the current page.

 ```{=html}
 <!-- -->
 ```

 **R**

 :   Reload the file and redraw the current page.

 ```{=html}
 <!-- -->
 ```

 **C**

 :   Erase the image cache.

 ```{=html}
 <!-- -->
 ```

 **T**

 :   Process thumbnails (graphical table of contents for the show).

 ```{=html}
 <!-- -->
 ```

 **t**

 :   Display thumbnails if processed or floating table of contents, or
     do nothing.

 ```{=html}
 <!-- -->
 ```

 **\^X-l (Control-X l)**

 :   Toggle on or off the laser pointer.

 ```{=html}
 <!-- -->
 ```

 **s**

 :   Turn on the write scratching mode (to interactively write on the
     slide). When in scratching mode press **?** to get help.

 ```{=html}
 <!-- -->
 ```

 **S**

 :   Turn on the draw scratching mode (to interactively draw on the
     slide). When in scratching mode press **?** to get help.

 ```{=html}
 <!-- -->
 ```

 **\^X-\^S (Control-X Control-S)**

 :   Save an image of the current state of the slide. The default image
     file name is shot **\<n\>** where **\<n\>** is the next available
     integer number such that no previously saved slide image is
     overwritten. The default image format is PNG which is the
     extension of the image file.

A click on an hyperlink, push the current page on this history as marked
(unless the target page is the current page) and move to the target
page. If the target is visible, it highlights the target.

Moreover, the user can drag the currently displayed page in the window
in order to change its relative position. (This is useful when the page
is displayed at a resolution such that it cannot fit in the window.)

EYE CANDY MACROS
================

Using the LaTeX style *advi.sty* provided with the package, you can
embed some Active-DVI specials into your TeX documents. **Advi**
interprets those specials to provide some eye candy features for your
presentation. For the casual user, the *advi-slides.sty* package gives a
truly simple way to write a show for **advi** (see in the examples
directory or the documentation inside the *advi-slides.sty* file for
more information).

**\\adviwait**

:   Active-DVI stops rendering at the point of the document and wait a
    user key stroke.

```{=html}
<!-- -->
```

**\\adviwait{sec}**

:   Delay the rendering at the point of the document for sec seconds.

```{=html}
<!-- -->
```

**\\advirecord\[play\]{this}{material}**

:   Define an \"advi tag\" named \`\`this\'\' to refer to the text
    enclosed in the following brackets. The tag can be used to change
    the color of the text later.

```{=html}
<!-- -->
```

**\\advirecord{this}{material}**

:   Same as \\advirecord\[play\]{this}{material}, but does not render
    the text at this point. You can display the text later, using the
    \\adviplay macro.

```{=html}
<!-- -->
```

**\\adviplay{this}**

:   Display the texts associated with the tag \`\`this\'\'.

```{=html}
<!-- -->
```

**\\adviplay\[col\]{this}**

:   Display the texts associated with the tag \`\`this\'\', using the
    color \`\`col\'\'.

The directory examples contains a lot of presentations. Please look also
at *test/demo\*.{tex\|dvi}* and *test/macros.{tex\|dvi}* for a rather
comprehensive demonstration of Active-DVI capabilities.

# COPYRIGHT

This program is distributed under the GNU LGPL.

# SEE ALSO

**latex**(1), **kpsewhich**(1), the Active-DVI user\'s

and the

# AUTHORS

Jun Furuse \<Jun.Furuse\@inria.fr\> Pierre Weis
\<Pierre.Weis\@inria.fr\> Didier Remy \<Didier.Remy\@inria.fr\> inlined
Postscript, hyperlinks Roberto Di Cosmo \<dicosmo\@pauillac.inria.fr\>
Xavier Leroy \<Xavier.Leroy\@inria.fr\> **gpic**(1) specials Didier Le
Botlan \<Didier.Le\_Botlan\@inria.fr\> Alan Schmitt
\<Alan.Schmitt\@inria.fr\> Alexandre Miquel
\<Alexandre.Miquel\@inria.fr\>

The original version of this manual page was written by Sven LUTHER
\<luther\@debian.org\>, for the Debian GNU/Linux system port of **advi**
version 1.2. This page has then been enhanced and updated for later
versions of Active-DVI, and finally rewritten for version 1.6 by Pierre
Weis.
