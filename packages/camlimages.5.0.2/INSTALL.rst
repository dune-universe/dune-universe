=========================================================
CamlImages - Objective Caml image processing library
=========================================================

Requirements
=================

To install CamlImages library, you need the following softwares:

* OCaml 4.01.0 or higher
* Findlib (aka ocamlfind, http://www.camlcity.org/archive/programming/findlib.html )
* OMake ( http://omake.metaprl.org/index.html )

Note that this is the minimum requirement: you can read/write BMP or
PXM (PPM, PGM, PBM) image formats but no other formats. If you want to
deal with other image formats, you need to install the corresponding
external libraries:

* libpng for PNG format
* libjpeg for JPEG format
* libexif for EXIF tags in JPEG files
* libtiff for TIFF format
* libxpm for XPM format (could be already by the X server installation)
* freetype for drawing texts using truetype fonts
* libungif/libgif for GIF format
* ghostscript for PS format
* lablgtk2, an OCaml interface to gtk+

Installation
=====================================

::

    % yes no | omake --install 
    % omake --configure <configuration options>
    % omake install

At omake --configure, you can specify CFLAGS and LDFLAGS 
to add extra header and library search paths respectively. For example,::

    % omake --configure CFLAGS="-I /usr/include/libexif" LDFLAGS="-L/opt/blah"

List of configurable variables
---------------------------------

CFLASG, INCLUDES, LDFLAGS: as usual.

ARG_WANT_<feature>=bool

      Without specifying ARG_WANT_<feature>, omake --configure automatically
      searches the availability of <feature> and enables it when found.

      If ARG_WANT_<feature>=0, the feature is not checked, and disabled.

      If ARG_WANT_<feature>=1, the feature must exist and is enabled.
      If omake fails to find the feature, the entire build fails.

      Currently the following features are available:
        GIF, PNG, JPEG, EXIF, TIFF, XPM, GS, LABLGTK2, GRAPHICS, FREETYPE

ARG_PATH_PKG_CONFIG=string
ARG_PATH_FREETYPE_CONFIG=string
ARG_PATH_GS=string
      PATH of external commands like pkg-config, freetype-config and gs
      Without specifying, omake tries to find them in the PATH.

Test
----

Before you actually install the library, you can check that it
really works, by running examples in the test directory. For the test
programs,::

        % cd test
        % omake
        % ./test
        % ./test.run

(./test.run is the bytecode executable and ./test the binary
executable).

Where to report building issues?
==========================================================

https://bitbucket.org/camlspotter/camlimages/issues?status=new&status=open
