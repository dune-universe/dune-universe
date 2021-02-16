
# 0.9 (January 19, 2021)
  - fixes floating-point error in extremum computation
  - special points are kept in empty from box
  - migrate to cairo2 and dune
  - added experimental export to pgf
  - added Arrows.mk_classic
  - added Helpers.box_point_line, point_box_line, box_loop, box_label_loop

# 0.8.2 (March 10, 2017)
  - mlpost does not complain anymore about Metapost errors
  - ocamlopt is called with warning 58 disabled
  - fixed installation with OCaml 4.04
  - new module [Triangle] to draw tree-like, triangular shapes
  - :exclamation: (internal) Misc.call_cmd now does print output of
    the called program directly, instead of returning it as a string
  - :exclamation: module Generate has been removed

# 0.8.1 (April 26th 2010)
  - configure: store absolute paths of programs
  - configure: fixed META file
  - doc: documentation for contribs
  - contrib lablgtk: background setting possible and function auto_aspect

# 0.8.0 (April 13th, 2010)
  - :exclamation: ocaml >= 3.10.1 is required now
  - :exclamation: externalimage work only with png image
  - :exclamation: module Color: the definitions of the following
  colors have changed: lightblue, green, lightgreen, orange,
  lightcyan, purple, lightyellow These colors are now compatible to
  HTML/CSS and X11 definitions
  - :exclamation: Box: Box.tabularl did modify the input boxes, now it
  leaves them unchanged (reported by Julien Signoles)
  - contrib Mlpost_lablgtk : define a gtk widget to display mlpost
  figures It also allow to easily create an interface to interact with
  an mlpost figures
  - contrib Mlpost_dot : Use graphviz (dot) to place picture, box, ...
    make contrib && make install-contrib
    mlpost -contrib dot [...]
  - module Real_plot: Plot function from float to float. It can use
      logarithmic scale.
  - module Color: new function hsv to create a color from hsv
  colorspace and color_gen to generate different colors using hsv
  colorspace
  - concrete computations are now available without the Cairo library
  - option -ps with -cairo
  - adding Concrete.baseline
  - Num: new units em, ex
  - Bugfix: "make install" with ocamlfind (reported by Julien Signoles)
  - Bugfix: Concrete does not complain about being unsupported for the following
  functions: set_verbosity; set_prelude, set_prelude2, set_t1disasm
  - Bugfix: Don't use "tracingchoices"
  - Bugfix #411: correct definition of objects used in Path.subpath
  - metapost errors are printed (this should rarely occur)
  - each call of mpost are done in separate and temporary directories


# 0.7.4 (October 20th, 2009)
  - Mlpost tool : Fix compilation with ocamlbuild

# 0.7.3 (October 13th, 2009)
  - Fix installation without ocamlfind and without ocamlbuild

# 0.7.2 (October 9th, 2009)
  - :exclamation: -classic-display is not an option of mlpost tool
    anymore (use -v instead)
  - :exclamation: Change in the signature of Cairost.emit_cairo
  - Fix the -compile-name option with ocamlbuild
  - ocamlfind remove/install is used if ocamlfind is present
  - The backend Concrete output informations only with the verbose option
  - Radar: fixed size of bullets
  - Helpers: the functions for boxes have a new optional argument
  [within] to give a box in which the arguments will be searched
  - Box: new functions [set_{post,pre}_draw]
  - :exclamation: Box: [get_name] now returns a string option
  - Tree.Simple: alignment options for [node]
  - Box: optional argument dash

# 0.7.1 (July 24th, 2009)
  - Fix for Performance bug when shifting boxes

# 0.7 (July 23rd, 2009)
  - :exclamation: add Point.draw and Path.draw (alias of Command.draw)
  which can mask Command.draw in case of an open Point after an open
  Command
  - :exclamation: Command.draw_arrow becomes Arrow.simple
  - :exclamation: Arrow.draw: ~pos becomes ~anchor, new ~pos is point on path
  - :exclamation: Arrow.draw: now gives the same result by default as
  Arrow.simple (former Command.draw_arrow)
  - :exclamation: Arrow.draw2 becomes Arrow.point_to_point
  - :exclamation: Mlpost tool: -pdf now the default; use -ps to
    produce .1 files
  - :exclamation: Mlpost tool: erases all generated intermediate files
    on success
  - New experimental backend using Cairo; it permits output in PS,
  PDF, SVG and X11; use it with commandline option -cairo. It is
  intended to deliver the same results as the old metapost
  backend. Please send a bug report if it is not the case
  - A module Concrete which permits to compute concrete values of
  mlpost objects, e.g. the float value corresponding to an object of
  type Num.t , the concrete point { x : float; y : float }
  corresponding to a Point.t, and so on
  - A better tree drawing algorithm (module Tree)
  - new function Tree.nodel to add labels to tree edges
  - "Smart" paths to construct a path by giving only a sequence of
  directions (module Path)
  - Histograms and Radar diagrams (modules Hist and Radar)
  - The type Picture.t now is equal to the type Command.t (no more
  conversion needed)
  - module Box: each box has a name by default; use Box.sub to retrieve a box
  with the same name inside another
  - New optional argument sep of Path.strip to strip both ends of a path; used
  in Tree, Box.cpath, and Helpers
  - New position constructors `North, `South, `Upperleft to improve
  upon `Top, `Bot etc, but the old variants are still there

# 0.6 (February 4th, 2009)
  - :exclamation: "open Mlpost" is not added to input files any more -
  users have to add by themselves
  - :exclamation: the type Command.figure becomes Command.t
  - inclusion of external images (png, jpg etc)
  - transformations on boxes
  - Box.{grid,gridl,gridi}: new options hpadding, vpadding, stroke, pen
  - additional options for many functions
  - corrections of some small bugs in box calculations
  - A function in the API to scan a TeX file for the prelude

# 0.5 (Octobre 20, 2008, first public release)
  - new option -native to use native compilation, useful for
    complicated pictures

# 0.3
  - new module Pos to place lists (arrays, trees)
  - :exclamation: Num.f function removed
  - new commandline arguments -v, -eps
  - :exclamation: The functions in the Shapes module now build objects
  of type Shapes.t instead of Path.t
  - :exclamation: In Diag, one can now specify more (and different)
    types of boxes for nodes

# 0.2 (July 22nd, 2008)
  - Box: no more use of boxes.mp, replaced by Ocaml code
  - License: LGPL updated to version 2.1
  - Num: t is now an abstract datatype
  - Moved repository to a trunk/branches style
  - Subversion repository updated to schema version 5

# 0.1
  - first release of Mlpost

