(** A Caml modelisation of the X font descriptions.

   In X parlance, this is the X Logical Font Description (or XLFD for short).

   We define here a set of Caml type definitions that describes the X font
   description schema.

*)

(* Fonts included with X

Standard bitmap fonts

The Sample Implementation of X11 comes with a large number of bitmap
fonts, including the ``fixed'' family, and bitmap versions of Courier,
Times and Helvetica. In the SI, these fonts are provided in the ISO
8859-1 encoding (ISO Latin Western-European).

Standard scalable fonts

XFree86 includes all the scalable fonts distributed with X11R6.

Standard Type 1 fonts

    IBM Courier set of fonts cover ISO 8859-1 and ISO 8859-2 as well as
    Adobe Standard Encoding:

    -adobe-courier-medium-*-*--0-0-0-0-m-0-*-*
      (in /usr/X11R6/lib/X11/fonts/Type1/cour*.pfa)
    -adobe-utopia-*-*-normal--0-0-0-0-p-0-iso8859-1
      (in /usr/X11R6/lib/X11/fonts/Type1/UT*.pfa)

Standard Speedo fonts

    Bitstream Courier and Charter:

    -bitstream-courier-*-*-normal--0-0-0-0-m-0-iso8859-1
    -bitstream-charter-*-*-normal--0-0-0-0-p-0-iso8859-1
     (in the font files /usr/X11R6/lib/X11/fonts/Type1/c*bt_.pfb)


   All X fonts have a XLFD 14-part name

   FontNameRegistry-Foundry-FamilyName-WeightName-Slant-SetwidthName
   -AddStyleName-PixelSize-PointSize-ResolutionX-ResolutionY
   -Spacing-AverageWidth-CharSetRegistry-CharSetCodin

XFree86 includes the Lucidux family of Type 1 fonts. This family
 consists of the fonts Lucidux Serif, with XLFD

     -b&h-lucidux serif-medium-*-normal--*-*-*-*-p-*-*-*

Lucidux Sans, with XLFD

     -b&h-lucidux sans-medium-*-normal--*-*-*-*-p-*-*-*

and Lucidux Mono, with XLFD

     -b&h-lucidux mono-medium-*-normal--*-*-*-*-m-*-*-*

Each of these fonts currently comes in Roman and oblique variants
(bold variants will be included in a future release) and has 337
glyphs covering the basic ASCII Unicode range, the Latin 1 range, as
well as the Extended Latin range. In particular, these fonts include
all the glyphs needed for ISO 8859 parts 1, 2, 3, 4, 9 and 15.
*)

(*
You may use either upper-case or lower-case letters when you specify a
characteristic.

Reference XLFD names are all lower-case.

X11 font instances may also be specified by short name. Unlike an
XLFD, a short name has no structure and is simply a conventional name
for a font instance. Two short names are of particular interest, as
they are handled specially by the server, and the server will not
start if font instances with these names cannot be opened. These are
`fixed', which specifies the fallback font to use when the requested
font cannot be opened, and `cursor', which specifies the set of glyphs
to be used by the mouse pointer.

Short names are usually implemented as aliases to XLFDs; the `fixed'
and `cursor' aliases are defined in

     /usr/X11R6/lib/X11/fonts/misc/fonts.alias

In general:
$ locate fonts.alias
/usr/share/doc/sketch-0.6.8/fonts.alias
/usr/share/fonts/ISO8859-2/75dpi/fonts.alias
/usr/X11R6/lib/X11/fonts/misc/fonts.alias
/usr/X11R6/lib/X11/fonts/mozilla-fonts/fonts.alias
/usr/X11R6/lib/X11/fonts/75dpi/fonts.alias
/usr/X11R6/lib/X11/fonts/100dpi/fonts.alias
/usr/lib/metamail/fonts/fonts.alias
/usr/local/lib/xtel/fonts/fonts.alias

A font alias file is as this:

! note the quotes:
fixed        -misc-fixed-medium-r-semicondensed--13-120-75-75-c-60-iso8859-1
-adobe-times-medium-r-normal--0-0-0-0-p-0-iso8859-1 -urw-"Nimbus Roman No9 L"-regular-r-normal--0-0-0-0-p-0-iso8859-1

(Lines starting by ! are comments.
 Empty lines are allowed and have no meaning.
 Other lines are: name SPACE font-XLFD meaning that the name is aliased to the
 font-XLFD.
 Note that the name is anything before the space (not starting by a !).
 Note also that a correct line may not start by a minus sign (-).
 Note that we may have double quotes "inside" XLFD names !)

Pw: The example given just violates the second note.
Should write a parse_fonts_alias_file utility ...
*)

type weightName =
   | WnNil
   | WnBold
   | WnBook
   | WnDemiBold
   | WnExtraBold
   | WnHeavy
   | WnLight
   | WnMedium
   | WnRegular
   | WnSemi_Bold
   | WnThin

and slant =
  | SlNil
  | SlItalic
  | SlOblique
  | SlReverseItalic
  | SlReverseOblique
  | SlRoman

and setwidthName =
  | SwNil
  | SwBold
  | SwCompressed
  | SwCondensed
  | SwExtended
  | SwNormal
  | SwSemiCondensed

and addStyleName =
  | AsNil
  | AsAnn
  | AsCursive
  | AsFantasy
  | AsFire
  | AsFold
  | AsJa
  | AsKo
  | AsMedium
  | AsOldStyle
  | AsPlain
  | AsSansSerif
  | AsScript
  | AsSerif
  | AsStick
  | AsStone

and spacing =
  | ScNil
  | ScCharacterCell         (* Character cell (glyphs are boxes) *)
  | ScMonospace             (* Monospace (fixed pitch) *)
  | ScProportional          (* Proportional (variable pitch) *)
;;

type size = int option;;

type font_variant = {
  weightName : weightName;
  slant : slant;
  setwidthName : setwidthName;
  addStyleName : addStyleName;
  pixelSize : size;
  pointSize : size;
}
;;

type width = int option;;
type resolution = int option;;

type font = {
  fontNameRegistry : string;
  foundry : string;
  familyName : string;
  scalable : bool;
  resolutionX : resolution;
  resolutionY : resolution;
  (* The horizontal (X) and vertical (Y) resolution of the device
     that the font was designed for, measured in pixels-per-inch. *)
  spacing : spacing;
  averageWidth : width;
  (* Average, unweighted width of all the glyphs in the font, measured
     in 1/10th device-dependent pixels. *)
  charSetRegistry : string;
  charSetEncoding : string;
  variants : font_variant array;
}
;;

type x_font =
  | Font of font
  | Alias of font_name * font

and font_name = string

and file_name = string
;;

type x_fonts_table = (font_name, x_font) Hashtbl.t;;
