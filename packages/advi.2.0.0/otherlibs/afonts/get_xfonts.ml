open Xfonts;;

exception Invalid_platform of string;;
exception Invalid_XLFD of string;;

let debug_flag = ref false;;
let debug s = if !debug_flag then prerr_endline s;;

let warning s = prerr_endline s;;

let weightName_of_string = function
  | "*" -> WnNil
  | "bold" -> WnBold
  | "book" -> WnBook
  | "demibold" -> WnDemiBold
  | "extrabold" -> WnExtraBold
  | "heavy" -> WnHeavy
  | "light" -> WnLight
  | "medium" -> WnMedium
  | "regular" -> WnRegular
  | "semi bold" -> WnSemi_Bold
  | "thin" -> WnThin
  | _ -> (* Warning *) WnRegular

and slant_of_string = function
  | "*" -> SlNil
  | "r" -> SlRoman
  | "i" -> SlItalic
  | "o" -> SlOblique
  | "ri" -> SlReverseItalic
  | "ro" -> SlReverseOblique
  | _ -> (* Warning. *) SlRoman

and setwidthName_of_string = function
  | "*" -> SwNil
  | "bold" -> SwBold
  | "compressed" -> SwCompressed
  | "condensed" -> SwCondensed
  | "extended" -> SwExtended
  | "normal" -> SwNormal
  | "semicondensed"
  | "semi condensed" -> SwSemiCondensed
  | _ -> (* Warning. *) SwNormal

and addStyleName_of_string = function
  | "*" -> AsNil
  | "ann" -> AsAnn
  | "cursive" -> AsCursive
  | "fantasy" -> AsFantasy
  | "fire" -> AsFire
  | "fold" -> AsFold
  | "ja" -> AsJa
  | "ko" -> AsKo
  | "medium" -> AsMedium
  | "oldstyle" -> AsOldStyle
  | "plain" -> AsPlain
  | "sans" | "sansserif" -> AsSansSerif
  | "script" -> AsScript
  | "serif" -> AsSerif
  | "stick" -> AsStick
  | "stone" -> AsStone
  | _ -> (* Warning. *) AsMedium

and spacing_of_string = function
  | "*" -> ScNil
  | "c" -> ScCharacterCell
  | "m" -> ScMonospace
  | "p" -> ScProportional
  | _ -> (* Warning. *) ScCharacterCell (* As in fixed fonts *)
;;

(* foundry (fndry), family (fmly), weight (wght), slant, stretch (sWdth),
   adstyl,

(1) FontNameRegistry.

     The authority that registered the font. Usually left blank. If
     there is a value in the field, it is of the form +version, where
     version is the version of some future XLFD specification.

(2) Foundry.

     The name of the digital type foundry that digitized the font data.

(3) FamilyName.

     The trademarked commercial name of the font. If the FamilyName
     contains spaces, do one of the following for a request XLFD name:

     Enclose the entire XLFD name in double quotes (""). For example,
     this fonts.alias file line.

     italic "-agfa-cg century schoolbook italic-normal-i-*---240---p-150-*-roman8"

     Use wild cards for part of the field.

     italic -agfa-*schoolbook*italic-normal-i-*---240---p-150-*-roman8

(4) WeightName[extensions].

     The relative weight of the font, such as bold.

     For scalable typefaces, the user may specify that the font be
     darker (bolder) or lighter than the normal for that font.

(5) Slant[extensions].

     A code indicating the direction of the slant for the font.

     r       Roman (no slant)
     i       Italic (slant left)
     o       Oblique (slant left)
     ri      Reverse italic (slant right)
     ro      Reverse oblique (slant right)

(6) SetwidthName, stretch (sWdth).

     The width-per-unit of the font, such as compressed or expanded.

(7) AddStyleName[extensions].

     A description of anything else needed to uniquely identify the
     font, such as serif or cursive.

(8) PixelSize, size in pixels (pxlsz):

     An integer describing the height of an EM square in pixels. (This
     parameter scales the font.)

(9) PointSize, size in points (ptSz):

     An integer giving the EM square size in decipoints. For example
     140 is 14-points. (This parameter scales the font.)

(10) (11) ResolutionX, ResolutionY height (resx), width (resy),

     The horizontal (X) and vertical (Y) resolution of the device that
     the font was designed for, measured in pixels-per-inch. If the
     resolution is not specified in a request XLFD name, the X server
     defaults to the resolution of the display for which the font is
     requested.

(12) Spacing (spc).

     A code indicating the spacing between units in the font.

     M       Monospaced (fixed pitch)
     P       Proportional spaced (variable pitch)
     C       Character cell.  The glyphs of the font can be thought of as
             "boxes" of the same width and height that are stacked side by
             side or top to bottom.

(13) AverageWidth.

     An integer string giving the average, unweighted width of all the
     glyphs in the font, measured in 1/10th device-dependent pixels.

(14) CharSetRegistry.

     The registration authority that registered the specified
     CharSetEncoding. The XLFD conventions expect organizations that
     control characters to register with the X Consortium and be given
     a unique name to use in this field.

(15) CharSetEncoding[extensions].

     The character set from which the characters in the font are drawn.

   spc,
   average width (avgWdth) (parameter to anamorphically scale the font),
   registry (rgstry),
   encoding (as EncA, EncB)

  Scalable fonts come back from the server with zero for the pixel size,
  point size, and average width fields.  Selecting a font name with a zero in
  these positions results in an implementation-dependent size. Any pixel or
  point size can be selected to scale the font to a particular size.  Any
  average width can be selected to anamorphically scale the font (although
  you may find this challenging given the size of the average width menu).

*)

let regular_xlfd_encdng_fmt () = format_of_string "%s ";;
let delimited_xlfd_encdng_fmt () = format_of_string "%[^\"]\" ";;

let xlfd_fmt encdng_fmt =
  (* fnry fndry fmly wght slant sWdth adstyl *)
     "%s@-%s@-%s@-%s@-%s@-%s@-%s@-" ^^
  (* pxlsz ptsz resx resy *)
     "%s@-%s@-%s@-%s@-" ^^
  (* spc avgWdth rgstry *)
     "%s@-%s@-%s@-" ^^
  (* encdng *)
     encdng_fmt
;;

(* The format to scan an XLFD delimited by "" characters. *)
let delimited_xlfd_fmt () = "\"" ^^ xlfd_fmt (delimited_xlfd_encdng_fmt ())
;;

let regular_xlfd_fmt () = xlfd_fmt (regular_xlfd_encdng_fmt ())
;;

(* The format to scan a regular XLFD, with char set encoding ending at end of line. *)
let scan_regular_xlfd_fmt = regular_xlfd_fmt ();;
(* The format to scan an XLFD delimited by "" characters. *)
let scan_delimited_xlfd_fmt = delimited_xlfd_fmt ();;

(* The format to print regular XLFD. *)
let print_regular_xlfd_fmt = regular_xlfd_fmt ();;

let string_of_weightName = function
  | WnNil -> "*"
  | WnBold -> "bold"
  | WnBook -> "book"
  | WnDemiBold -> "demibold"
  | WnExtraBold -> "extrabold"
  | WnHeavy -> "heavy"
  | WnLight -> "light"
  | WnMedium -> "medium"
  | WnRegular -> "regular"
  | WnSemi_Bold -> "semi bold"
  | WnThin -> "thin"

and string_of_slant = function
  | SlNil -> "*"
  | SlItalic -> "i"
  | SlOblique -> "o"
  | SlReverseItalic -> "ri"
  | SlReverseOblique -> "ro"
  | SlRoman -> "r"

and string_of_setwidthName = function
  | SwNil -> "*"
  | SwBold -> "bold"
  | SwCompressed -> "compressed"
  | SwCondensed -> "condensed"
  | SwExtended -> "extended"
  | SwNormal -> "normal"
  | SwSemiCondensed -> "semicondensed"

and string_of_addStyleName = function
  | AsNil -> "*"
  | AsAnn -> "ann"
  | AsCursive -> "cursive"
  | AsFantasy -> "fantasy"
  | AsFire -> "fire"
  | AsFold -> "fold"
  | AsJa -> "ja"
  | AsKo -> "ko"
  | AsMedium -> "medium"
  | AsOldStyle -> "oldstyle"
  | AsPlain -> "plain"
  | AsSansSerif -> "sansserif"
  | AsScript -> "script"
  | AsSerif -> "serif"
  | AsStick -> "stick"
  | AsStone -> "stone"

and string_of_spacing = function
  | ScNil -> "*"
  | ScCharacterCell -> "c"
  | ScMonospace -> "m"
  | ScProportional -> "p"
;;

let string_of_size = function
  | None -> "*"
  | Some sz -> string_of_int sz
;;

let string_of_resolution = function
  | None -> "*"
  | Some sz -> string_of_int sz
;;

let string_of_width = function
  | None -> "*"
  | Some sz -> string_of_int sz
;;

let add_xlfd ob font =
  let font_variants = font.variants in
  let v0 = font_variants.(0) in
  let {
    weightName = wght;
    slant = slant;
    setwidthName = sWdth;
    addStyleName = adStyl;
    pixelSize = pxlsz;
    pointSize = ptsz;
  } = v0 in
  Printf.bprintf ob print_regular_xlfd_fmt
    font.fontNameRegistry font.foundry font.familyName
      (string_of_weightName wght) (string_of_slant slant)
      (string_of_setwidthName sWdth) (string_of_addStyleName adStyl)
    (string_of_size pxlsz) (string_of_size ptsz)
      (string_of_resolution font.resolutionX) (string_of_resolution font.resolutionY)
    (string_of_spacing font.spacing) (string_of_width font.averageWidth)
    font.charSetRegistry font.charSetEncoding
;;

let xlfd_of_font font =
  let ob = Buffer.create 255 in
  add_xlfd ob font;
  Buffer.contents ob
;;

let size_of_string = function
  | "*" | "" -> None
  | sz -> Some (int_of_string sz)
;;

let resolution_of_string = function
  | "*" | "" -> None
  | sz -> Some (int_of_string sz)
;;

let width_of_string = function
  | "*" | "" -> None
  | sz -> Some (int_of_string sz)
;;

let make_font_variant wght slant sWdth adstyl pxlsz ptsz = {
  pixelSize = size_of_string pxlsz;
  pointSize = size_of_string ptsz;
  weightName = weightName_of_string wght;
  slant = slant_of_string slant;
  setwidthName = setwidthName_of_string sWdth;
  addStyleName = addStyleName_of_string adstyl;
}
;;

let make_font fnmRy fndry fmly wght slant sWdth adstyl pxlsz ptsz
      resx resy spc avgWdth rgstry encdng =
  let scalable = (size_of_string pxlsz = Some 0) in
  let font_variant =
    make_font_variant wght slant sWdth adstyl pxlsz ptsz in {
    charSetRegistry = rgstry;
    charSetEncoding = encdng;
    familyName = fmly;
    fontNameRegistry = fnmRy;
    foundry = fndry;
    resolutionX = resolution_of_string resx;
    resolutionY = resolution_of_string resy;
    (* The horizontal (X) and vertical (Y) resolution of the device
       that the font was designed for, measured in pixels-per-inch. *)
    averageWidth = width_of_string avgWdth;
    (* Average, unweighted width of all the glyphs in the font, measured
       in 1/10th device-dependent pixels. *)
    scalable = scalable;
    spacing = spacing_of_string spc;
    variants = [| font_variant |];
  }
;;

let rec parse_xlfd ib =
  Scanf.bscanf ib " %0c"
   (function
    | '!' -> Scanf.bscanf ib "%_s@\n" parse_xlfd ib
    | '-' ->
      Scanf.bscanf ib "%s@\n"
        (fun s ->
         let ib = Scanf.Scanning.from_string s in
         let font = Scanf.bscanf ib scan_regular_xlfd_fmt make_font in
         s, Font font)
      (* This line does not start with a -
         It is thus an alias: we just ignore it and go on. *)
    | _ ->
      Scanf.bscanf ib "%s@\n" (fun s -> raise (Invalid_XLFD s))
   )
;;

let parse_xlfds ic =
  let ib = Scanf.Scanning.from_channel ic in
  let rec loop accu =
    if Scanf.Scanning.end_of_input ib then accu else
    let new_accu =
      try parse_xlfd ib :: accu with
      | Invalid_XLFD s ->
        warning
          (Printf.sprintf "Xfonts.parse_xlfds: skip invalid XLFD %s" s);
        accu in
    loop new_accu in
  loop []
;;

let get_x_fonts_ic display =
  Unix.open_process_in ("xlsfonts -d " ^ display ^ " | sort -u");;

(* Returns the entire list of available X fonts. *)
let get_x_fonts_info display =
  parse_xlfds (get_x_fonts_ic display)
;;

(* Parsing font alias files. *)

let rec parse_font_alias ib =
  Scanf.bscanf ib " %0c"
   (function
    | '!' -> Scanf.bscanf ib "%_s@\n" parse_font_alias ib
    | _ ->
      Scanf.bscanf ib "%s " (fun alias_name ->
      debug (Printf.sprintf "Xfonts.parse_font_alias: Found %s" alias_name);
      if alias_name = "" then
        Scanf.bscanf ib "%s@\n"
          (fun s -> raise (Invalid_XLFD ("empty alias name in " ^ s))) else
      Scanf.bscanf ib " %0c" (fun c ->
        let fmt =
          if c = '\"'
          (* This line starts with a ": it should be a valid XLFD delimited
             with " *)
          then (debug "Delimited xlfd found."; scan_delimited_xlfd_fmt)
          else scan_regular_xlfd_fmt in
          let font = Scanf.bscanf ib fmt make_font in
        alias_name, Alias (alias_name, font))))
;;

let parse_fonts_alias ib =
  let rec loop accu =
    if Scanf.Scanning.end_of_input ib then accu else
    let new_accu =
      try parse_font_alias ib :: accu with
      | Invalid_XLFD s ->
        warning
          (Printf.sprintf "Xfonts.parse_fonts_alias: skip invalid font alias line %s" s);
        accu in
    loop new_accu in
  loop []
;;

let parse_fonts_alias_file fname =
  debug (Printf.sprintf "Xfonts.parse_fonts_alias_file: parsing %s" fname);
  if not (Sys.file_exists fname) then [] else
  let ib = Scanf.Scanning.from_file fname in
  parse_fonts_alias ib
;;

let get_x_fonts_aliases alias_files =
  List.fold_right
    (fun fname res -> parse_fonts_alias_file fname @ res)
    alias_files
    []
;;

let get_x_display () =
  match Sys.os_type with
  | "Unix" ->
    let display =
      try Unix.getenv "DISPLAY" with
      | Not_found ->
        (* try a reasonable default *)
        ":0.0" in
    display
  | s -> raise (Invalid_platform s)
;;

let fonts_alias_files = [
  "/usr/X11R6/lib/X11/fonts/misc/fonts.alias";
  "/usr/X11R6/lib/X11/fonts/75dpi/fonts.alias";
  "/usr/X11R6/lib/X11/fonts/100dpi/fonts.alias";
  "/usr/share/fonts/X11/100dpi/fonts.alias";
  "/usr/share/fonts/X11/75dpi/fonts.alias";
  "/usr/share/fonts/X11/misc/fonts.alias";
]
;;

let get_all_x_fonts display =

  let xlfds = get_x_fonts_info display in
  let aliases = get_x_fonts_aliases fonts_alias_files in

  let all_fonts = aliases @ xlfds in

  if !debug_flag then begin
    List.iter
      (function
       | (alias_name, Alias (_, {familyName = f})) ->
         print_endline (Printf.sprintf "Alias: %s %s" alias_name f)
       | (fname, Font {familyName = f}) ->
         print_endline (Printf.sprintf "Font %s" f))
      all_fonts;

    print_newline ();
  end;

  all_fonts
;;

let all () = get_all_x_fonts (get_x_display ());;

(*
fixed        -misc-fixed-medium-r-semicondensed--13-120-75-75-c-60-iso8859-1
variable     -*-helvetica-bold-r-normal-*-*-120-*-*-*-*-iso8859-1
5x7          -misc-fixed-medium-r-normal--7-70-75-75-c-50-iso8859-1
5x8          -misc-fixed-medium-r-normal--8-80-75-75-c-50-iso8859-1
6x9          -misc-fixed-medium-r-normal--9-90-75-75-c-60-iso8859-1
6x10         -misc-fixed-medium-r-normal--10-100-75-75-c-60-iso8859-1
6x12         -misc-fixed-medium-r-semicondensed--12-110-75-75-c-60-iso8859-1
6x13         -misc-fixed-medium-r-semicondensed--13-120-75-75-c-60-iso8859-1
6x13bold     -misc-fixed-bold-r-semicondensed--13-120-75-75-c-60-iso8859-1
7x13         -misc-fixed-medium-r-normal--13-120-75-75-c-70-iso8859-1
7x13bold     -misc-fixed-bold-r-normal--13-120-75-75-c-70-iso8859-1
7x13euro     -misc-fixed-medium-r-normal--13-120-75-75-c-70-iso8859-15
7x13eurobold -misc-fixed-bold-r-normal--13-120-75-75-c-70-iso8859-15
7x14         -misc-fixed-medium-r-normal--14-130-75-75-c-70-iso8859-1
7x14bold     -misc-fixed-bold-r-normal--14-130-75-75-c-70-iso8859-1
8x13         -misc-fixed-medium-r-normal--13-120-75-75-c-80-iso8859-1
8x13bold     -misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1
8x16         -sony-fixed-medium-r-normal--16-120-100-100-c-80-iso8859-1
9x15         -misc-fixed-medium-r-normal--15-140-75-75-c-90-iso8859-1
9x15bold     -misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1
10x20        -misc-fixed-medium-r-normal--20-200-75-75-c-100-iso8859-1
12x24        -sony-fixed-medium-r-normal--24-170-100-100-c-120-iso8859-1
nil2         -misc-nil-medium-r-normal--2-20-75-75-c-10-misc-fontspecific

heb6x13      -misc-fixed-medium-r-semicondensed--13-120-75-75-c-60-iso8859-8
heb8x13      -misc-fixed-medium-r-normal--13-120-75-75-c-80-iso8859-8

k14          -misc-fixed-medium-r-normal--14-*-*-*-*-*-jisx0208.1983-0
a14          -misc-fixed-medium-r-normal--14-*-*-*-*-*-iso8859-1
r14          -misc-fixed-medium-r-normal--14-*-*-*-*-*-jisx0201.1976-0
rk14         -misc-fixed-medium-r-normal--14-*-*-*-*-*-jisx0201.1976-0
r16          -sony-fixed-medium-r-normal--16-*-*-*-*-*-jisx0201.1976-0
rk16         -sony-fixed-medium-r-normal--16-*-*-*-*-*-jisx0201.1976-0
r24          -sony-fixed-medium-r-normal--24-*-*-*-*-*-jisx0201.1976-0
rk24         -sony-fixed-medium-r-normal--24-*-*-*-*-*-jisx0201.1976-0
kana14       -misc-fixed-medium-r-normal--14-*-*-*-*-*-jisx0201.1976-0
8x16kana     -sony-fixed-medium-r-normal--16-120-100-100-c-80-jisx0201.1976-0
8x16romankana -sony-fixed-medium-r-normal--16-120-100-100-c-80-jisx0201.1976-0
12x24kana     -sony-fixed-medium-r-normal--24-170-100-100-c-120-jisx0201.1976-0
12x24romankana -sony-fixed-medium-r-normal--24-170-100-100-c-120-jisx0201.1976-0
kanji16      -jis-fixed-medium-r-normal--16-*-*-*-*-*-jisx0208.1983-0
kanji24      -jis-fixed-medium-r-normal--24-*-*-*-*-*-jisx0208.1983-0

hanzigb16st "-isas-song ti-medium-r-normal--16-160-72-72-c-160-gb2312.1980-0"
hanzigb24st "-isas-song ti-medium-r-normal--24-240-72-72-c-240-gb2312.1980-0"
hanzigb16fs "-isas-fangsong ti-medium-r-normal--16-160-72-72-c-160-gb2312.1980-0"

olcursor   "-sun-open look cursor-----12-120-75-75-p-160-sunolcursor-1"
olglyph-10 "-sun-open look glyph-----10-100-75-75-p-101-sunolglyph-1"
olglyph-12 "-sun-open look glyph-----12-120-75-75-p-113-sunolglyph-1"
olglyph-14 "-sun-open look glyph-----14-140-75-75-p-128-sunolglyph-1"
olglyph-19 "-sun-open look glyph-----19-190-75-75-p-154-sunolglyph-1"
*)
