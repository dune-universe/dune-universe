module Lowlevel = Lowlevel

(** Available line types, see also {{: https://gr-framework.org/linetypes.html} GR Line Types} *)
type linetype =
  | SOLID  (** Solid line *)
  | DASHED  (** Dashed line *)
  | DOTTED  (** Dotted line *)
  | DASHED_DOTTED  (** Dashed-dotted line *)
  | DASH_2_DOT  (** Sequence of one dash followed by two dots *)
  | DASH_3_DOT  (** Sequence of one dash followed by three dots *)
  | LONG_DASH  (** Sequence of long dashes *)
  | LONG_SHORT_DASH  (** Sequence of a long dash followed by a short dash *)
  | SPACED_DASH  (** Sequence of dashes double spaced *)
  | SPACED_DOT  (** Sequence of dots double spaced *)
  | DOUBLE_DOT  (** Sequence of pairs of dots *)
  | TRIPLE_DOT  (** Sequence of groups of three dots *)

let int_of_linetype = function
  | SOLID -> 1
  | DASHED -> 2
  | DOTTED -> 3
  | DASHED_DOTTED -> 4
  | DASH_2_DOT -> -1
  | DASH_3_DOT -> -2
  | LONG_DASH -> -3
  | LONG_SHORT_DASH -> -4
  | SPACED_DASH -> -5
  | SPACED_DOT -> -6
  | DOUBLE_DOT -> -7
  | TRIPLE_DOT -> -8

(* let linetype_of_int = function
   | 1 -> SOLID
   | 2 -> DASHED
   | 3 -> DOTTED
   | 4 -> DASHED_DOTTED
   | -1 -> DASH_2_DOT
   | -2 -> DASH_3_DOT
   | -3 -> LONG_DASH
   | -4 -> LONG_SHORT_DASH
   | -5 -> SPACED_DASH
   | -6 -> SPACED_DOT
   | -7 -> DOUBLE_DOT
   | -8 -> TRIPLE_DOT
   | d -> failwith @@ "Error when inferring line type. Got " ^ string_of_int d *)

(** Available marker types, see also {{: https://gr-framework.org/markertypes.html} GR Marker Types} *)
type markertype =
  | DOT  (** Smallest displayable dot *)
  | PLUS  (** Plus sign *)
  | ASTERISK  (** Asterisk *)
  | CIRCLE  (** Hollow circle *)
  | DIAGONAL_CROSS  (** Diagonal cross *)
  | SOLID_CIRCLE  (** Filled circle *)
  | TRIANGLE_UP  (** Hollow triangle pointing upward *)
  | SOLID_TRI_UP  (** Filled triangle pointing upward *)
  | TRIANGLE_DOWN  (** Hollow triangle pointing downward *)
  | SOLID_TRI_DOWN  (** Filled triangle pointing downward *)
  | SQUARE  (** Hollow square *)
  | SOLID_SQUARE  (** Filled square *)
  | BOWTIE  (** Hollow bowtie *)
  | SOLID_BOWTIE  (** Filled bowtie *)
  | HGLASS  (** Hollow hourglass *)
  | SOLID_HGLASS  (** Filled hourglass *)
  | DIAMOND  (** Hollow diamond *)
  | SOLID_DIAMOND  (** Filled Diamond *)
  | STAR  (** Hollow star *)
  | SOLID_STAR  (** Filled Star *)
  | TRI_UP_DOWN  (** Hollow triangles pointing up and down overlaid *)
  | SOLID_TRI_RIGHT  (** Filled triangle point right *)
  | SOLID_TRI_LEFT  (** Filled triangle pointing left *)
  | HOLLOW_PLUS  (** Hollow plus sign *)
  | SOLID_PLUS  (** Solid plus sign *)
  | PENTAGON  (** Pentagon *)
  | HEXAGON  (** Hexagon *)
  | HEPTAGON  (** Heptagon *)
  | OCTAGON  (** Octagon *)
  | STAR_4  (** 4-pointed star *)
  | STAR_5  (** 5-pointed star (pentagram) *)
  | STAR_6  (** 6-pointed star (hexagram) *)
  | STAR_7  (** 7-pointed star (heptagram) *)
  | STAR_8  (** 8-pointed star (octagram) *)
  | VLINE  (** verical line *)
  | HLINE  (** horizontal line *)
  | OMARK  (** o-mark *)

let int_of_markertype = function
  | DOT -> 1
  | PLUS -> 2
  | ASTERISK -> 3
  | CIRCLE -> 4
  | DIAGONAL_CROSS -> 5
  | SOLID_CIRCLE -> -1
  | TRIANGLE_UP -> -2
  | SOLID_TRI_UP -> -3
  | TRIANGLE_DOWN -> -4
  | SOLID_TRI_DOWN -> -5
  | SQUARE -> -6
  | SOLID_SQUARE -> -7
  | BOWTIE -> -8
  | SOLID_BOWTIE -> -9
  | HGLASS -> -10
  | SOLID_HGLASS -> -11
  | DIAMOND -> -12
  | SOLID_DIAMOND -> -13
  | STAR -> -14
  | SOLID_STAR -> -15
  | TRI_UP_DOWN -> -16
  | SOLID_TRI_RIGHT -> -17
  | SOLID_TRI_LEFT -> -18
  | HOLLOW_PLUS -> -19
  | SOLID_PLUS -> -20
  | PENTAGON -> -21
  | HEXAGON -> -22
  | HEPTAGON -> -23
  | OCTAGON -> -24
  | STAR_4 -> -25
  | STAR_5 -> -26
  | STAR_6 -> -27
  | STAR_7 -> -28
  | STAR_8 -> -29
  | VLINE -> -30
  | HLINE -> -31
  | OMARK -> -32

(* let markertype_of_int = function
   | 1 -> DOT
   | 2 -> PLUS
   | 3 -> ASTERISK
   | 4 -> CIRCLE
   | 5 -> DIAGONAL_CROSS
   | -1 -> SOLID_CIRCLE
   | -2 -> TRIANGLE_UP
   | -3 -> SOLID_TRI_UP
   | -4 -> TRIANGLE_DOWN
   | -5 -> SOLID_TRI_DOWN
   | -6 -> SQUARE
   | -7 -> SOLID_SQUARE
   | -8 -> BOWTIE
   | -9 -> SOLID_BOWTIE
   | -10 -> HGLASS
   | -11 -> SOLID_HGLASS
   | -12 -> DIAMOND
   | -13 -> SOLID_DIAMOND
   | -14 -> STAR
   | -15 -> SOLID_STAR
   | -16 -> TRI_UP_DOWN
   | -17 -> SOLID_TRI_RIGHT
   | -18 -> SOLID_TRI_LEFT
   | -19 -> HOLLOW_PLUS
   | -20 -> SOLID_PLUS
   | -21 -> PENTAGON
   | -22 -> HEXAGON
   | -23 -> HEPTAGON
   | -24 -> OCTAGON
   | -25 -> STAR_4
   | -26 -> STAR_5
   | -27 -> STAR_6
   | -28 -> STAR_7
   | -29 -> STAR_8
   | -30 -> VLINE
   | -31 -> HLINE
   | -32 -> OMARK
   | d -> failwith @@ "Error when inferring marker type. Got " ^ string_of_int d *)

type scale_options =
  | OPTION_X_LOG  (** Logarithmic X-axis *)
  | OPTION_Y_LOG  (** Logarithmic Y-axis *)
  | OPTION_Z_LOG  (** Logarithmic Z-axis *)
  | OPTION_FLIP_X  (** Flip X-axis *)
  | OPTION_FLIP_Y  (** Flip Y-axis *)
  | OPTION_FLIP_Z  (** Flip Z-axis *)

let int_of_scale_options opts =
  let int_of = function
    | OPTION_X_LOG -> 1
    | OPTION_Y_LOG -> 2
    | OPTION_Z_LOG -> 4
    | OPTION_FLIP_X -> 8
    | OPTION_FLIP_Y -> 16
    | OPTION_FLIP_Z -> 32
  in
  List.fold_left (fun acc s -> acc + int_of s) 0 opts

type spline_algo =
  | GeneralizedCrossValidatedSmoothing
  | InterpolatingNaturalCubic
  | CubicBSpline

let int_of_spline_algo = function
  | GeneralizedCrossValidatedSmoothing -> 1
  | InterpolatingNaturalCubic -> 0
  | CubicBSpline -> -1

(** Available fonts, see also {{: https://gr-framework.org/fonts.html} GR Font list} *)
type font =
  | TIMES_ROMAN
  | TIMES_ITALIC
  | TIMES_BOLD
  | TIMES_BOLDITALIC
  | HELVETICA
  | HELVETICA_OBLIQUE
  | HELVETICA_BOLD
  | HELVETICA_BOLDOBLIQUE
  | COURIER
  | COURIER_OBLIQUE
  | COURIER_BOLD
  | COURIER_BOLDOBLIQUE
  | SYMBOL
  | BOOKMAN_LIGHT
  | BOOKMAN_LIGHTITALIC
  | BOOKMAN_DEMI
  | BOOKMAN_DEMIITALIC
  | NEWCENTURYSCHLBK_ROMAN
  | NEWCENTURYSCHLBK_ITALIC
  | NEWCENTURYSCHLBK_BOLD
  | NEWCENTURYSCHLBK_BOLDITALIC
  | AVANTGARDE_BOOK
  | AVANTGARDE_BOOKOBLIQUE
  | AVANTGARDE_DEMI
  | AVANTGARDE_DEMIOBLIQUE
  | PALATINO_ROMAN
  | PALATINO_ITALIC
  | PALATINO_BOLD
  | PALATINO_BOLDITALIC
  | ZAPFCHANCERY_MEDIUMITALIC
  | ZAPFDINGBATS

let int_of_font = function
  | TIMES_ROMAN -> 101
  | TIMES_ITALIC -> 102
  | TIMES_BOLD -> 103
  | TIMES_BOLDITALIC -> 104
  | HELVETICA -> 105
  | HELVETICA_OBLIQUE -> 106
  | HELVETICA_BOLD -> 107
  | HELVETICA_BOLDOBLIQUE -> 108
  | COURIER -> 109
  | COURIER_OBLIQUE -> 110
  | COURIER_BOLD -> 111
  | COURIER_BOLDOBLIQUE -> 112
  | SYMBOL -> 113
  | BOOKMAN_LIGHT -> 114
  | BOOKMAN_LIGHTITALIC -> 115
  | BOOKMAN_DEMI -> 116
  | BOOKMAN_DEMIITALIC -> 117
  | NEWCENTURYSCHLBK_ROMAN -> 118
  | NEWCENTURYSCHLBK_ITALIC -> 119
  | NEWCENTURYSCHLBK_BOLD -> 120
  | NEWCENTURYSCHLBK_BOLDITALIC -> 121
  | AVANTGARDE_BOOK -> 122
  | AVANTGARDE_BOOKOBLIQUE -> 123
  | AVANTGARDE_DEMI -> 124
  | AVANTGARDE_DEMIOBLIQUE -> 125
  | PALATINO_ROMAN -> 126
  | PALATINO_ITALIC -> 127
  | PALATINO_BOLD -> 128
  | PALATINO_BOLDITALIC -> 129
  | ZAPFCHANCERY_MEDIUMITALIC -> 130
  | ZAPFDINGBATS -> 131

type text_precision =
  | STRING  (** String precision (higher quality) *)
  | CHAR  (** Character precision (medium quality) *)
  | STROKE  (** Stroke precision (lower quality) *)

let int_of_text_precision = function STRING -> 0 | CHAR -> 1 | STROKE -> 2

type text_path_direction =
  | RIGHT  (** left-to-right *)
  | LEFT  (** right-to-left *)
  | UP  (** downside-up *)
  | DOWN  (** upside-down *)

let int_of_text_path_direction = function
  | RIGHT -> 0
  | LEFT -> 1
  | UP -> 2
  | DOWN -> 3

type text_halign =
  | NORMAL
  | LEFT  (** Left justify *)
  | CENTER  (** Center justify *)
  | RIGHT  (** Right justify *)

let int_of_text_halign = function
  | NORMAL -> 0
  | LEFT -> 1
  | CENTER -> 2
  | RIGHT -> 3

type text_valign =
  | NORMAL
  | TOP  (** Align with the top of the characters *)
  | CAP  (** Aligned with the cap of the characters *)
  | HALF  (** Aligned with the half line of the characters *)
  | BASE  (** Aligned with the base line of the characters *)
  | BOTTOM  (** Aligned with the bottom line of the characters *)

let int_of_text_valign = function
  | NORMAL -> 0
  | TOP -> 1
  | CAP -> 2
  | HALF -> 3
  | BASE -> 4
  | BOTTOM -> 5

type pattern_style = int
(** Pattern style, see also {{: https://gr-framework.org/patterns.html} GR Fill Patterns and Hatches}  *)

let pattern_style n =
  if n > 0 && n < 109 then n else failwith "pattern_style out of range"

type hatch_style = int
(** Hatch style, see also {{: https://gr-framework.org/patterns.html} GR Fill Patterns and Hatches}  *)

let hatch_style n =
  if n > 1 && n < 11 then n else failwith "hatch_style out of range"

type fill_style =
  | HOLLOW  (** No filling. Just draw the bounding polyline *)
  | SOLID  (** Fill the interior of the polygon using the fill color index *)
  | PATTERN of pattern_style
      (** Fill the interior of the polygon using the style index as a pattern index *)
  | HATCH of hatch_style
      (** Fill the interior of the polygon using the style index as a cross-hatched style *)

let int_of_fill_style = function
  | HOLLOW -> 0
  | SOLID -> 1
  | PATTERN _ -> 2
  | HATCH _ -> 3

(** Color Maps, see also {{: https://gr-framework.org/colormaps.html} GR Color Maps} *)
type color_map =
  | Uniform
  | Temperature
  | Grayscale
  | Glowing
  | Rainbowlike
  | Geologic
  | Greenscale
  | Cyanscale
  | Bluescale
  | Magentascale
  | Redscale
  | Flame
  | Brownscale
  | Pilatus
  | Autumn
  | Bone
  | Cool
  | Copper
  | Gray
  | Hot
  | Hsv
  | Jet
  | Pink
  | Spectral
  | Spring
  | Summer
  | Winter
  | Gist_Earth
  | Gist_Heat
  | Gist_Ncar
  | Gist_Rainbow
  | Gist_Stern
  | Afmhot
  | Brg
  | Bwr
  | Coolwarm
  | Cmrmap
  | Cubehelix
  | Gnuplot
  | Gnuplot2
  | Ocean
  | Rainbow
  | Seismic
  | Terrain
  | Viridis
  | Inferno
  | Plasma
  | Magma

let int_of_color_map = function
  | Uniform -> 0
  | Temperature -> 1
  | Grayscale -> 2
  | Glowing -> 3
  | Rainbowlike -> 4
  | Geologic -> 5
  | Greenscale -> 6
  | Cyanscale -> 7
  | Bluescale -> 8
  | Magentascale -> 9
  | Redscale -> 10
  | Flame -> 11
  | Brownscale -> 12
  | Pilatus -> 13
  | Autumn -> 14
  | Bone -> 15
  | Cool -> 16
  | Copper -> 17
  | Gray -> 18
  | Hot -> 19
  | Hsv -> 20
  | Jet -> 21
  | Pink -> 22
  | Spectral -> 23
  | Spring -> 24
  | Summer -> 25
  | Winter -> 26
  | Gist_Earth -> 27
  | Gist_Heat -> 28
  | Gist_Ncar -> 29
  | Gist_Rainbow -> 30
  | Gist_Stern -> 31
  | Afmhot -> 32
  | Brg -> 33
  | Bwr -> 34
  | Coolwarm -> 35
  | Cmrmap -> 36
  | Cubehelix -> 37
  | Gnuplot -> 38
  | Gnuplot2 -> 39
  | Ocean -> 40
  | Rainbow -> 41
  | Seismic -> 42
  | Terrain -> 43
  | Viridis -> 44
  | Inferno -> 45
  | Plasma -> 46
  | Magma -> 47

type surface_options =
  | LINES  (** Use X Y polylines to denote the surface *)
  | MESH  (** Use a wire grid to denote the surface *)
  | FILLED_MESH  (** Applies an opaque grid to the surface *)
  | Z_SHADED_MESH  (** Applies Z-value shading to the surface *)
  | COLORED_MESH  (** Applies a colored grid to the surface *)
  | CELL_ARRAY
      (** Applies a grid of individually-colored cells to the surface *)
  | SHADED_MESH  (** Applies light source shading to the 3-D surface *)

let int_of_surface_options = function
  | LINES -> 0
  | MESH -> 1
  | FILLED_MESH -> 2
  | Z_SHADED_MESH -> 3
  | COLORED_MESH -> 4
  | CELL_ARRAY -> 5
  | SHADED_MESH -> 6

module Workstation = struct
  type id = W of int

  (** Available workstation types, see also {{: https://gr-framework.org/workstations.html} GR Workstation Types} *)
  type workstation_type =
    | WISS  (** Workstation Independent Segment ptr Storage *)
    | WinGDI  (** Windows ptr GDI *)
    | PS_1  (**PostScript (b/w \@-> color) *)
    | PS_2  (**PostScript (b/w \@-> color) *)
    | PS_3  (**PostScript (b/w \@-> color) *)
    | PS_4  (**PostScript (b/w \@-> color) *)
    | PDFPlain  (** Portable Document Format ptr plain *)
    | PDFCompressed  (** Portable Document Format ptr compressed *)
    | X_1  (** X ptr Windows *)
    | X_2  (** X ptr Windows *)
    | X_3  (** X ptr Windows *)
    | X_4  (** X ptr Windows *)
    | SunRF  (** Sun Raster file (RF) *)
    | GIF87  (** Graphics Interchange Format ptr GIF87 *)
    | GIF89  (**Graphics Interchange Format ptr GIF89 *)
    | MotifUIL  (** Motif User Interface Language (UIL) *)
    | BMP  (** Windows Bitmap (BMP) *)
    | JPEG  (** JPEG image ptr file *)
    | PNG  (** Portable Network Graphics file (PNG) *)
    | TIFF  (** Tagged Image File Format (TIFF) *)
    | Gtk  (** ptr Gtk *)
    | Wx  (** ptr wxWidgets *)
    | Qt4  (** ptr Qt4 *)
    | SVG  (** Scaleable Vector Graphics (SVG) *)
    | WMF  (** Windows ptr Metafile *)
    | Quartz  (** ptr Quartz *)
    | Sock  (** Socket ptr driver *)
    | ZMQ  (** 0MQ ptr driver *)
    | OGL  (** ptr OpenGL *)

  let int_of_workstation_type = function
    | WISS -> 5
    | WinGDI -> 41
    | PS_1 -> 61
    | PS_2 -> 62
    | PS_3 -> 63
    | PS_4 -> 64
    | PDFPlain -> 101
    | PDFCompressed -> 102
    | X_1 -> 210
    | X_2 -> 211
    | X_3 -> 213
    | X_4 -> 212
    | SunRF -> 214
    | GIF87 -> 215
    | GIF89 -> 218
    | MotifUIL -> 216
    | BMP -> 320
    | JPEG -> 321
    | PNG -> 322
    | TIFF -> 323
    | Gtk -> 371
    | Wx -> 380
    | Qt4 -> 381
    | SVG -> 382
    | WMF -> 390
    | Quartz -> 400
    | Sock -> 410
    | ZMQ -> 415
    | OGL -> 420

  let wid id = W id

  let open' (W id) conn typ =
    Lowlevel.openws id conn (int_of_workstation_type typ)

  let close (W id) = Lowlevel.closews id

  let activate (W id) = Lowlevel.activatews id

  let deactivate (W id) = Lowlevel.deactivatews id

  let clear = Lowlevel.clearws

  let update = Lowlevel.updatews

  let set_window = Lowlevel.setwswindow

  let set_viewport = Lowlevel.setwsviewport

  let copy_segment = Lowlevel.copysegws

  let redraw_segment = Lowlevel.redrawsegws
end

module Gks = struct
  let emergency_close = Lowlevel.emergencyclosegks

  let update = Lowlevel.updategks
end

module State = struct
  let save () = Lowlevel.savestate ()

  let restore () = Lowlevel.restorestate ()

  let with_sandbox f =
    save ();
    Fun.protect ~finally:restore f
end

let set_window = Lowlevel.setwindow

let set_viewport = Lowlevel.setviewport

let select_transformation = Lowlevel.selntran

let clip c = Lowlevel.setclip (if c then 1 else 2)

type segment = int

let segment i = i

let create_segment = Lowlevel.createseg

let set_segment_transform = Lowlevel.setsegtran

let close_segment = Lowlevel.closeseg

let set_space = Lowlevel.setspace

let set_linetype lt = lt |> int_of_linetype |> Lowlevel.setlinetype

let set_linewidth = Lowlevel.setlinewidth

let set_linecolorindex = function
  | c when c >= 0 && c < 1256 -> Lowlevel.setlinecolorind c
  | c ->
      failwith @@ "Color index must be in the range [0, 1256]. Got "
      ^ string_of_int c

let set_markertype mt = mt |> int_of_markertype |> Lowlevel.setmarkertype

let set_markersize = Lowlevel.setmarkersize

let set_markercolorindex = function
  | c when c >= 0 && c < 1256 -> Lowlevel.setmarkercolorind c
  | c ->
      failwith @@ "Color index must be in the range [0, 1256]. Got "
      ^ string_of_int c

let set_arrowstyle s =
  if s < 0 || s > 18 then
    failwith @@ Printf.sprintf "Only styles 1..18 are supported. Got %d" s;
  Lowlevel.setarrowstyle s

let set_arrowsize = Lowlevel.setarrowsize

let set_text_font_prec ?(precision = STRING) font =
  Lowlevel.settextfontprec (int_of_font font) (int_of_text_precision precision)

let set_char_expand_factor = Lowlevel.setcharexpan

let set_text_colorindex = function
  | c when c >= 0 && c < 1256 -> Lowlevel.settextcolorind c
  | c ->
      failwith @@ "Color index must be in the range [0, 1256]. Got "
      ^ string_of_int c

let set_char_height = Lowlevel.setcharheight

let set_char_up (x, y) = Lowlevel.setcharup x y

let set_char_space = Lowlevel.setcharspace

let set_text_path direction =
  Lowlevel.settextpath (int_of_text_path_direction direction)

let set_text_align : text_halign option -> text_valign option -> unit =
 fun horizontal vertical ->
  let horizontal = Option.value ~default:NORMAL horizontal in
  let vertical = Option.value ~default:NORMAL vertical in
  Lowlevel.settextalign
    (int_of_text_halign horizontal)
    (int_of_text_valign vertical)

let set_fill_interior_style style =
  Lowlevel.setfillintstyle (int_of_fill_style style);
  match style with
  | PATTERN pat -> Lowlevel.setfillstyle pat
  | HATCH hat -> Lowlevel.setfillstyle hat
  | _ -> ()

let set_fill_colorindex = function
  | c when c >= 0 && c < 1256 -> Lowlevel.setfillcolorind c
  | c ->
      failwith @@ "Color index must be in the range [0, 1256]. Got "
      ^ string_of_int c

let set_color_representation index (red, green, blue) =
  if index < 0 || index >= 1256 then
    failwith @@ "Color index must be in the range [0, 1256]. Got "
    ^ string_of_int index;
  if
    (red < 0.0 || red > 1.0)
    || (green < 0.0 || green > 1.0)
    || blue < 0.0 || blue > 1.0
  then
    failwith
    @@ Printf.sprintf
         "Color values must be in the range [0.0, 1.0]. Got: (%f, %f, %f)" red
         green blue;
  Lowlevel.setcolorrep index red green blue

let set_colormap cmap = Lowlevel.setcolormap (int_of_color_map cmap)

let set_scale scale = Lowlevel.setscale (int_of_scale_options scale)

let set_shadow (offsetx, offsety) blur = Lowlevel.setshadow offsetx offsety blur

let set_transparency alpha =
  if alpha < 0.0 && alpha > 1.0 then
    failwith @@ "Alpha index must be between 0.0 and 1.0";
  Lowlevel.settransparency alpha

let set_coord_transform transform =
  let transform =
    match Bigarray.Genarray.dims transform with
    | [| 3; 2 |] -> transform
    | [| 2; 3 |] -> failwith "Need a 3x2 array, got a 2x3 array"
    | _ -> failwith "Need a 3x2 array but got something different"
  in
  Lowlevel.setcoordxform Ctypes.(bigarray_start genarray transform)

module Graphics = struct
  type t = string

  let beging = Lowlevel.begingraphics

  let endg = Lowlevel.endgraphics

  let get : unit -> t = Lowlevel.getgraphics

  let draw = Lowlevel.drawgraphics

  let import = Lowlevel.importgraphics
end

let polyline ?linetype ?linewidth ?coloridx x y =
  State.with_sandbox (fun () ->
      Option.iter set_linetype linetype;
      Option.iter set_linewidth linewidth;
      Option.iter set_linecolorindex coloridx;
      let n, x, y = Lowlevel.get_size_and_pointers x y in
      Lowlevel.polyline n x y)

let polyline3d ?linetype ?linewidth ?coloridx x y z =
  State.with_sandbox (fun () ->
      Option.iter set_linetype linetype;
      Option.iter set_linewidth linewidth;
      Option.iter set_linecolorindex coloridx;
      let n, x, y = Lowlevel.get_size_and_pointers x y in
      let _nz, z = Lowlevel.get_size_and_pointer z in
      (* TODO: Check z dimension *)
      Lowlevel.polyline3d n x y z)

let polymarker ?markertype ?markersize ?coloridx x y =
  State.with_sandbox (fun () ->
      Option.iter set_markertype markertype;
      Option.iter set_markersize markersize;
      Option.iter set_markercolorindex coloridx;
      let n, x, y = Lowlevel.get_size_and_pointers x y in
      Lowlevel.polymarker n x y)

let polymarker3d ?markertype ?markersize ?coloridx x y z =
  State.with_sandbox (fun () ->
      Option.iter set_markertype markertype;
      Option.iter set_markersize markersize;
      Option.iter set_markercolorindex coloridx;
      let n, x, y = Lowlevel.get_size_and_pointers x y in
      let _nz, z = Lowlevel.get_size_and_pointer z in
      (* TODO: Check z dimension *)
      Lowlevel.polymarker3d n x y z)

let text = Lowlevel.text

let fillarea x y =
  let n, x', y' = Lowlevel.get_size_and_pointers x y in
  Lowlevel.fillarea n x' y'

let cellarray (xmin, xmax) (ymin, ymax) (dimx, dimy) (scol, srow) (ncol, nrow)
    colors =
  let color' = Ctypes.(bigarray_start genarray colors) in
  Lowlevel.cellarray xmin xmax ymin ymax dimx dimy scol srow ncol nrow color'

(* let gdp = ... (* No idea what this does... *) *)

let spline ?linetype ?linewidth ?coloridx x y m algo =
  State.with_sandbox (fun () ->
      Option.iter set_linetype linetype;
      Option.iter set_linewidth linewidth;
      Option.iter set_linecolorindex coloridx;
      let n, x', y' = Lowlevel.get_size_and_pointers x y in
      Lowlevel.spline n x' y' m (int_of_spline_algo algo))

let gridit x y z (nx, ny) =
  let x' = Bigarray.(Genarray.create float64 c_layout [| nx |]) in
  let y' = Bigarray.(Genarray.create float64 c_layout [| ny |]) in
  let z' = Bigarray.(Genarray.create float64 c_layout [| nx * ny |]) in
  let n, x, y = Lowlevel.get_size_and_pointers x y in
  let nz, z = Lowlevel.get_size_and_pointer z in
  if nz <> n then
    failwith
    @@ Printf.sprintf "Expected arrays with dimensions n, n, n. Got %d, %d, %d"
         n n nz;
  Lowlevel.gridit n x y z nx ny
    Ctypes.(bigarray_start genarray x')
    Ctypes.(bigarray_start genarray y')
    Ctypes.(bigarray_start genarray z');
  (x', y', z')

let tex_text (x, y) text = Lowlevel.textext x y text

let math_tex (x, y) tex = Lowlevel.mathtex x y tex

let axes ?(scale = []) ?linetype ?linewidth ?coloridx ?(origin = (0.0, 0.0))
    ?(major = (1, 1)) ?(tick_size = -0.01) x_tick y_tick =
  State.with_sandbox (fun () ->
      if scale <> [] then set_scale scale |> ignore;
      Option.iter set_linetype linetype;
      Option.iter set_linewidth linewidth;
      Option.iter set_linecolorindex coloridx;
      let x_org, y_org = origin in
      let major_x, major_y = major in
      Lowlevel.axes x_tick y_tick x_org y_org major_x major_y tick_size)

let axes_labels ?(scale = []) ?linetype ?linewidth ?coloridx
    ?(origin = (0.0, 0.0)) ?(major = (1, 1)) ?(tick_size = -0.01)
    (fpx : float -> float -> string -> float -> unit)
    (fpy : float -> float -> string -> float -> unit) x_tick y_tick =
  State.with_sandbox (fun () ->
      if scale <> [] then
        Lowlevel.setscale (int_of_scale_options scale) |> ignore;
      Option.iter set_linetype linetype;
      Option.iter set_linewidth linewidth;
      Option.iter set_linecolorindex coloridx;
      let x_org, y_org = origin in
      let major_x, major_y = major in
      Lowlevel.axeslbl x_tick y_tick x_org y_org major_x major_y tick_size fpx
        fpy)

let axes3d ?(scale = []) ?linetype ?linewidth ?coloridx
    ?(origin = (0.0, 0.0, 0.0)) ?(major = (1, 1, 1)) ?(tick_size = -0.01) x_tick
    y_tick z_tick =
  State.with_sandbox (fun () ->
      if scale <> [] then set_scale scale |> ignore;
      Option.iter set_linetype linetype;
      Option.iter set_linewidth linewidth;
      Option.iter set_linecolorindex coloridx;
      let x_org, y_org, z_org = origin in
      let major_x, major_y, major_z = major in
      Lowlevel.axes3d x_tick y_tick z_tick x_org y_org z_org major_x major_y
        major_z tick_size)

let surface ?(options = LINES) x y z =
  let nx, x = Lowlevel.get_size_and_pointer x in
  let ny, y = Lowlevel.get_size_and_pointer y in
  let nz, z = Lowlevel.get_size_and_pointer z in
  if nz <> nx * ny then
    failwith
    @@ Printf.sprintf
         "Expected arrays with dimensions n, n', n*n'. Got %d, %d, %d" nx ny nz;
  Lowlevel.surface nx ny x y z (int_of_surface_options options)

let contour ?(major_h = 0) x y h z =
  (* TODO: validate z *)
  let nx, x = Lowlevel.get_size_and_pointer x in
  let ny, y = Lowlevel.get_size_and_pointer y in
  let nh, h = Lowlevel.get_size_and_pointer h in
  let _nz, z = Lowlevel.get_size_and_pointer z in
  Lowlevel.contour nx ny nh x y h z major_h

let contourf ?(major_h = 0) x y h z =
  (* TODO: validate z *)
  let nx, x = Lowlevel.get_size_and_pointer x in
  let ny, y = Lowlevel.get_size_and_pointer y in
  let nh, h = Lowlevel.get_size_and_pointer h in
  let _nz, z = Lowlevel.get_size_and_pointer z in
  Lowlevel.contourf nx ny nh x y h z major_h

let grid ?(scale = []) ?linetype ?linewidth ?coloridx ?(origin = (0.0, 0.0))
    ?(major = (1, 1)) x_tick y_tick =
  State.with_sandbox (fun () ->
      if scale <> [] then set_scale scale |> ignore;
      Option.iter set_linetype linetype;
      Option.iter set_linewidth linewidth;
      Option.iter set_linecolorindex coloridx;
      let x_org, y_org = origin in
      let major_x, major_y = major in
      Lowlevel.grid x_tick y_tick x_org y_org major_x major_y)

let grid3d ?(scale = []) ?linetype ?linewidth ?coloridx
    ?(origin = (0.0, 0.0, 0.0)) ?(major = (1, 1, 1)) x_tick y_tick z_tick =
  State.with_sandbox (fun () ->
      if scale <> [] then set_scale scale |> ignore;
      Option.iter set_linetype linetype;
      Option.iter set_linewidth linewidth;
      Option.iter set_linecolorindex coloridx;
      let x_org, y_org, z_org = origin in
      let major_x, major_y, major_z = major in
      Lowlevel.grid3d x_tick y_tick z_tick x_org y_org z_org major_x major_y
        major_z)

let vertical_errorbars x y el eu =
  let n, x, y = Lowlevel.get_size_and_pointers x y in
  let ne, el, eu = Lowlevel.get_size_and_pointers el eu in
  if n <> ne then
    failwith
    @@ Printf.sprintf "Expected arrays of the same dimensions. Got: %d, %d" n ne;
  Lowlevel.verrorbars n x y el eu

let horizontal_errorbars x y el eu =
  let n, x, y = Lowlevel.get_size_and_pointers x y in
  let ne, el, eu = Lowlevel.get_size_and_pointers el eu in
  if n <> ne then
    failwith
    @@ Printf.sprintf "Expected arrays of the same dimensions. Got: %d, %d" n ne;
  Lowlevel.herrorbars n x y el eu

let titles3d = Lowlevel.titles3d

let tricontour x y z levels =
  let nx, x = Lowlevel.get_size_and_pointer x in
  let _ny, y = Lowlevel.get_size_and_pointer y in
  let _nz, z = Lowlevel.get_size_and_pointer z in
  let nlevels, levels = Lowlevel.get_size_and_pointer levels in
  Lowlevel.tricontour nx x y z nlevels levels

(* (* TODO: I don't know what this function does... *)
   let hexbin = foreign "gr_hexbin" (int @-> ptr double @-> ptr double @-> int @-> returning int)
*)

let colorbar () = Lowlevel.colorbar ()

(*
(* TODO: postponed *)
let hsvtorgb = foreign "gr_hsvtorgb" (double @-> double @-> double @-> ptr double@-> ptr double @-> ptr double @-> returning void)
*)

let tick = Lowlevel.tick

module Print = struct
  let validate path =
    if
      not
      @@ List.fold_left
           (fun acc suffix -> acc || Filename.check_suffix path suffix)
           false
           [
             ".ps";
             ".eps";
             ".pdf";
             ".bmp";
             ".jpeg";
             ".jpg";
             ".png";
             ".tiff";
             ".tif";
             ".svg";
             ".wmf";
             ".mp4";
             ".webm";
             ".ogg";
           ]
    then failwith @@ Printf.sprintf "Unsupported file type: %s" path

  let beginp path =
    validate path;
    Lowlevel.beginprint path

  let endp = Lowlevel.endprint

  let beginp_extended path mode format orientation =
    validate path;
    let mode =
      match mode with `Color -> "Color" | `GreyScale -> "GreyScale"
    in
    let orientation =
      match orientation with
      | `Landscape -> "Landscape"
      | `Portrait -> "Portrait"
    in
    let format =
      match format with
      | `A4 -> "A4"
      | `B5 -> "B5"
      | `Letter -> "Letter"
      | `Legal -> "Legal"
      | `Executive -> "Executive"
      | `A0 -> "A0"
      | `A1 -> "A1"
      | `A2 -> "A2"
      | `A3 -> "A3"
      | `A5 -> "A5"
      | `A6 -> "A6"
      | `A7 -> "A7"
      | `A8 -> "A8"
      | `A9 -> "A9"
      | `B0 -> "B0"
      | `B1 -> "B1"
      | `B10 -> "B10"
      | `B2 -> "B2"
      | `B3 -> "B3"
      | `B4 -> "B4"
      | `B6 -> "B6"
      | `B7 -> "B7"
      | `B8 -> "B8"
      | `B9 -> "B9"
      | `C5E -> "C5E"
      | `Comm10E -> "Comm10E"
      | `DLE -> "DLE"
      | `Folio -> "Folio"
      | `Ledger -> "Ledger"
      | `Tabloid -> "Tabloid"
    in
    Lowlevel.beginprintext path mode format orientation
end

(*
   (* TODO: operates on double pointers - postponed *)
   let ndctowc = foreign "gr_ndctowc" (ptr double @-> ptr double @-> returning void)
   let wctondc = foreign "gr_wctondc" (ptr double @-> ptr double @-> returning void)
   let wc3towc = foreign "gr_wc3towc" (ptr double @-> ptr double @-> ptr double @-> returning void)
*)

let drawrect ?linetype ?linewidth ?coloridx left right bottom up =
  State.with_sandbox (fun () ->
      Option.iter set_linetype linetype;
      Option.iter set_linewidth linewidth;
      Option.iter set_linecolorindex coloridx;
      Lowlevel.drawrect left right bottom up)

let fillrect ?fillstyle ?fillcoloridx ?linetype ?linewidth ?coloridx left right
    bottom up =
  State.with_sandbox (fun () ->
      Option.iter set_linetype linetype;
      Option.iter set_linewidth linewidth;
      Option.iter set_linecolorindex coloridx;
      Option.iter set_fill_interior_style fillstyle;
      Option.iter set_fill_colorindex fillcoloridx;
      Lowlevel.fillrect left right bottom up)

let drawarc ?linetype ?linewidth ?coloridx left right bottom up a1 a2 =
  State.with_sandbox (fun () ->
      Option.iter set_linetype linetype;
      Option.iter set_linewidth linewidth;
      Option.iter set_linecolorindex coloridx;
      Lowlevel.drawarc left right bottom up a1 a2)

let fillarc ?fillstyle ?fillcoloridx ?linetype ?linewidth ?coloridx left right
    bottom up a1 a2 =
  State.with_sandbox (fun () ->
      Option.iter set_linetype linetype;
      Option.iter set_linewidth linewidth;
      Option.iter set_linecolorindex coloridx;
      Option.iter set_fill_interior_style fillstyle;
      Option.iter set_fill_colorindex fillcoloridx;
      Lowlevel.fillarc left right bottom up a1 a2)

let drawpath vertices codes fill =
  let code_to_uchar = function
    | `STOP -> Unsigned.UChar.of_int 0
    | `MOVETO -> Unsigned.UChar.of_int 1
    | `LINETO -> Unsigned.UChar.of_int 2
    | `CURVE3 -> Unsigned.UChar.of_int 3
    | `CURVE4 -> Unsigned.UChar.of_int 4
    | `CLOSEPOLY -> Unsigned.UChar.of_int 0x4f
  in
  let fill = if fill then 1 else 0 in
  let n = Array.length vertices in
  let open Ctypes in
  let cvertices = CArray.make Lowlevel.vertex n in
  let ccodes = CArray.make uchar n in
  for i = 0 to n - 1 do
    let x, y = vertices.(i) in
    let v = make Lowlevel.vertex in
    setf v Lowlevel.vertex_x x;
    setf v Lowlevel.vertex_y y;
    CArray.set cvertices i v;
    CArray.set ccodes i (code_to_uchar codes.(i))
  done;
  Lowlevel.drawpath n CArray.(start cvertices) CArray.(start ccodes) fill

let drawarrow ?arrowsize ?arrowstyle (x1, y1) (x2, y2) =
  State.with_sandbox (fun () ->
      Option.iter set_arrowsize arrowsize;
      Option.iter set_arrowstyle arrowstyle;
      Lowlevel.drawarrow x1 y1 x2 y2)

(* TODO:
   let readimage = foreign "gr_readimage" (string @-> ptr int @-> ptr int @-> ptr (ptr int) @-> returning int)
   *)

let drawimage (xmin, ymin) (xmax, ymax) image_data model =
  let model = match model with `RGB -> 0 | `HSV -> 1 in
  let width, height, image_data =
    match Bigarray.Genarray.dims image_data with
    | [| width; height |] ->
        (width, height, Ctypes.(bigarray_start genarray image_data))
    | _ -> failwith "Expecting a 2D array, but got something else!"
  in
  Lowlevel.drawimage xmin ymin xmax ymax width height image_data model

module Selection = struct
  let begins = Lowlevel.beginselection

  let ends = Lowlevel.endselection

  let move x y = Lowlevel.moveselection x y

  let resize kind x y = Lowlevel.resizeselection kind x y
end

(*
   TODO:
   inqbbox &xmin &xmax &ymin &ymax
   uselinespec linespec
  *)

(* let with_ws ?(typ = PNG) plot =
  let id = Random.int 1024 in
  try
    Lowlevel.openws id ("plot" ^ string_of_int id) (int_of_workstation_type typ);
    plot id;
    Lowlevel.closews id
  with
  | exn ->
    (try Lowlevel.closews id with
    | _ -> ());
    raise exn
*)
