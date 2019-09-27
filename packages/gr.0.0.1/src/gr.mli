(** {1 OCaml Bindings for the GR Framework} 

    GR is based on an implementation of a Graphical Kernel System (GKS) and OpenGL. As a self-contained system it can quickly and easily be integrated into existing applications.

    The GR framework can be used in imperative programming systems or integrated into modern object-oriented systems, in particular those based on GUI toolkits.
    GR is characterized by its high interoperability and can be used with modern web technologies and mobile devices.
    The GR framework is especially suitable for real-time environments.
*)

module Lowlevel = Lowlevel

(** Available workstation types, see also {{: https://gr-framework.org/workstations.html} GR Workstation Types} *)
type workstation_type =
  | WISS (** Workstation Independent Segment ptr Storage *)
  | WinGDI (** Windows ptr GDI *)
  | PS_1 (**PostScript (b/w \@-> color) *)
  | PS_2 (**PostScript (b/w \@-> color) *)
  | PS_3 (**PostScript (b/w \@-> color) *)
  | PS_4 (**PostScript (b/w \@-> color) *)
  | PDFPlain (** Portable Document Format ptr plain *)
  | PDFCompressed (** Portable Document Format ptr compressed *)
  | X_1 (** X ptr Windows *)
  | X_2 (** X ptr Windows *)
  | X_3 (** X ptr Windows *)
  | X_4 (** X ptr Windows *)
  | SunRF (** Sun Raster file (RF) *)
  | GIF87 (** Graphics Interchange Format ptr GIF87 *)
  | GIF89 (**Graphics Interchange Format ptr GIF89 *)
  | MotifUIL (** Motif User Interface Language (UIL) *)
  | BMP (** Windows Bitmap (BMP) *)
  | JPEG (** JPEG image ptr file *)
  | PNG (** Portable Network Graphics file (PNG) *)
  | TIFF (** Tagged Image File Format (TIFF) *)
  | Gtk (** ptr Gtk *)
  | Wx (** ptr wxWidgets *)
  | Qt4 (** ptr Qt4 *)
  | SVG (** Scaleable Vector Graphics (SVG) *)
  | WMF (** Windows ptr Metafile *)
  | Quartz (** ptr Quartz *)
  | Sock (** Socket ptr driver *)
  | ZMQ (** 0MQ ptr driver *)
  | OGL (** ptr OpenGL *)

(** Available line types, see also {{: https://gr-framework.org/linetypes.html} GR Line Types} *)
type linetype =
  | SOLID (** Solid line *)
  | DASHED (** Dashed line *)
  | DOTTED (** Dotted line *)
  | DASHED_DOTTED (** Dashed-dotted line *)
  | DASH_2_DOT (** Sequence of one dash followed by two dots *)
  | DASH_3_DOT (** Sequence of one dash followed by three dots *)
  | LONG_DASH (** Sequence of long dashes *)
  | LONG_SHORT_DASH (** Sequence of a long dash followed by a short dash *)
  | SPACED_DASH (** Sequence of dashes double spaced *)
  | SPACED_DOT (** Sequence of dots double spaced *)
  | DOUBLE_DOT (** Sequence of pairs of dots *)
  | TRIPLE_DOT (** Sequence of groups of three dots *)

(** Available marker types, see also {{: https://gr-framework.org/markertypes.html} GR Marker Types} *)
type markertype =
  | DOT (** Smallest displayable dot *)
  | PLUS (** Plus sign *)
  | ASTERISK (** Asterisk *)
  | CIRCLE (** Hollow circle *)
  | DIAGONAL_CROSS (** Diagonal cross *)
  | SOLID_CIRCLE (** Filled circle *)
  | TRIANGLE_UP (** Hollow triangle pointing upward *)
  | SOLID_TRI_UP (** Filled triangle pointing upward *)
  | TRIANGLE_DOWN (** Hollow triangle pointing downward *)
  | SOLID_TRI_DOWN (** Filled triangle pointing downward *)
  | SQUARE (** Hollow square *)
  | SOLID_SQUARE (** Filled square *)
  | BOWTIE (** Hollow bowtie *)
  | SOLID_BOWTIE (** Filled bowtie *)
  | HGLASS (** Hollow hourglass *)
  | SOLID_HGLASS (** Filled hourglass *)
  | DIAMOND (** Hollow diamond *)
  | SOLID_DIAMOND (** Filled Diamond *)
  | STAR (** Hollow star *)
  | SOLID_STAR (** Filled Star *)
  | TRI_UP_DOWN (** Hollow triangles pointing up and down overlaid *)
  | SOLID_TRI_RIGHT (** Filled triangle point right *)
  | SOLID_TRI_LEFT (** Filled triangle pointing left *)
  | HOLLOW_PLUS (** Hollow plus sign *)
  | SOLID_PLUS (** Solid plus sign *)
  | PENTAGON (** Pentagon *)
  | HEXAGON (** Hexagon *)
  | HEPTAGON (** Heptagon *)
  | OCTAGON (** Octagon *)
  | STAR_4 (** 4-pointed star *)
  | STAR_5 (** 5-pointed star (pentagram) *)
  | STAR_6 (** 6-pointed star (hexagram) *)
  | STAR_7 (** 7-pointed star (heptagram) *)
  | STAR_8 (** 8-pointed star (octagram) *)
  | VLINE (** verical line *)
  | HLINE (** horizontal line *)
  | OMARK (** o-mark *)

type scale_options =
  | OPTION_X_LOG (** Logarithmic X-axis *)
  | OPTION_Y_LOG (** Logarithmic Y-axis *)
  | OPTION_Z_LOG (** Logarithmic Z-axis *)
  | OPTION_FLIP_X (** Flip X-axis *)
  | OPTION_FLIP_Y (** Flip Y-axis *)
  | OPTION_FLIP_Z (** Flip Z-axis *)

type spline_algo =
  | GeneralizedCrossValidatedSmoothing
  | InterpolatingNaturalCubic
  | CubicBSpline

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

type text_precision =
  | STRING (** String precision (higher quality) *)
  | CHAR (** Character precision (medium quality) *)
  | STROKE (** Stroke precision (lower quality) *)

type text_path_direction =
  | RIGHT (** left-to-right *)
  | LEFT (** right-to-left *)
  | UP (** downside-up *)
  | DOWN (** upside-down *)

type text_halign =
  | NORMAL
  | LEFT (** Left justify *)
  | CENTER (** Center justify *)
  | RIGHT (** Right justify *)

type text_valign =
  | NORMAL
  | TOP (** Align with the top of the characters *)
  | CAP (** Aligned with the cap of the characters *)
  | HALF (** Aligned with the half line of the characters *)
  | BASE (** Aligned with the base line of the characters *)
  | BOTTOM (** Aligned with the bottom line of the characters *)

(** Pattern style, see also {{: https://gr-framework.org/patterns.html} GR Fill Patterns and Hatches}  *)
type pattern_style

val pattern_style : int -> pattern_style

(** Hatch style, see also {{: https://gr-framework.org/patterns.html} GR Fill Patterns and Hatches}  *)
type hatch_style

val hatch_style : int -> hatch_style

type fill_style =
  | HOLLOW (** No filling. Just draw the bounding polyline *)
  | SOLID (** Fill the interior of the polygon using the fill color index *)
  | PATTERN of pattern_style
      (** Fill the interior of the polygon using the style index as a pattern index *)
  | HATCH of hatch_style
      (** Fill the interior of the polygon using the style index as a cross-hatched style *)

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

type surface_options =
  | LINES (** Use X Y polylines to denote the surface *)
  | MESH (** Use a wire grid to denote the surface *)
  | FILLED_MESH (** Applies an opaque grid to the surface *)
  | Z_SHADED_MESH (** Applies Z-value shading to the surface *)
  | COLORED_MESH (** Applies a colored grid to the surface *)
  | CELL_ARRAY (** Applies a grid of individually-colored cells to the surface *)
  | SHADED_MESH (** Applies light source shading to the 3-D surface *)

type segment

module Workstation : sig
  type id

  val wid : int -> id
  val open_ : id -> string -> workstation_type -> unit
  val close : id -> unit
  val activate : id -> unit
  val deactivate : id -> unit
  val clear : unit -> unit
  val update : unit -> unit

  (** [set_window xmin xmax ymin ymax] sets the area of the NDC viewport that is to be drawn in the workstation window.

        This function defines the rectangular area of the Normalized Device Coordinate space to be output to the device.
        By default, the workstation transformation will map the range [0,1] x [0,1] in NDC onto the largest square on the workstation’s display surface.
        The aspect ratio of the workstation window is maintained at 1 to 1.

        Parameters
            xmin: The left horizontal coordinate of the workstation window (0 <= xmin < xmax).
            xmax: The right horizontal coordinate of the workstation window (xmin < xmax <= 1).
            ymin: The bottom vertical coordinate of the workstation window (0 <= ymin < ymax).
            ymax: The top vertical coordinate of the workstation window (ymin < ymax <= 1).
    *)
  val set_window : float -> float -> float -> float -> unit

  (** [set_viewport xmin xmax ymin ymax] defines the size of the workstation graphics window in meters.

        This function places a workstation window on the display of the specified size in meters.
        This command allows the workstation window to be accurately sized for a display or hardcopy device, and is often useful for sizing graphs for desktop publishing applications.

        Parameters
            xmin: The left horizontal coordinate of the workstation window.
            xmax: The right horizontal coordinate of the workstation window.
            ymin: The bottom vertical coordinate of the workstation window.
            ymax: The top vertical coordinate of the workstation window.
    *)
  val set_viewport : float -> float -> float -> float -> unit

  val copy_segment : segment -> unit
  val redraw_segment : unit -> unit
end

module Gks : sig
  val emergency_close : unit -> unit
  val update : unit -> unit
end

module State : sig
  val save : unit -> unit
  val restore : unit -> unit
  val with_sandbox : (unit -> 'a) -> 'a
end

(** [set_window xmin xmax ymin ymax] establishes a window, or rectangular subspace, of world coordinates to be plotted. If you desire log scaling or mirror-imaging of axes, use the gr_setscale function.

This function defines the rectangular portion of the World Coordinate space (WC) to be associated with the specified normalization transformation.
The WC window and the Normalized Device Coordinates (NDC) viewport define the normalization transformation through which all output primitives are mapped.
The WC window is mapped onto the rectangular NDC viewport which is, in turn, mapped onto the display surface of the open and active workstation, in device coordinates.
By default, GR uses the range [0,1] x [0,1], in world coordinates, as the normalization transformation window.

Parameters

        xmin: The left horizontal coordinate of the window (xmin < xmax).
        xmax: The right horizontal coordinate of the window (xmin < xmax).
        ymin: The bottom vertical coordinate of the window (ymin < ymax).
        ymax: The top vertical coordinate of the window (ymin < ymax).

*)
val set_window : float -> float -> float -> float -> unit

(** [set_viewport xmin xmax ymin ymax] establishes a rectangular subspace of normalized device coordinates.

This function defines the rectangular portion of the Normalized Device Coordinate (NDC) space to be associated with the specified normalization transformation.
The NDC viewport and World Coordinate (WC) window define the normalization transformation through which all output primitives pass.
The WC window is mapped onto the rectangular NDC viewport which is, in turn, mapped onto the display surface of the open and active workstation, in device coordinates.

Parameters

        xmin: The left horizontal coordinate of the viewport (0 <= xmin < xmax).
        xmax: The right horizontal coordinate of the viewport (xmin < xmax <= 1).
        ymin: The bottom vertical coordinate of the viewport (0 <= ymin < ymax).
        ymax: The top vertical coordinate of the viewport (ymin < ymax <= 1).

 *)
val set_viewport : float -> float -> float -> float -> unit

(** [select_transformation transform] selects a predefined transformation from world coordinates to normalized device coordinates.

0 	Selects the identity transformation in which both the window and viewport have the range of 0 to 1
>= 1 	Selects a normalization transformation as defined by [set_window] and [set_viewport]

Parameters
        transform: A normalization transformation number.
*)
val select_transformation : int -> unit

(** [clip indicator] sets the clipping indicator.

    false 	Clipping is off. Data outside of the window will be drawn.
    true 	Clipping is on. Data outside of the window will not be drawn.

    Parameters
            indicator: An indicator specifying whether clipping is on or off.

    This function enables or disables clipping of the image drawn in the current window.
    Clipping is defined as the removal of those portions of the graph that lie outside of the defined viewport. If clipping is on, GR does not draw generated output primitives past the viewport boundaries. If clipping is off, primitives may exceed the viewport boundaries, and they will be drawn to the edge of the workstation window.
    By default, clipping is on.
*)
val clip : bool -> unit

val segment : int -> segment
val create_segment : segment -> unit

val set_segment_transform
  :  segment
  -> float
  -> float
  -> float
  -> float
  -> float
  -> float
  -> float
  -> unit

val close_segment : unit -> unit

(** [set_space zmin zmax rotation tilt] sets the abstract Z-space used for mapping three-dimensional output primitives into the current world coordinate space.

This function establishes the limits of an abstract Z-axis and defines the angles for rotation and for the viewing angle (tilt) of a simulated three-dimensional graph, used for mapping corresponding output primitives into the current window.
These settings are used for all subsequent three-dimensional output primitives until other values are specified.
Angles of rotation and viewing angle must be specified between 0 and 90 degrees.

Parameters
        zmin: Minimum value for the Z-axis.
        zmax: Maximum value for the Z-axis.
        rotation: Angle for the rotation of the X axis, in degrees.
        tilt: Viewing angle of the Z axis, in degrees.
*)
val set_space : float -> float -> int -> int -> int

val set_linetype : linetype -> unit

(** [set_linewidth lw] defines the line width of subsequent polyline output primitives.

    The line width is calculated as the nominal line width generated on the workstation multiplied by the line width scale factor.
    This value is mapped by the workstation to the nearest available line width.
    The default line width is 1.0, or 1 times the line width generated on the graphics device.
*)
val set_linewidth : float -> unit

(** [set_linecolorindex c] defines the color of subsequent polyline output primitives.
    Note: c < 1256
*)
val set_linecolorindex : int -> unit

val set_markertype : markertype -> unit

(** [set_markersize ms] specify the marker size for polymarkers.

    The polymarker size is calculated as the nominal size generated on the graphics device multiplied by the marker size scale factor. 
*)
val set_markersize : float -> unit

(** [set_markercolorindex c] define the color of subsequent markers output primitives.
    Note: c < 1256
*)
val set_markercolorindex : int -> unit

(** [set_arrowstyle style] sets the arrow style to be used for subsequent arrow commands.

    This function defines the arrow style for subsequent arrow primitives.
    The default arrow style is 1.

    Parameters
        style: The arrow style to be used. Available styles are:

            1 	simple, single-ended
            2 	simple, single-ended, acute head
            3 	hollow, single-ended
            4 	filled, single-ended
            5 	triangle, single-ended
            6 	filled triangle, single-ended
            7 	kite, single-ended
            8 	filled kite, single-ended
            9 	simple, double-ended
            10 	simple, double-ended, acute head
            11 	hollow, double-ended
            12 	filled, double-ended
            13 	triangle, double-ended
            14 	filled triangle, double-ended
            15 	kite, double-ended
            16 	filled kite, double-ended
            17 	double line, single-ended
            18 	double line, double-ended

  TODO: Use a sum type.
*)
val set_arrowstyle : int -> unit

(* [set_arrowsize size] sets the arrow size to be used for subsequent arrow commands.

    This function defines the arrow size for subsequent arrow primitives.
    The default arrow size is 1.

    Parameters
        size: The arrow size to be used
*)
val set_arrowsize : float -> unit

(** [set_text_font_prec ?precision font] specifies the text font and precision for subsequent text output primitives.

    The appearance of a font depends on the text precision value specified.
    STRING, CHARACTER or STROKE precision allows for a greater or lesser realization of the text primitives, for efficiency.
    STRING is the default precision for GR and produces the highest quality output.

    XXX: CHARACTER and STROKE precision seem to be broken (and to break the [axes] command with it...)!
*)
val set_text_font_prec : ?precision:text_precision -> font -> unit

(** [set_char_expand_factor factor] sets the current character expansion factor (width to height ratio).

    This function defines the width of subsequent text output primitives.
    The expansion factor alters the width of the generated characters, but not their height. The default text expansion factor is 1, or one times the normal width-to-height ratio of the text.

    Parameters
        factor: Text expansion factor applied to the nominal text width-to-height ratio
*)
val set_char_expand_factor : float -> unit

(** [set_text_colorindex color] sets the current text color index.

    This function defines the color of subsequent text output primitives.
    GR uses the default foreground color (black=1) for the default text color index.

    Parameters
            color: The text color index (COLOR < 1256)
*)
val set_text_colorindex : int -> unit

(** [set_char_height height] sets the current character height.

    This function defines the height of subsequent text output primitives.
    Text height is defined as a percentage of the default window.
    GR uses the default text height of 0.027 (2.7% of the height of the default window).
*)
val set_char_height : float -> unit

(** [set_char_up (x, y)]
    Set the current character text angle up vector.
    This function defines the vertical rotation of subsequent text output primitives.
    The text up vector is initially set to (0, 1), horizontal to the baseline.

    Parameters
            x: X coordinate of the text up vector
            y: Y coordinate of the text up vector
*)
val set_char_up : float * float -> unit

val set_char_space : float -> unit

(** [set_text_path direction] defines the current direction in which subsequent text will be drawn. *)
val set_text_path : text_path_direction -> unit

(** [set_text_align horizontal vertical] specifies how the characters in a text primitive will be aligned in horizontal and vertical space.
    The default text alignment indicates horizontal left alignment and vertical baseline alignment.
*)
val set_text_align : text_halign option -> text_valign option -> unit

(** [set_fill_interior_style style] sets the fill area interior style to be used for fill areas.

    This function defines the interior style for subsequent fill area output primitives.
    The default interior style is HOLLOW. 
*)
val set_fill_interior_style : fill_style -> unit

(** [set_fill_colorindex color] sets the current fill area color index.

    This function defines the color of subsequent fill area output primitives.
    GR uses the default foreground color (black=1) for the default fill area color index.

    Parameters
            color: The fill area color index (COLOR < 1256)
*)
val set_fill_colorindex : int -> unit

(** [set_color_representation index (red, green, blue)] redefines an existing color index representation by specifying an RGB color triplet.

    Parameters
        index: Color index in the range 0 to 1256
        red: Red intensity in the range 0.0 to 1.0
        green: Green intensity in the range 0.0 to 1.0
        blue: Blue intensity in the range 0.0 to 1.0

*)
val set_color_representation : int -> float * float * float -> unit

(** [set_colormap cmap] sets the currently used colormap.

    A list of colormaps can be found at: {{: https://gr-framework.org/colormaps.html} GR Colormaps}.
*)
val set_colormap : color_map -> unit

val set_scale : scale_options list -> int

(** [set_shadow (offsetx, offsety) blur]
    Allows drawing of shadows, realized by images painted underneath, and offset from, graphics objects such that the shadow mimics the effect of a light source cast on the graphics objects.

    Parameters
            offsetx: An x-offset, which specifies how far in the horizontal direction the shadow is offset from the object
            offsety: A y-offset, which specifies how far in the vertical direction the shadow is offset from the object
            blur: A blur value, which specifies whether the object has a hard or a diffuse edge
*)
val set_shadow : float * float -> float -> unit

(** [set_transparency alpha] sets the value of the alpha component associated with GR colors.

    Parameters
            alpha: An alpha value (0.0 - 1.0)
*)
val set_transparency : float -> unit

(** [set_coord_transform transform]
    Change the coordinate transformation according to the given matrix.
    Parameters
            mat: 2D transformation matrix (3x2)
*)
val set_coord_transform
  :  (float Ctypes_static.ptr, 'a, 'b) Bigarray.Genarray.t
  -> unit

module Graphics : sig
  type t

  (** [beging path]
    Open a file for graphics output.

    [beging] allows to write all graphics output into a XML-formatted file until the [endg] functions is called.
    The resulting file may later be imported with the [import] function.

    Parameters
            path: Filename for the graphics file.
*)
  val beging : t -> unit

  val endg : unit -> unit
  val get : unit -> t
  val draw : t -> int
  val import : string -> int
end

(**
   [polyline ?linetype ?linewidth ?coloridx x y] draws a polyline using the current line attributes, starting from the first data point and ending at the last data point.

   The values for [x] and [y] are in world coordinates.
   The attributes that control the appearance of a polyline are linetype, linewidth and color index.
*)
val polyline
  :  ?linetype:linetype
  -> ?linewidth:float
  -> ?coloridx:int
  -> (float, 'a, 'b) Bigarray.Genarray.t
  -> (float, 'c, 'd) Bigarray.Genarray.t
  -> unit

(**
   [polyline3d ?linetype ?linewidth ?coloridx x y z] draws a polyline using the current line attributes, starting from the first data point and ending at the last data point.

   The values for [x], [y] and [z] are in world coordinates.
   The attributes that control the appearance of a polyline are linetype, linewidth and color index.
*)
val polyline3d
  :  ?linetype:linetype
  -> ?linewidth:float
  -> ?coloridx:int
  -> (float, 'a, 'b) Bigarray.Genarray.t
  -> (float, 'c, 'd) Bigarray.Genarray.t
  -> (float, 'e, 'f) Bigarray.Genarray.t
  -> unit

(**
     [polymarker ?markertype ?markersize ?coloridx x y] draws marker symbols centered at the given data points.

     The values for [x] and [y] are in world coordinates.
     The attributes that control the appearance of a polyline are markertype, markersize and color index.
  *)
val polymarker
  :  ?markertype:markertype
  -> ?markersize:float
  -> ?coloridx:int
  -> (float, 'a, 'b) Bigarray.Genarray.t
  -> (float, 'c, 'd) Bigarray.Genarray.t
  -> unit

(**
     [polymarker3d ?markertype ?markersize ?coloridx x y z] draws marker symbols centered at the given data points.

     The values for [x], [y] and [z] are in world coordinates.
     The attributes that control the appearance of a polyline are markertype, markersize and color index.
  *)
val polymarker3d
  :  ?markertype:markertype
  -> ?markersize:float
  -> ?coloridx:int
  -> (float, 'a, 'b) Bigarray.Genarray.t
  -> (float, 'c, 'd) Bigarray.Genarray.t
  -> (float, 'e, 'f) Bigarray.Genarray.t
  -> unit

(** [text x y content] draws a text at position [x], [y] using the current text attributes.

        The values for [x] and [y] are in normalized device coordinates.
        The attributes that control the appearance of text are text font and precision, character expansion factor, character spacing, text color index, character height, character up vector, text path and text alignment. 
    *)
val text : float -> float -> string -> unit

(** 
   [fillarea x y] allows you to specify a polygonal shape of an area to be filled.
   The vectors [x] and [y] specify the coordinates of the polygonal shape corners.

   The attributes that control the appearance of fill areas are fill area interior style, fill area style index and fill area color index. 
*)
val fillarea
  :  (float, 'a, 'b) Bigarray.Genarray.t
  -> (float, 'c, 'd) Bigarray.Genarray.t
  -> unit

(** [cellarray (xmin, xmax) (ymin, ymax) (dimx, dimy) (scol, srow) (ncol, nrow) color] displays rasterlike images in a device-independent manner.
    The cell array function partitions a rectangle given by two corner points into DIMX X DIMY cells, each of them colored individually by the corresponding color index of the given cell array.

    The values for xmin, xmax, ymin and ymax are in world coordinates.

    Parameters:
        xmin: X coordinate of the lower left point of the rectangle
        ymin: Y coordinate of the lower left point of the rectangle
        xmax: X coordinate of the upper right point of the rectangle
        ymax: Y coordinate of the upper right point of the rectangle
        dimx: X dimension of the color index array
        dimy: Y dimension of the color index array
        scol: number of leading columns in the color index array
        srow: number of leading rows in the color index array
        ncol: total number of columns in the color index array
        nrow: total number of rows in the color index array
        color: color index array

    Note: gr_nonuniformcellarray and gr_polycellarray have been introduced in newer versions of gr.
*)
val cellarray
  :  float * float
  -> float * float
  -> int * int
  -> int * int
  -> int * int
  -> (int, 'a, 'b) Bigarray.Genarray.t
  -> unit

(** [spline ?linetype ?linewidth ?coloridx x y m method_t] generates a cubic spline-fit, starting from the first data point and ending at the last data point.

    The values for [x] and [y] are in world coordinates.
    The attributes that control the appearance of a spline-fit are linetype, linewidth and color index.

    Parameters
            x: The X coordinates
            y: The Y coordinates
            m: The number of points in the polygon to be drawn (m > n)
            method: The smoothing method

    If method is > 0, then a generalized cross-validated smoothing spline is calculated. If method is 0, then an interpolating natural cubic spline is calculated. If method is < -1, then a cubic B-spline is calculated.
*)
val spline
  :  ?linetype:linetype
  -> ?linewidth:float
  -> ?coloridx:int
  -> (float, 'a, 'b) Bigarray.Genarray.t
  -> (float, 'c, 'd) Bigarray.Genarray.t
  -> int
  -> spline_algo
  -> unit

(** [gridit x y z (nx, ny)] interpolates data from arbitrary points at points on a rectangular grid.

      Parameters

          x: The X coordinates of the input points
          y: The Y coordinates of the input points
          z: The values of the points
          nx: The number of points in X direction for the output grid
          ny: The number of points in Y direction for the output grid

      Returns the tuple (x', y', z') with

      x': The points in X direction for the output grid
      y': The points in Y direction for the output grid
      z': The interpolated values on the nx x ny grid points
  *)
val gridit
  :  (float, 'a, 'b) Bigarray.Genarray.t
  -> (float, 'c, 'd) Bigarray.Genarray.t
  -> (float, 'e, 'f) Bigarray.Genarray.t
  -> int * int
  -> ( float
     , Bigarray.float64_elt
     , Bigarray.c_layout )
     Bigarray.Genarray.t
     * ( float
       , Bigarray.float64_elt
       , Bigarray.c_layout )
       Bigarray.Genarray.t
     * ( float
       , Bigarray.float64_elt
       , Bigarray.c_layout )
       Bigarray.Genarray.t

(** [tex_text (x, y) text] draws a text at position x, y using the current text attributes.
Strings can be defined to create basic mathematical expressions and Greek letters.

The values for X and Y are in normalized device coordinates.
The attributes that control the appearance of text are text font and precision, character expansion factor, character spacing, text color index, character height, character up vector, text path and text alignment.

Parameters
        x: The X coordinate of starting position of the text string
        y: The Y coordinate of starting position of the text string
        text: The text to be drawn

The character string is interpreted to be a simple mathematical formula.
The following notations apply:

- Subscripts and superscripts: These are indicated by carets (‘^’) and underscores (‘_’).
  If the sub/superscript contains more than one character, it must be enclosed in curly braces (‘\{\}’).
- Fractions are typeset with A ‘/’ B, where A stands for the numerator and B for the denominator.

To include a Greek letter you must specify the corresponding keyword after a backslash (‘') character. The text translator produces uppercase or lowercase Greek letters depending on the case of the keyword. 
For more sophisticated mathematical formulas, you should use the gr_mathtex function.

See the full documentation at {{: https://gr-framework.org/c-gr.html#_CPPv410gr_textextddPc} GR Documentation for gr_textext}.
*)
val tex_text : float * float -> string -> int

(** [math_tex (x, y) tex] generates a character string starting at the given location.
Strings can be defined to create mathematical symbols and Greek letters using LaTeX syntax.

Parameters
        x: The X coordinate of the starting position of the text string
        y: The Y coordinate of the starting position of the text string
        tex: The TeX text string to be drawn
 *)
val math_tex : float * float -> string -> unit

(** [axes ?scale ?linetype ?linewidth ?origin:(0,0) ?major:(0,0) ?size:1 x_tick y_tick] draws X and Y coordinate axes with linearly and/or logarithmically spaced tick marks.
    Tick marks are positioned along each axis so that major tick marks fall on the axes origin (whether visible or not).
    Major tick marks are labeled with the corresponding data values.
    Axes are drawn according to the scale of the window.

    Parameters
        x_tick: The interval between minor tick marks on the X axis.
        y_tick: The interval between minor tick marks on the Y axis.
        x_org: The world coordinate of the origin (point of intersection) of the X axis.
        y_org: The world coordinate of the origin (point of intersection) of the Y axis.
        major_x: Unitless integer value specifying the number of minor tick intervals between major tick marks on the X axis. Values of 0 or 1 imply no minor ticks. Negative values specify no labels will be drawn for the associated axis.
        major_y: Unitless integer value specifying the number of minor tick intervals between major tick marks on the Y axis. Values of 0 or 1 imply no minor ticks. Negative values specify no labels will be drawn for the associated axis.
        tick_size: The length of minor tick marks specified in a normalized device coordinate unit. Major tick marks are twice as long as minor tick marks. A negative value reverses the tick marks on the axes from inward facing to outward facing (or vice versa)
*)
val axes
  :  ?scale:scale_options list
  -> ?linetype:linetype
  -> ?linewidth:float
  -> ?coloridx:int
  -> ?origin:float * float
  -> ?major:int * int
  -> ?tick_size:float
  -> float
  -> float
  -> unit

(** [axes_labels ?(scale = []) ?linetype ?linewidth ?coloridx ?(origin = 0.0, 0.0) ?(major = 0, 0) ?(tick_size = -0.01) (fpx : float -> float -> string -> float -> unit) (fpy : float -> float -> string -> float -> unit) x_tick y_tick] creates axes in the current workspace and supply a custom function for changing the behaviour of the tick labels.

      Similar to [axes] but allows more fine-grained control over tick labels and text positioning by supplying callback functions.
      Within the callback function you can use normal GR text primitives for performing any manipulations on the label text.

      See [axes] for more details on drawing axes.

      Parameters

          x_tick: The interval between minor tick marks on the X axis.
          y_tick: The interval between minor tick marks on the Y axis.
          x_org: The world coordinate of the origin (point of intersection) of the X axis.
          y_org: The world coordinate of the origin (point of intersection) of the Y axis.
          major_x: Unitless integer value specifying the number of minor tick intervals between major tick marks on the X axis. Values of 0 or 1 imply no minor ticks. Negative values specify no labels will be drawn for the associated axis.
          major_y: Unitless integer value specifying the number of minor tick intervals between major tick marks on the Y axis. Values of 0 or 1 imply no minor ticks. Negative values specify no labels will be drawn for the associated axis.
          tick_size: The length of minor tick marks specified in a normalized device coordinate unit. Major tick marks are twice as long as minor tick marks. A negative value reverses the tick marks on the axes from inward facing to outward facing (or vice versa).
          fpx: Function pointer to a function that returns a label for a given tick on the X axis. The callback function should have the following arguments:
            x: NDC of the label in X direction.
            y: NDC of the label in Y direction.
            svalue: Internal string representation of the text drawn by GR at (x,y).
            value: Floating point representation of the label drawn at (x,y).
          fpy: Exactly same as the fpx above, but for the the Y axis.

  *)
val axes_labels
  :  ?scale:scale_options list
  -> ?linetype:linetype
  -> ?linewidth:float
  -> ?coloridx:int
  -> ?origin:float * float
  -> ?major:int * int
  -> ?tick_size:float
  -> (float -> float -> string -> float -> unit)
  -> (float -> float -> string -> float -> unit)
  -> float
  -> float
  -> unit

(** [axes3d ?scale ?linetype ?linewidth ?origin:(0,0) ?major:(0,0) ?size:1 x_tick y_tick] draws X, Y and Z coordinate axes with linearly and/or logarithmically spaced tick marks.
    Tick marks are positioned along each axis so that major tick marks fall on the axes origin (whether visible or not).
    Major tick marks are labeled with the corresponding data values.
    Axes are drawn according to the scale of the window.

    Parameters
        x_tick: The interval between minor tick marks on the X axis.
        y_tick: The interval between minor tick marks on the Y axis.
        z_tick: The length in world coordinates of the interval between minor grid lines in Z direction. 
        x_org: The world coordinate of the origin (point of intersection) of the X axis.
        y_org: The world coordinate of the origin (point of intersection) of the Y axis.
        z_org: The world coordinate of the origin (point of intersection) of the Z axis. 
        major_x: Unitless integer value specifying the number of minor tick intervals between major tick marks on the X axis. Values of 0 or 1 imply no minor ticks. Negative values specify no labels will be drawn for the associated axis.
        major_y: Unitless integer value specifying the number of minor tick intervals between major tick marks on the Y axis. Values of 0 or 1 imply no minor ticks. Negative values specify no labels will be drawn for the associated axis.
        major_z: Unitless integer value specifying the number of minor grid lines between major grid lines on the Z axis. Values of 0 or 1 imply no grid lines. 
        tick_size: The length of minor tick marks specified in a normalized device coordinate unit. Major tick marks are twice as long as minor tick marks. A negative value reverses the tick marks on the axes from inward facing to outward facing (or vice versa)
*)
val axes3d
  :  ?scale:scale_options list
  -> ?linetype:linetype
  -> ?linewidth:float
  -> ?coloridx:int
  -> ?origin:float * float * float
  -> ?major:int * int * int
  -> ?tick_size:float
  -> float
  -> float
  -> float
  -> unit

(** [surface x y z ?option] draws a three-dimensional surface plot for the given data points.

        x and y define a grid.
        z is a singly dimensioned array containing at least nx * ny data points.
        z describes the surface height at each point on the grid.
        Data is ordered as shown in the following table:

        Parameters

            nx: The number of points along the X axis
            ny: The number of points along the Y axis
            px: A pointer to the X coordinates
            py: A pointer to the Y coordinates
            pz: A pointer to the Z coordinates
            option: Surface display option (see table)
    *)
val surface
  :  ?options:surface_options
  -> (float, 'a, 'b) Bigarray.Genarray.t
  -> (float, 'c, 'd) Bigarray.Genarray.t
  -> (float, 'e, 'f) Bigarray.Genarray.t
  -> unit

(** [contour ?major_h x y h z] sraw contours of a three-dimensional data set whose values are specified over a rectangular mesh.
    Contour lines may optionally be labeled.

    Parameters

        nx: The number of points along the X axis
        ny: The number of points along the Y axis
        nh: The number of height values
        px: A pointer to the X coordinates
        py: A pointer to the Y coordinates
        h: A pointer to the height values
        pz: A pointer to the Z coordinates
        major_h: Directs GR to label contour lines. For example, a value of 3 would label every third line. A value of 1 will label every line. A value of 0 produces no labels. To produce colored contour lines, add an offset of 1000 to major_h

*)
val contour
  :  ?major_h:int
  -> (float, 'a, 'b) Bigarray.Genarray.t
  -> (float, 'c, 'd) Bigarray.Genarray.t
  -> (float, 'e, 'f) Bigarray.Genarray.t
  -> (float, 'g, 'h) Bigarray.Genarray.t
  -> unit

(** [contourf ?(major_h = 0) x y h z] draws filled contour plot of a three-dimensional data set whose values are specified over a rectangular mesh.

Parameters
        px: A pointer to the X coordinates
        py: A pointer to the Y coordinates
        h: A pointer to the height values. If NULL, use nh evenly distributed height values between minimum and maximum Z value.
        major_h: Directs GR to label contour lines. For example, a value of 3 would label every third line. A value of 1 will label every line. A value of 0 produces no labels. To produce colored contour lines, add an offset of 1000 to major_h

 *)
val contourf
  :  ?major_h:int
  -> (float, 'a, 'b) Bigarray.Genarray.t
  -> (float, 'c, 'd) Bigarray.Genarray.t
  -> (float, 'e, 'f) Bigarray.Genarray.t
  -> (float, 'g, 'h) Bigarray.Genarray.t
  -> unit

(** [grid ?scale ?linetype ?linewidth ?origin:(0,0) ?major:(0,0) x_tick y_tick] draws a linear and/or logarithmic grid.

Major grid lines correspond to the axes origin and major tick marks whether visible or not. Minor grid lines are drawn at points equal to minor tick marks. Major grid lines are drawn using black lines and minor grid lines are drawn using gray lines.

Parameters

        x_tick: The length in world coordinates of the interval between minor grid lines in X direction.
        y_tick: The length in world coordinates of the interval between minor grid lines in Y direction.
        x_org: The world coordinate of the origin (point of intersection) of the X axis.
        y_org: The world coordinate of the origin (point of intersection) of the Y axis.
        major_x: Unitless integer value specifying the number of minor grid lines between major grid lines on the X axis. Values of 0 or 1 imply no grid lines.
        major_y: Unitless integer value specifying the number of minor grid lines between major grid lines on the Y axis. Values of 0 or 1 imply no grid lines.

*)
val grid
  :  ?scale:scale_options list
  -> ?linetype:linetype
  -> ?linewidth:float
  -> ?coloridx:int
  -> ?origin:float * float
  -> ?major:int * int
  -> float
  -> float
  -> unit

(** [grid3d ?scale ?linetype ?linewidth ?origin:(0,0,0) ?major:(0,0,0) x_tick y_tick z_tick] draws a linear and/or logarithmic grid.

Major grid lines correspond to the axes origin and major tick marks whether visible or not. Minor grid lines are drawn at points equal to minor tick marks. Major grid lines are drawn using black lines and minor grid lines are drawn using gray lines.

Parameters

        x_tick: The length in world coordinates of the interval between minor grid lines in X direction.
        y_tick: The length in world coordinates of the interval between minor grid lines in Y direction.
        z_tick: The length in world coordinates of the interval between minor grid lines in Z direction. 
        x_org: The world coordinate of the origin (point of intersection) of the X axis.
        y_org: The world coordinate of the origin (point of intersection) of the Y axis.
        z_org: The world coordinate of the origin (point of intersection) of the Z axis. 
        major_x: Unitless integer value specifying the number of minor grid lines between major grid lines on the X axis. Values of 0 or 1 imply no grid lines.
        major_y: Unitless integer value specifying the number of minor grid lines between major grid lines on the Y axis. Values of 0 or 1 imply no grid lines.
        major_z: Unitless integer value specifying the number of minor grid lines between major grid lines on the Z axis. Values of 0 or 1 imply no grid lines.

*)
val grid3d
  :  ?scale:scale_options list
  -> ?linetype:linetype
  -> ?linewidth:float
  -> ?coloridx:int
  -> ?origin:float * float * float
  -> ?major:int * int * int
  -> float
  -> float
  -> float
  -> unit

(** [vertical_errorbars x y el eu] draws a standard vertical error bar graph.

Parameters
        px: A pointer to the X coordinates
        py: A pointer to the Y coordinates
        el: A pointer to the absolute values of the lower error bar data
        eu: A pointer to the absolute values of the upper error bar data
*)
val vertical_errorbars
  :  (float, 'a, 'b) Bigarray.Genarray.t
  -> (float, 'c, 'd) Bigarray.Genarray.t
  -> (float, 'e, 'f) Bigarray.Genarray.t
  -> (float, 'g, 'h) Bigarray.Genarray.t
  -> unit

(** [horizontal_errorbars x y el eu] draws a standard horizontal error bar graph.

Parameters
        px: A pointer to the X coordinates
        py: A pointer to the Y coordinates
        el: A pointer to the absolute values of the lower error bar data
        eu: A pointer to the absolute values of the upper error bar data
*)
val horizontal_errorbars
  :  (float, 'a, 'b) Bigarray.Genarray.t
  -> (float, 'c, 'd) Bigarray.Genarray.t
  -> (float, 'e, 'f) Bigarray.Genarray.t
  -> (float, 'g, 'h) Bigarray.Genarray.t
  -> unit

(** [titles3d x_title y_title z_title] displays axis titles just outside of their respective axes.

    Parameters
            x_title: The text to be displayed on the X axis
            y_title: The text to be displayed on the Y axis
            z_title: The text to be displayed on the Z axis
*)
val titles3d : string -> string -> string -> unit

(** [tricontour x y z levels] draws a contour plot for the given triangle mesh.

Parameters
        x: A pointer to the X coordinates
        y: A pointer to the Y coordinates
        z: A pointer to the Z coordinates
        levels: A pointer to the contour levels
 *)
val tricontour
  :  (float, 'a, 'b) Bigarray.Genarray.t
  -> (float, 'c, 'd) Bigarray.Genarray.t
  -> (float, 'e, 'f) Bigarray.Genarray.t
  -> (float, 'g, 'h) Bigarray.Genarray.t
  -> unit

(** [colorbar ()]  plots a colorbar. *)
val colorbar : unit -> unit

val tick : float -> float -> float

module Print : sig
  (** [beginp path] opens and activates a print device.

This function opens an additional graphics output device.
The device type is obtained from the given file extension.

Parameters

        pathname: Filename for the print device.

The following file types are supported:
.ps, .eps 	PostScript
.pdf 	Portable Document Format
.bmp 	Windows Bitmap (BMP)
.jpeg, .jpg 	JPEG image file
.png 	Portable Network Graphics file (PNG)
.tiff, .tif 	Tagged Image File Format (TIFF)
.svg 	Scalable Vector Graphics
.wmf 	Windows Metafile
.mp4 	MPEG-4 video file
.webm 	WebM video file
.ogg 	Ogg video file
*)
  val beginp : string -> unit

  val endp : unit -> unit

  (**
[beginprint_ext path mode format orientation] opens and activates a print device with the given layout attributes.

The available formats are:
    A4 	0.210 x 0.297
    B5 	0.176 x 0.250
    Letter 	0.216 x 0.279
    Legal 	0.216 x 0.356
    Executive 	0.191 x 0.254
    A0 	0.841 x 1.189
    A1 	0.594 x 0.841
    A2 	0.420 x 0.594
    A3 	0.297 x 0.420
    A5 	0.148 x 0.210
    A6 	0.105 x 0.148
    A7 	0.074 x 0.105
    A8 	0.052 x 0.074
    A9 	0.037 x 0.052
    B0 	1.000 x 1.414
    B1 	0.500 x 0.707
    B10 	0.031 x 0.044
    B2 	0.500 x 0.707
    B3 	0.353 x 0.500
    B4 	0.250 x 0.353
    B6 	0.125 x 0.176
    B7 	0.088 x 0.125
    B8 	0.062 x 0.088
    B9 	0.044 x 0.062
    C5E 	0.163 x 0.229
    Comm10E 	0.105 x 0.241
    DLE 	0.110 x 0.220
    Folio 	0.210 x 0.330
    Ledger 	0.432 x 0.279
    Tabloid 	0.279 x 0.432

Parameters

        pathname: Filename for the print device.
        mode: Output mode (Color, GrayScale)
        format: Output format (see table)
        orientation: Page orientation (Landscape, Portait)
*)
  val beginp_extended
    :  string
    -> [< `Color | `GreyScale ]
    -> [< `A0
       | `A1
       | `A2
       | `A3
       | `A4
       | `A5
       | `A6
       | `A7
       | `A8
       | `A9
       | `B0
       | `B1
       | `B10
       | `B2
       | `B3
       | `B4
       | `B5
       | `B6
       | `B7
       | `B8
       | `B9
       | `C5E
       | `Comm10E
       | `DLE
       | `Executive
       | `Folio
       | `Ledger
       | `Legal
       | `Letter
       | `Tabloid
       ]
    -> [< `Landscape | `Portrait ]
    -> unit
end

(** [drawrect ?linetype ?linewidth ?coloridx left right bottom up] draws a rectangle.

  Parameters
        left: Left edge of the rectangle
        right: Right edge of the rectangle
        bottom: Bottom edge of the rectangle
        up: Upper edge of the rectangle
 *)
val drawrect
  :  ?linetype:linetype
  -> ?linewidth:float
  -> ?coloridx:int
  -> float
  -> float
  -> float
  -> float
  -> unit

(** [fillrect ?fillstyle ?fillcoloridx ?linetype ?linewidth ?coloridx left right bottom up] draws a filled rectangle.

  Parameters
        left: Left edge of the rectangle
        right: Right edge of the rectangle
        bottom: Bottom edge of the rectangle
        up: Upper edge of the rectangle
 *)
val fillrect
  :  ?fillstyle:fill_style
  -> ?fillcoloridx:int
  -> ?linetype:linetype
  -> ?linewidth:float
  -> ?coloridx:int
  -> float
  -> float
  -> float
  -> float
  -> unit

(** [drawarc ?linetype ?linewidth ?coloridx left right bottom up a1 a2] draws a circular or elliptical arc covering the specified rectangle.

The resulting arc begins at a1 and ends at a2 degrees.
Angles are interpreted such that 0 degrees is at the 3 o’clock position.
The center of the arc is the center of the given rectangle. 

  Parameters
        left: Left edge of the rectangle
        right: Right edge of the rectangle
        bottom: Bottom edge of the rectangle
        up: Upper edge of the rectangle
        a1: The start angle
        a2: The end angle
 *)
val drawarc
  :  ?linetype:linetype
  -> ?linewidth:float
  -> ?coloridx:int
  -> float
  -> float
  -> float
  -> float
  -> int
  -> int
  -> unit

(** [fillarc ?fillstyle ?fillcoloridx ?linetype ?linewidth ?coloridx left right bottom up] draws a filled circular or elliptical arc covering the specified rectangle.

The resulting arc begins at a1 and ends at a2 degrees.
Angles are interpreted such that 0 degrees is at the 3 o’clock position.
The center of the arc is the center of the given rectangle. 

  Parameters
        left: Left edge of the rectangle
        right: Right edge of the rectangle
        bottom: Bottom edge of the rectangle
        up: Upper edge of the rectangle
        a1: The start angle
        a2: The end angle
 *)
val fillarc
  :  ?fillstyle:fill_style
  -> ?fillcoloridx:int
  -> ?linetype:linetype
  -> ?linewidth:float
  -> ?coloridx:int
  -> float
  -> float
  -> float
  -> float
  -> int
  -> int
  -> unit

(** [drawpath vertices codes fill] draws simple and compound outlines consisting of line segments and bezier curves.

    The following path codes are recognized:
    `STOP 	end the entire path
    `MOVETO 	move to the given vertex
    `LINETO 	draw a line from the current position to the given vertex
    `CURVE3 	draw a quadratic Bezier curve
    `CURVE4 	draw a cubic Bezier curve
    `CLOSEPOLY 	draw a line segment to the start point of the current path

    Parameters
        vertices: the vertices (x,y)
        codes: path codes
        fill: A flag indication whether resulting path is to be filled or not
*)
val drawpath
  :  (float * float) array
  -> [< `CLOSEPOLY | `CURVE3 | `CURVE4 | `LINETO | `MOVETO | `STOP ] array
  -> bool
  -> unit

(** [drawarrow ?arrowsize ?arrowstyle (x1, y1) (x2, y2)] draws an arrow between two points.

    Different arrow styles (angles between arrow tail and wing, optionally filled heads, double headed arrows) are available.
    Check the documentation of [set_arrowstyle] and [set_arrowsize] for more information.

    Parameters

        x1: The X coordinate of the arrow start point (tail)
        y1: The Y coordinate of the arrow start point (tail)
        x2: The X coordinate of the arrow end point (head)
        y2: The Y coordinate of the arrow end point (head)
 *)
val drawarrow
  :  ?arrowsize:float
  -> ?arrowstyle:int
  -> float * float
  -> float * float
  -> unit

(** [drawimage (xmin, ymin) (xmax, ymax) image_data model] draws an image into a given rectangular area.

  The points (xmin, ymin) and (xmax, ymax) are world coordinates defining diagonally opposite corner points of a rectangle.
  This rectangle is divided into width by height cells. 
  The two-dimensional array data specifies colors for each cell.

  Parameters

          xmin: X coordinate of the lower left point of the rectangle
          ymin: Y coordinate of the lower left point of the rectangle
          xmax: X coordinate of the upper right point of the rectangle
          ymax: Y coordinate of the upper right point of the rectangle
          width: X dimension of the color index array
          height: Y dimension of the color index array
          data: color array
          model: color model

  The available color models are:
    RGB 	0 	AABBGGRR
    HSV 	1 	AAVVSSHH
*)
val drawimage
  :  float * float
  -> float * float
  -> (int, 'a, 'b) Bigarray.Genarray.t
  -> [< `HSV | `RGB ]
  -> unit

module Selection : sig
  (** [beings index kind] *)
  val begins : int -> int -> unit

  val ends : unit -> unit

  (** [resize x y] *)
  val move : float -> float -> unit

  (** [resize kind x y] *)
  val resize : int -> float -> float -> unit
end

(* val with_ws : ?typ:workstation_type -> (int -> 'a) -> unit *)
