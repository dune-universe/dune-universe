(* File generated from plplot_core.idl *)

type plplot3d_style_enum =
  | PL_DIFFUSE
  | PL_DRAW_LINEX
  | PL_DRAW_LINEY
  | PL_DRAW_LINEXY
  | PL_MAG_COLOR
  | PL_BASE_CONT
  | PL_TOP_CONT
  | PL_SURF_CONT
  | PL_DRAW_SIDES
  | PL_FACETED
  | PL_MESH
and plplot3d_style = plplot3d_style_enum list
and plplot_bin_enum =
  | PL_BIN_DEFAULT
  | PL_BIN_CENTRED
  | PL_BIN_NOEXPAND
  | PL_BIN_NOEMPTY
and plplot_bin_style = plplot_bin_enum list
and plplot_hist_enum =
  | PL_HIST_DEFAULT
  | PL_HIST_NOSCALING
  | PL_HIST_IGNORE_OUTLIERS
  | PL_HIST_NOEXPAND
  | PL_HIST_NOEMPTY
and plplot_hist_style = plplot_hist_enum list
and plplot_run_level_enum =
  | PL_UNINITIALIZED
  | PL_INITIALIZED
  | PL_VIEWPORT_DEFINED
  | PL_WORLD_COORDINATES_DEFINED
and plplot_run_level = plplot_run_level_enum
and plplot_position_enum =
  | PL_POSITION_LEFT
  | PL_POSITION_RIGHT
  | PL_POSITION_TOP
  | PL_POSITION_BOTTOM
  | PL_POSITION_INSIDE
  | PL_POSITION_OUTSIDE
  | PL_POSITION_VIEWPORT
  | PL_POSITION_SUBPAGE
and plplot_position_opt = plplot_position_enum list
and plplot_legend_enum =
  | PL_LEGEND_NONE
  | PL_LEGEND_COLOR_BOX
  | PL_LEGEND_LINE
  | PL_LEGEND_SYMBOL
  | PL_LEGEND_TEXT_LEFT
  | PL_LEGEND_BACKGROUND
  | PL_LEGEND_BOUNDING_BOX
  | PL_LEGEND_ROW_MAJOR
and plplot_legend_opt = plplot_legend_enum list
and plplot_colorbar_enum =
  | PL_COLORBAR_LABEL_LEFT
  | PL_COLORBAR_LABEL_RIGHT
  | PL_COLORBAR_LABEL_TOP
  | PL_COLORBAR_LABEL_BOTTOM
  | PL_COLORBAR_IMAGE
  | PL_COLORBAR_SHADE
  | PL_COLORBAR_GRADIENT
  | PL_COLORBAR_CAP_NONE
  | PL_COLORBAR_CAP_LOW
  | PL_COLORBAR_CAP_HIGH
  | PL_COLORBAR_SHADE_LABEL
  | PL_COLORBAR_ORIENT_RIGHT
  | PL_COLORBAR_ORIENT_TOP
  | PL_COLORBAR_ORIENT_LEFT
  | PL_COLORBAR_ORIENT_BOTTOM
  | PL_COLORBAR_BACKGROUND
  | PL_COLORBAR_BOUNDING_BOX
and plplot_colorbar_opt = plplot_colorbar_enum list
and plplot_fci_family_enum =
  | PL_FCI_FAMILY_UNCHANGED
  | PL_FCI_SANS
  | PL_FCI_SERIF
  | PL_FCI_MONO
  | PL_FCI_SCRIPT
  | PL_FCI_SYMBOL
and plplot_fci_style_enum =
  | PL_FCI_STYLE_UNCHANGED
  | PL_FCI_UPRIGHT
  | PL_FCI_ITALIC
  | PL_FCI_OBLIQUE
and plplot_fci_weight_enum =
  | PL_FCI_WEIGHT_UNCHANGED
  | PL_FCI_MEDIUM
  | PL_FCI_BOLD
and plplot_draw_mode_enum =
  | PL_DRAWMODE_UNKNOWN
  | PL_DRAWMODE_DEFAULT
  | PL_DRAWMODE_REPLACE
  | PL_DRAWMODE_XOR
and nonzero_error_int = int

external pl_setcontlabelformat : int -> int -> unit
	= "camlidl_plplot_core_c_pl_setcontlabelformat"

external pl_setcontlabelparam : float -> float -> float -> int -> unit
	= "camlidl_plplot_core_c_pl_setcontlabelparam"

external pladv : int -> unit
	= "camlidl_plplot_core_c_pladv"

external plarc : float -> float -> float -> float -> float -> float -> float -> bool -> unit
	= "camlidl_plplot_core_c_plarc_bytecode" "camlidl_plplot_core_c_plarc"

external plaxes : float -> float -> string -> float -> int -> string -> float -> int -> unit
	= "camlidl_plplot_core_c_plaxes_bytecode" "camlidl_plplot_core_c_plaxes"

external plbin : float array -> float array -> plplot_bin_style -> unit
	= "camlidl_plplot_core_c_plbin"

external plbtime : float -> int * int * int * int * int * float
	= "camlidl_plplot_core_c_plbtime"

external plbop : unit -> unit
	= "camlidl_plplot_core_c_plbop"

external plbox : string -> float -> int -> string -> float -> int -> unit
	= "camlidl_plplot_core_c_plbox_bytecode" "camlidl_plplot_core_c_plbox"

external plbox3 : string -> string -> float -> int -> string -> string -> float -> int -> string -> string -> float -> int -> unit
	= "camlidl_plplot_core_c_plbox3_bytecode" "camlidl_plplot_core_c_plbox3"

external plcalc_world : float -> float -> float * float * int
	= "camlidl_plplot_core_c_plcalc_world"

external plclear : unit -> unit
	= "camlidl_plplot_core_c_plclear"

external plcol0 : int -> unit
	= "camlidl_plplot_core_c_plcol0"

external plcol1 : float -> unit
	= "camlidl_plplot_core_c_plcol1"

external plconfigtime : float -> float -> float -> int -> bool -> int -> int -> int -> int -> int -> float -> unit
	= "camlidl_plplot_core_c_plconfigtime_bytecode" "camlidl_plplot_core_c_plconfigtime"

external plcpstrm : int -> bool -> unit
	= "camlidl_plplot_core_c_plcpstrm"

external plctime : int -> int -> int -> int -> int -> float -> float
	= "camlidl_plplot_core_c_plctime_bytecode" "camlidl_plplot_core_c_plctime"

external plend : unit -> unit
	= "camlidl_plplot_core_c_plend"

external plend1 : unit -> unit
	= "camlidl_plplot_core_c_plend1"

external plenv : float -> float -> float -> float -> int -> int -> unit
	= "camlidl_plplot_core_c_plenv_bytecode" "camlidl_plplot_core_c_plenv"

external plenv0 : float -> float -> float -> float -> int -> int -> unit
	= "camlidl_plplot_core_c_plenv0_bytecode" "camlidl_plplot_core_c_plenv0"

external pleop : unit -> unit
	= "camlidl_plplot_core_c_pleop"

external plerrx : float array -> float array -> float array -> unit
	= "camlidl_plplot_core_c_plerrx"

external plerry : float array -> float array -> float array -> unit
	= "camlidl_plplot_core_c_plerry"

external plfamadv : unit -> unit
	= "camlidl_plplot_core_c_plfamadv"

external plfill : float array -> float array -> unit
	= "camlidl_plplot_core_c_plfill"

external plfill3 : float array -> float array -> float array -> unit
	= "camlidl_plplot_core_c_plfill3"

external plflush : unit -> unit
	= "camlidl_plplot_core_c_plflush"

external plfont : int -> unit
	= "camlidl_plplot_core_c_plfont"

external plfontld : int -> unit
	= "camlidl_plplot_core_c_plfontld"

external plgchr : unit -> float * float
	= "camlidl_plplot_core_c_plgchr"

external plgcmap1_range : unit -> float * float
	= "camlidl_plplot_core_c_plgcmap1_range"

external plgcol0 : int -> int * int * int
	= "camlidl_plplot_core_c_plgcol0"

external plgcol0a : int -> int * int * int * float
	= "camlidl_plplot_core_c_plgcol0a"

external plgcolbg : unit -> int * int * int
	= "camlidl_plplot_core_c_plgcolbg"

external plgcolbga : unit -> int * int * int * float
	= "camlidl_plplot_core_c_plgcolbga"

external plgcompression : unit -> int
	= "camlidl_plplot_core_c_plgcompression"

external plgdev : unit -> string
	= "camlidl_plplot_core_c_plgdev"

external plgdidev : unit -> float * float * float * float
	= "camlidl_plplot_core_c_plgdidev"

external plgdiori : unit -> float
	= "camlidl_plplot_core_c_plgdiori"

external plgdiplt : unit -> float * float * float * float
	= "camlidl_plplot_core_c_plgdiplt"

external plgdrawmode : unit -> plplot_draw_mode_enum
	= "camlidl_plplot_core_c_plgdrawmode"

external plgfci : unit -> int64
	= "camlidl_plplot_core_c_plgfci"

external plgfam : unit -> int * int * int
	= "camlidl_plplot_core_c_plgfam"

external plgfnam : unit -> string
	= "camlidl_plplot_core_c_plgfnam"

external plgfont : unit -> int * int * int
	= "camlidl_plplot_core_c_plgfont"

external plglevel : unit -> plplot_run_level
	= "camlidl_plplot_core_c_plglevel"

external plgpage : unit -> float * float * int * int * int * int
	= "camlidl_plplot_core_c_plgpage"

external plgra : unit -> unit
	= "camlidl_plplot_core_c_plgra"

external plgradient : float array -> float array -> float -> unit
	= "camlidl_plplot_core_c_plgradient"

external plgspa : unit -> float * float * float * float
	= "camlidl_plplot_core_c_plgspa"

external plgstrm : unit -> int
	= "camlidl_plplot_core_c_plgstrm"

external plgver : unit -> string
	= "camlidl_plplot_core_c_plgver"

external plgvpd : unit -> float * float * float * float
	= "camlidl_plplot_core_c_plgvpd"

external plgvpw : unit -> float * float * float * float
	= "camlidl_plplot_core_c_plgvpw"

external plgxax : unit -> int * int
	= "camlidl_plplot_core_c_plgxax"

external plgyax : unit -> int * int
	= "camlidl_plplot_core_c_plgyax"

external plgzax : unit -> int * int
	= "camlidl_plplot_core_c_plgzax"

external plhist : float array -> float -> float -> int -> plplot_hist_style -> unit
	= "camlidl_plplot_core_c_plhist"

external plhlsrgb : float -> float -> float -> float * float * float
	= "camlidl_plplot_core_c_plhlsrgb"

external plinit : unit -> unit
	= "camlidl_plplot_core_c_plinit"

external pljoin : float -> float -> float -> float -> unit
	= "camlidl_plplot_core_c_pljoin"

external pllab : string -> string -> string -> unit
	= "camlidl_plplot_core_c_pllab"

external pllightsource : float -> float -> float -> unit
	= "camlidl_plplot_core_c_pllightsource"

external plline : float array -> float array -> unit
	= "camlidl_plplot_core_c_plline"

external plline3 : float array -> float array -> float array -> unit
	= "camlidl_plplot_core_c_plline3"

external pllsty : int -> unit
	= "camlidl_plplot_core_c_pllsty"

external plmesh : float array -> float array -> float array array -> plplot3d_style -> unit
	= "camlidl_plplot_core_c_plmesh"

external plmeshc : float array -> float array -> float array array -> plplot3d_style -> float array -> unit
	= "camlidl_plplot_core_c_plmeshc"

external plmkstrm : unit -> int
	= "camlidl_plplot_core_c_plmkstrm"

external plmtex : string -> float -> float -> float -> string -> unit
	= "camlidl_plplot_core_c_plmtex"

external plmtex3 : string -> float -> float -> float -> string -> unit
	= "camlidl_plplot_core_c_plmtex3"

external plot3d : float array -> float array -> float array array -> plplot3d_style -> bool -> unit
	= "camlidl_plplot_core_c_plot3d"

external plot3dc : float array -> float array -> float array array -> plplot3d_style -> float array -> unit
	= "camlidl_plplot_core_c_plot3dc"

external plpat : int array -> int array -> unit
	= "camlidl_plplot_core_c_plpat"

external plpath : int -> float -> float -> float -> float -> unit
	= "camlidl_plplot_core_c_plpath"

external plpoin : float array -> float array -> int -> unit
	= "camlidl_plplot_core_c_plpoin"

external plpoin3 : float array -> float array -> float array -> int -> unit
	= "camlidl_plplot_core_c_plpoin3"

external plprec : int -> int -> unit
	= "camlidl_plplot_core_c_plprec"

external plpsty : int -> unit
	= "camlidl_plplot_core_c_plpsty"

external plptex : float -> float -> float -> float -> float -> string -> unit
	= "camlidl_plplot_core_c_plptex_bytecode" "camlidl_plplot_core_c_plptex"

external plptex3 : float -> float -> float -> float -> float -> float -> float -> float -> float -> float -> string -> unit
	= "camlidl_plplot_core_c_plptex3_bytecode" "camlidl_plplot_core_c_plptex3"

external plrandd : unit -> float
	= "camlidl_plplot_core_c_plrandd"

external plreplot : unit -> unit
	= "camlidl_plplot_core_c_plreplot"

external plrgbhls : float -> float -> float -> float * float * float
	= "camlidl_plplot_core_c_plrgbhls"

external plschr : float -> float -> unit
	= "camlidl_plplot_core_c_plschr"

external plscmap0 : int array -> int array -> int array -> unit
	= "camlidl_plplot_core_c_plscmap0"

external plscmap0a : int array -> int array -> int array -> float array -> unit
	= "camlidl_plplot_core_c_plscmap0a"

external plscmap0n : int -> unit
	= "camlidl_plplot_core_c_plscmap0n"

external plscmap1 : int array -> int array -> int array -> unit
	= "camlidl_plplot_core_c_plscmap1"

external plscmap1a : int array -> int array -> int array -> float array -> unit
	= "camlidl_plplot_core_c_plscmap1a"

external plscmap1l : bool -> float array -> float array -> float array -> float array -> bool array option -> unit
	= "camlidl_plplot_core_c_plscmap1l_bytecode" "camlidl_plplot_core_c_plscmap1l"

external plscmap1la : bool -> float array -> float array -> float array -> float array -> float array -> bool array option -> unit
	= "camlidl_plplot_core_c_plscmap1la_bytecode" "camlidl_plplot_core_c_plscmap1la"

external plscmap1n : int -> unit
	= "camlidl_plplot_core_c_plscmap1n"

external plscmap1_range : float -> float -> unit
	= "camlidl_plplot_core_c_plscmap1_range"

external plscol0 : int -> int -> int -> int -> unit
	= "camlidl_plplot_core_c_plscol0"

external plscol0a : int -> int -> int -> int -> float -> unit
	= "camlidl_plplot_core_c_plscol0a"

external plscolbg : int -> int -> int -> unit
	= "camlidl_plplot_core_c_plscolbg"

external plscolbga : int -> int -> int -> float -> unit
	= "camlidl_plplot_core_c_plscolbga"

external plscolor : int -> unit
	= "camlidl_plplot_core_c_plscolor"

external plscompression : int -> unit
	= "camlidl_plplot_core_c_plscompression"

external plsdev : string -> unit
	= "camlidl_plplot_core_c_plsdev"

external plsdidev : float -> float -> float -> float -> unit
	= "camlidl_plplot_core_c_plsdidev"

external plsdimap : int -> int -> int -> int -> float -> float -> unit
	= "camlidl_plplot_core_c_plsdimap_bytecode" "camlidl_plplot_core_c_plsdimap"

external plsdiori : float -> unit
	= "camlidl_plplot_core_c_plsdiori"

external plsdiplt : float -> float -> float -> float -> unit
	= "camlidl_plplot_core_c_plsdiplt"

external plsdiplz : float -> float -> float -> float -> unit
	= "camlidl_plplot_core_c_plsdiplz"

external plseed : int64 -> unit
	= "camlidl_plplot_core_c_plseed"

external plsesc : char -> unit
	= "camlidl_plplot_core_c_plsesc"

external plsfam : int -> int -> int -> unit
	= "camlidl_plplot_core_c_plsfam"

external plsfci : int64 -> unit
	= "camlidl_plplot_core_c_plsfci"

external plsfnam : string -> unit
	= "camlidl_plplot_core_c_plsfnam"

external plsfont : plplot_fci_family_enum -> plplot_fci_style_enum -> plplot_fci_weight_enum -> unit
	= "camlidl_plplot_core_c_plsfont"

external plsmaj : float -> float -> unit
	= "camlidl_plplot_core_c_plsmaj"

external plsmin : float -> float -> unit
	= "camlidl_plplot_core_c_plsmin"

external plsdrawmode : plplot_draw_mode_enum -> unit
	= "camlidl_plplot_core_c_plsdrawmode"

external plsori : int -> unit
	= "camlidl_plplot_core_c_plsori"

external plspage : float -> float -> int -> int -> int -> int -> unit
	= "camlidl_plplot_core_c_plspage_bytecode" "camlidl_plplot_core_c_plspage"

external plspal0 : string -> unit
	= "camlidl_plplot_core_c_plspal0"

external plspal1 : string -> bool -> unit
	= "camlidl_plplot_core_c_plspal1"

external plspause : bool -> unit
	= "camlidl_plplot_core_c_plspause"

external plsstrm : int -> unit
	= "camlidl_plplot_core_c_plsstrm"

external plssub : int -> int -> unit
	= "camlidl_plplot_core_c_plssub"

external plssym : float -> float -> unit
	= "camlidl_plplot_core_c_plssym"

external plstar : int -> int -> unit
	= "camlidl_plplot_core_c_plstar"

external plstart : string -> int -> int -> unit
	= "camlidl_plplot_core_c_plstart"

external plstring : float array -> float array -> string -> unit
	= "camlidl_plplot_core_c_plstring"

external plstring3 : float array -> float array -> float array -> string -> unit
	= "camlidl_plplot_core_c_plstring3"

external plstripa : int -> int -> float -> float -> unit
	= "camlidl_plplot_core_c_plstripa"

external plstripd : int -> unit
	= "camlidl_plplot_core_c_plstripd"

external plimage : float array array -> float -> float -> float -> float -> float -> float -> float -> float -> float -> float -> unit
	= "camlidl_plplot_core_c_plimage_bytecode" "camlidl_plplot_core_c_plimage"

external plstyl : int array -> int array -> unit
	= "camlidl_plplot_core_c_plstyl"

external plsurf3d : float array -> float array -> float array array -> plplot3d_style -> float array -> unit
	= "camlidl_plplot_core_c_plsurf3d"

external plsvect : float array -> float array -> bool -> unit
	= "camlidl_plplot_core_c_plsvect"

external plsvpa : float -> float -> float -> float -> unit
	= "camlidl_plplot_core_c_plsvpa"

external plsxax : int -> int -> unit
	= "camlidl_plplot_core_c_plsxax"

external plsxwin : int -> unit
	= "camlidl_plplot_core_plsxwin"

external plsyax : int -> int -> unit
	= "camlidl_plplot_core_c_plsyax"

external plsym : float array -> float array -> int -> unit
	= "camlidl_plplot_core_c_plsym"

external plszax : int -> int -> unit
	= "camlidl_plplot_core_c_plszax"

external pltext : unit -> unit
	= "camlidl_plplot_core_c_pltext"

external pltimefmt : string -> unit
	= "camlidl_plplot_core_c_pltimefmt"

external plvasp : float -> unit
	= "camlidl_plplot_core_c_plvasp"

external plvpas : float -> float -> float -> float -> float -> unit
	= "camlidl_plplot_core_c_plvpas"

external plvpor : float -> float -> float -> float -> unit
	= "camlidl_plplot_core_c_plvpor"

external plvsta : unit -> unit
	= "camlidl_plplot_core_c_plvsta"

external plw3d : float -> float -> float -> float -> float -> float -> float -> float -> float -> float -> float -> unit
	= "camlidl_plplot_core_c_plw3d_bytecode" "camlidl_plplot_core_c_plw3d"

external plwidth : float -> unit
	= "camlidl_plplot_core_c_plwidth"

external plwind : float -> float -> float -> float -> unit
	= "camlidl_plplot_core_c_plwind"

external plxormod : bool -> bool
	= "camlidl_plplot_core_c_plxormod"

external plsetopt : string -> string -> unit
	= "camlidl_plplot_core_c_plsetopt"

external plMinMax2dGrid : float array array -> float * float
	= "camlidl_plplot_core_plMinMax2dGrid"

external plcont : float array array -> int -> int -> int -> int -> float array -> unit
	= "camlidl_plplot_core_ml_plcont_bytecode" "camlidl_plplot_core_ml_plcont"

external plshade : float array array -> float -> float -> float -> float -> float -> float -> int -> float -> float -> int -> float -> int -> float -> bool -> unit
	= "camlidl_plplot_core_ml_plshade_bytecode" "camlidl_plplot_core_ml_plshade"

external plshades : float array array -> float -> float -> float -> float -> float array -> float -> int -> float -> bool -> unit
	= "camlidl_plplot_core_ml_plshades_bytecode" "camlidl_plplot_core_ml_plshades"

external plimagefr : float array array -> float -> float -> float -> float -> float -> float -> float -> float -> unit
	= "camlidl_plplot_core_ml_plimagefr_bytecode" "camlidl_plplot_core_ml_plimagefr"

external plvect : float array array -> float array array -> float -> unit
	= "camlidl_plplot_core_ml_plvect"

external plmap : string -> float -> float -> float -> float -> unit
	= "camlidl_plplot_core_ml_plmap"

external plmeridians : float -> float -> float -> float -> float -> float -> unit
	= "camlidl_plplot_core_ml_plmeridians_bytecode" "camlidl_plplot_core_ml_plmeridians"

external plpoly3 : float array -> float array -> float array -> bool array -> bool -> unit
	= "camlidl_plplot_core_ml_plpoly3"

external pltr0 : float -> float -> float * float
	= "camlidl_plplot_core_ml_pltr0"

external plsvect_reset : unit -> unit
	= "camlidl_plplot_core_ml_plsvect_reset"

external plg_current_col0 : unit -> int
	= "camlidl_plplot_core_plg_current_col0"

external plg_current_col1 : unit -> float
	= "camlidl_plplot_core_plg_current_col1"

external plgwidth : unit -> float
	= "camlidl_plplot_core_plgwidth"

external plgchrht : unit -> float
	= "camlidl_plplot_core_plgchrht"

external plstripc : string -> string -> float -> float -> float -> float ->                          float -> float -> float -> bool -> bool -> int -> int ->                          int array -> int array -> string array -> string ->                          string -> string -> int = "ml_plstripc_byte" "ml_plstripc"
external pltr1 : float array -> float array -> float -> float -> float * float     = "ml_pltr1"
external pltr2 : float array array -> float array array -> float -> float -> float * float     = "ml_pltr2"
let plset_pltr (f : float -> float -> (float * float)) =     Callback.register "caml_plplot_plotter" f
let plunset_pltr () = Callback.register "caml_plplot_plotter" 0
let plset_mapform (f : float -> float -> (float * float)) =     Callback.register "caml_plplot_mapform" f
let plunset_mapform () = Callback.register "caml_plplot_mapform" 0
let plset_defined (f : float -> float -> int) =  Callback.register "caml_plplot_defined" f
let plunset_defined () = Callback.register "caml_plplot_defined" 0
external ml_plstransform : unit -> unit = "ml_plstransform"
let plstransform (f : float -> float -> (float * float)) =     Callback.register "caml_plplot_transform" f;     ml_plstransform ()
let plunset_transform () =    Callback.register "caml_plplot_transform" 0;    ml_plstransform ()
type plplot_grid_method_type =                 PL_GRID_CSA |                 PL_GRID_DTLI |                 PL_GRID_NNI |                 PL_GRID_NNIDW |                 PL_GRID_NNLI |                 PL_GRID_NNAIDW
type plplot_parse_method_type =                 PL_PARSE_PARTIAL |                 PL_PARSE_FULL |                 PL_PARSE_QUIET |                 PL_PARSE_NODELETE |                 PL_PARSE_SHOWALL |                 PL_PARSE_OVERRIDE |                 PL_PARSE_NOPROGRAM |                 PL_PARSE_NODASH |                 PL_PARSE_SKIP
type plplot_axis_type =                 PL_X_AXIS |                 PL_Y_AXIS |                 PL_Z_AXIS
external ml_plslabelfunc : unit -> unit = "ml_plslabelfunc"
let plslabelfunc (f : plplot_axis_type -> float -> string) =  Callback.register "caml_plplot_customlabel" f;  ml_plslabelfunc ()
let plunset_labelfunc () =  Callback.register "caml_plplot_customlabel" 0;  ml_plslabelfunc ()
external ml_plsabort : unit -> unit = "ml_plsabort"
let plsabort (f : string -> unit) =  Callback.register "caml_plplot_abort" f;  ml_plsabort ()
let plunset_abort () =  Callback.register "caml_plplot_abort" 0;  ml_plsabort ()
external ml_plsexit : unit -> unit = "ml_plsexit"
let plsexit (f : string -> int) =  Callback.register "caml_plplot_exit" f;  ml_plsexit ()
let plunset_exit () =  Callback.register "caml_plplot_exit" 0;  ml_plsexit ()
external plgriddata : float array -> float array -> float array -> float array -> float array -> plplot_grid_method_type -> float -> float array array = "ml_plgriddata_bytecode" "ml_plgriddata"
external plparseopts : string array -> plplot_parse_method_type list -> unit = "ml_plparseopts"
external pllegend : plplot_legend_opt -> plplot_position_opt -> float -> float -> float -> int -> int -> int -> int -> int -> plplot_legend_opt array -> float -> float -> float -> float -> int array -> string array -> int array -> int array -> float array -> float array -> int array -> int array -> float array -> int array -> float array -> int array -> string array -> float * float = "ml_pllegend_byte" "ml_pllegend"
external plcolorbar : plplot_colorbar_opt -> plplot_position_opt -> float -> float -> float -> float -> int -> int -> int -> float -> float -> int -> float -> plplot_colorbar_opt array -> string array -> string array -> float array -> int array -> float array array -> float * float = "ml_plcolorbar_byte" "ml_plcolorbar"
