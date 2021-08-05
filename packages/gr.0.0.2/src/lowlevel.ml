open Ctypes
open Foreign

(* version >= 0.37.0 *)

(* The API is documented here: https://gr-framework.org/c-gr.html *)
(* See also https://gr-framework.org/about.html - at a certain point it would be nice to also have bindings for GKS and GR3 *)

(** {1} Lowlevel API bindings *)
let libGRpath =
  let path =
    try Sys.getenv "LIBGRPATH"
    with Not_found -> (
      try Sys.getenv "GRDIR" ^ "/lib/" with Not_found -> "")
  in
  let lib = path ^ "libGR." in
  (* brittle, temporary until we move to use generated stubs *)
  let ext = if Sys.file_exists (lib ^ "dylib") then "dylib" else "so" in
  lib ^ ext

let libGR = Dl.dlopen ~flags:[ Dl.RTLD_LAZY ] ~filename:libGRpath

let foreign = foreign ~from:libGR

type vertex_t

let vertex : vertex_t structure typ = structure "vertex_t"

let vertex_x = field vertex "x" double

let vertex_y = field vertex "y" double

let () = seal vertex

let opengks = foreign "gr_opengks" (void @-> returning void)

let closegks = foreign "gr_closegks" (void @-> returning void)

let inqdspsize =
  foreign "gr_inqdspsize"
    (ptr double @-> ptr double @-> ptr int @-> ptr int @-> returning void)

(* void let openws = foreign "gr_openws" (int workstation_id @-> ptr charconnection @-> int type) 
      workstation_id: a workstation identifier
      connection: a connection identifier
      type: the desired workstation type
*)
let openws = foreign "gr_openws" (int @-> string @-> int @-> returning void)

let closews = foreign "gr_closews" (int @-> returning void)

let activatews = foreign "gr_activatews" (int @-> returning void)

let deactivatews = foreign "gr_deactivatews" (int @-> returning void)

let clearws = foreign "gr_clearws" (void @-> returning void)

let updatews = foreign "gr_updatews" (void @-> returning void)

(* let polyline = foreign "gr_polyline" (int n @-> ptr doublex @-> ptr doubley)
   Draw a polyline using the current line attributes @-> starting from the first data point and ending at the last data point.
   The values for x and y are in world coordinates.
   The attributes that control the appearance of a polyline are linetype @-> linewidth and color index.

   Parameters
        n: The number of points
        x: A pointer to the X coordinates
        y: A pointer to the Y coordinates
*)
let polyline =
  foreign "gr_polyline" (int @-> ptr double @-> ptr double @-> returning void)

let polymarker =
  foreign "gr_polymarker" (int @-> ptr double @-> ptr double @-> returning void)

let text = foreign "gr_text" (double @-> double @-> string @-> returning void)

(*
(* Can be nasty to deal with:
  https://discuss.ocaml.org/t/ctypes-pass-ocaml-bytes-to-c-functions/2156/4
  http://lists.ocaml.org/pipermail/ctypes/2017-December/000248.html *)
let inqtext =
  foreign
    "gr_inqtext"
    (double @-> double @-> ptr char @-> ptr double @-> ptr double @-> returning void)
*)

let fillarea =
  foreign "gr_fillarea" (int @-> ptr double @-> ptr double @-> returning void)

let cellarray =
  foreign "gr_cellarray"
    (double @-> double @-> double @-> double @-> int @-> int @-> int @-> int
   @-> int @-> int @-> ptr int @-> returning void)

let gdp =
  foreign "gr_gdp"
    (int @-> ptr double @-> ptr double @-> int @-> int @-> ptr int
   @-> returning void)

let spline =
  foreign "gr_spline"
    (int @-> ptr double @-> ptr double @-> int @-> int @-> returning void)

let gridit =
  foreign "gr_gridit"
    (int @-> ptr double @-> ptr double @-> ptr double @-> int @-> int
   @-> ptr double @-> ptr double @-> ptr double @-> returning void)

let setlinetype = foreign "gr_setlinetype" (int @-> returning void)

(* let inqlinetype = foreign "gr_inqlinetype" (ptr int @-> returning void) *)
let setlinewidth = foreign "gr_setlinewidth" (double @-> returning void)

(* let inqlinewidth = foreign "gr_inqlinewidth" (ptr double @-> returning void) *)
let setlinecolorind = foreign "gr_setlinecolorind" (int @-> returning void)

(* let inqlinecolorind = foreign "gr_inqlinecolorind" (ptr int @-> returning void) *)
let setmarkertype = foreign "gr_setmarkertype" (int @-> returning void)

(* let inqmarkertype = foreign "gr_inqmarkertype" (ptr int @-> returning void) *)
let setmarkersize = foreign "gr_setmarkersize" (double @-> returning void)

let setmarkercolorind = foreign "gr_setmarkercolorind" (int @-> returning void)

(* let inqmarkercolorind = foreign "gr_inqmarkercolorind" (ptr int @-> returning void) *)
let settextfontprec =
  foreign "gr_settextfontprec" (int @-> int @-> returning void)

let setcharexpan = foreign "gr_setcharexpan" (double @-> returning void)

let setcharspace = foreign "gr_setcharspace" (double @-> returning void)

let settextcolorind = foreign "gr_settextcolorind" (int @-> returning void)

let setcharheight = foreign "gr_setcharheight" (double @-> returning void)

let setcharup = foreign "gr_setcharup" (double @-> double @-> returning void)

let settextpath = foreign "gr_settextpath" (int @-> returning void)

let settextalign = foreign "gr_settextalign" (int @-> int @-> returning void)

let setfillintstyle = foreign "gr_setfillintstyle" (int @-> returning void)

let setfillstyle = foreign "gr_setfillstyle" (int @-> returning void)

let setfillcolorind = foreign "gr_setfillcolorind" (int @-> returning void)

let setcolorrep =
  foreign "gr_setcolorrep"
    (int @-> double @-> double @-> double @-> returning void)

let setwindow =
  foreign "gr_setwindow"
    (double @-> double @-> double @-> double @-> returning void)

let inqwindow =
  foreign "gr_inqwindow"
    (ptr double @-> ptr double @-> ptr double @-> ptr double @-> returning void)

let setviewport =
  foreign "gr_setviewport"
    (double @-> double @-> double @-> double @-> returning void)

let inqviewport =
  foreign "gr_inqviewport"
    (ptr double @-> ptr double @-> ptr double @-> ptr double @-> returning void)

let selntran = foreign "gr_selntran" (int @-> returning void)

let setclip = foreign "gr_setclip" (int @-> returning void)

let setwswindow =
  foreign "gr_setwswindow"
    (double @-> double @-> double @-> double @-> returning void)

let setwsviewport =
  foreign "gr_setwsviewport"
    (double @-> double @-> double @-> double @-> returning void)

let createseg = foreign "gr_createseg" (int @-> returning void)

let copysegws = foreign "gr_copysegws" (int @-> returning void)

let redrawsegws = foreign "gr_redrawsegws" (void @-> returning void)

let setsegtran =
  foreign "gr_setsegtran"
    (int @-> double @-> double @-> double @-> double @-> double @-> double
   @-> double @-> returning void)

let closeseg = foreign "gr_closeseg" (void @-> returning void)

let emergencyclosegks = foreign "gr_emergencyclosegks" (void @-> returning void)

let updategks = foreign "gr_updategks" (void @-> returning void)

let setspace =
  foreign "gr_setspace" (double @-> double @-> int @-> int @-> returning int)

let inqspace =
  foreign "gr_inqspace"
    (ptr double @-> ptr double @-> ptr int @-> ptr int @-> returning void)

let setscale = foreign "gr_setscale" (int @-> returning int)

let inqscale = foreign "gr_inqscale" (ptr int @-> returning void)

let textext =
  foreign "gr_textext" (double @-> double @-> string @-> returning int)

let inqtextext =
  foreign "gr_inqtextext"
    (double @-> double @-> string @-> ptr double @-> ptr double
   @-> returning void)

(*
    void let axes = foreign "gr_axes" (double x_tick @-> double y_tick @-> double x_org @-> double y_org @-> int major_x @-> int major_y @-> double tick_size)

    Draw X and Y coordinate axes with linearly and/or logarithmically spaced tick marks.
    Tick marks are positioned along each axis so that major tick marks fall on the axes origin (whether visible or not).
    Major tick marks are labeled with the corresponding data values.
    Axes are drawn according to the scale of the window.
    Axes and tick marks are drawn using solid lines; line color and width can be modified using the let setlinetype = foreign "gr_setlinetype"  and let setlinewidth = foreign "gr_setlinewidth"  functions.
    Axes are drawn according to the linear or logarithmic transformation established by the let setscale = foreign "gr_setscale"  function.

    Parameters
        x_tick: The interval between minor tick marks on the X axis.
        y_tick: The interval between minor tick marks on the Y axis.
        x_org: The world coordinate of the origin (point of intersection) of the X axis.
        y_org: The world coordinate of the origin (point of intersection) of the Y axis.
        major_x: Unitless integer value specifying the number of minor tick intervals between major tick marks on the X axis. Values of 0 or 1 imply no minor ticks. Negative values specify no labels will be drawn for the associated axis.
        major_y: Unitless integer value specifying the number of minor tick intervals between major tick marks on the Y axis. Values of 0 or 1 imply no minor ticks. Negative values specify no labels will be drawn for the associated axis.
        tick_size: The length of minor tick marks specified in a normalized device coordinate unit. Major tick marks are twice as long as minor tick marks. A negative value reverses the tick marks on the axes from inward facing to outward facing (or vice versa)
    *)
let axes =
  foreign "gr_axes"
    (double @-> double @-> double @-> double @-> int @-> int @-> double
   @-> returning void)

let axeslbl =
  foreign "gr_axeslbl"
    (double @-> double @-> double @-> double @-> int @-> int @-> double
    @-> funptr (double @-> double @-> string @-> double @-> returning void)
    @-> funptr (double @-> double @-> string @-> double @-> returning void)
    @-> returning void)

let grid =
  foreign "gr_grid"
    (double @-> double @-> double @-> double @-> int @-> int @-> returning void)

let grid3d =
  foreign "gr_grid3d"
    (double @-> double @-> double @-> double @-> double @-> double @-> int
   @-> int @-> int @-> returning void)

let verrorbars =
  foreign "gr_verrorbars"
    (int @-> ptr double @-> ptr double @-> ptr double @-> ptr double
   @-> returning void)

let herrorbars =
  foreign "gr_herrorbars"
    (int @-> ptr double @-> ptr double @-> ptr double @-> ptr double
   @-> returning void)

let polyline3d =
  foreign "gr_polyline3d"
    (int @-> ptr double @-> ptr double @-> ptr double @-> returning void)

let polymarker3d =
  foreign "gr_polymarker3d"
    (int @-> ptr double @-> ptr double @-> ptr double @-> returning void)

let axes3d =
  foreign "gr_axes3d"
    (double @-> double @-> double @-> double @-> double @-> double @-> int
   @-> int @-> int @-> double @-> returning void)

let titles3d =
  foreign "gr_titles3d" (string @-> string @-> string @-> returning void)

let surface =
  foreign "gr_surface"
    (int @-> int @-> ptr double @-> ptr double @-> ptr double @-> int
   @-> returning void)

let contour =
  foreign "gr_contour"
    (int @-> int @-> int @-> ptr double @-> ptr double @-> ptr double
   @-> ptr double @-> int @-> returning void)

let contourf =
  foreign "gr_contourf"
    (int @-> int @-> int @-> ptr double @-> ptr double @-> ptr double
   @-> ptr double @-> int @-> returning void)

let tricontour =
  foreign "gr_tricontour"
    (int @-> ptr double @-> ptr double @-> ptr double @-> int @-> ptr double
   @-> returning void)

let hexbin =
  foreign "gr_hexbin"
    (int @-> ptr double @-> ptr double @-> int @-> returning int)

let setcolormap = foreign "gr_setcolormap" (int @-> returning void)

let inqcolormap = foreign "gr_inqcolormap" (ptr int @-> returning void)

let colorbar = foreign "gr_colorbar" (void @-> returning void)

let inqcolor = foreign "gr_inqcolor" (int @-> ptr int @-> returning void)

let inqcolorfromrgb =
  foreign "gr_inqcolorfromrgb" (double @-> double @-> double @-> returning int)

let hsvtorgb =
  foreign "gr_hsvtorgb"
    (double @-> double @-> double @-> ptr double @-> ptr double @-> ptr double
   @-> returning void)

let tick = foreign "gr_tick" (double @-> double @-> returning double)

let validaterange =
  foreign "gr_validaterange" (double @-> double @-> returning int)

let adjustlimits =
  foreign "gr_adjustlimits" (ptr double @-> ptr double @-> returning void)

let adjustrange =
  foreign "gr_adjustrange" (ptr double @-> ptr double @-> returning void)

let beginprint = foreign "gr_beginprint" (string @-> returning void)

let beginprintext =
  foreign "gr_beginprintext"
    (string @-> string @-> string @-> string @-> returning void)

let endprint = foreign "gr_endprint" (void @-> returning void)

let ndctowc = foreign "gr_ndctowc" (ptr double @-> ptr double @-> returning void)

let wctondc = foreign "gr_wctondc" (ptr double @-> ptr double @-> returning void)

let wc3towc =
  foreign "gr_wc3towc"
    (ptr double @-> ptr double @-> ptr double @-> returning void)

let drawrect =
  foreign "gr_drawrect"
    (double @-> double @-> double @-> double @-> returning void)

let fillrect =
  foreign "gr_fillrect"
    (double @-> double @-> double @-> double @-> returning void)

let drawarc =
  foreign "gr_drawarc"
    (double @-> double @-> double @-> double @-> int @-> int @-> returning void)

let fillarc =
  foreign "gr_fillarc"
    (double @-> double @-> double @-> double @-> int @-> int @-> returning void)

let drawpath =
  foreign "gr_drawpath"
    (int @-> ptr vertex @-> ptr uchar @-> int @-> returning void)

let setarrowstyle = foreign "gr_setarrowstyle" (int @-> returning void)

let setarrowsize = foreign "gr_setarrowsize" (double @-> returning void)

let drawarrow =
  foreign "gr_drawarrow"
    (double @-> double @-> double @-> double @-> returning void)

let readimage =
  foreign "gr_readimage"
    (string @-> ptr int @-> ptr int @-> ptr (ptr int) @-> returning int)

let drawimage =
  foreign "gr_drawimage"
    (double @-> double @-> double @-> double @-> int @-> int @-> ptr int @-> int
   @-> returning void)

let importgraphics = foreign "gr_importgraphics" (string @-> returning int)

let setshadow =
  foreign "gr_setshadow" (double @-> double @-> double @-> returning void)

let settransparency = foreign "gr_settransparency" (double @-> returning void)

(* This should be an actual double[3][2] *)
let setcoordxform =
  foreign "gr_setcoordxform" (ptr (ptr double) @-> returning void)

let begingraphics = foreign "gr_begingraphics" (string @-> returning void)

let endgraphics = foreign "gr_endgraphics" (void @-> returning void)

let getgraphics = foreign "gr_getgraphics" (void @-> returning string)

let drawgraphics = foreign "gr_drawgraphics" (string @-> returning int)

let mathtex =
  foreign "gr_mathtex" (double @-> double @-> string @-> returning void)

let inqmathtex =
  foreign "gr_inqmathtex"
    (double @-> double @-> string @-> ptr double @-> ptr double
   @-> returning void)

let beginselection = foreign "gr_beginselection" (int @-> int @-> returning void)

let endselection = foreign "gr_endselection" (void @-> returning void)

let moveselection =
  foreign "gr_moveselection" (double @-> double @-> returning void)

let resizeselection =
  foreign "gr_resizeselection" (int @-> double @-> double @-> returning void)

let inqbbox =
  foreign "gr_inqbbox"
    (ptr double @-> ptr double @-> ptr double @-> ptr double @-> returning void)

let precision = foreign "gr_precision" (void @-> returning double)

let setregenflags = foreign "gr_setregenflags" (int @-> returning void)

let inqregenflags = foreign "gr_inqregenflags" (void @-> returning int)

let savestate = foreign "gr_savestate" (void @-> returning void)

let restorestate = foreign "gr_restorestate" (void @-> returning void)

let selectcontext = foreign "gr_selectcontext" (int @-> returning void)

let destroycontext = foreign "gr_destroycontext" (int @-> returning void)

let uselinespec = foreign "gr_uselinespec" (string @-> returning int)

let delaunay =
  foreign "gr_delaunay"
    (int @-> ptr double @-> ptr double @-> ptr int
    @-> ptr (ptr int)
    @-> returning void)

let reducepoints =
  foreign "gr_reducepoints"
    (int @-> ptr double @-> ptr double @-> int @-> ptr double @-> ptr double
   @-> returning void)

let trisurface =
  foreign "gr_trisurface"
    (int @-> ptr double @-> ptr double @-> ptr double @-> returning void)

let gradient =
  foreign "gr_gradient"
    (int @-> int @-> ptr double @-> ptr double @-> ptr double @-> ptr double
   @-> ptr double @-> returning void)

let quiver =
  foreign "gr_quiver"
    (int @-> int @-> ptr double @-> ptr double @-> ptr double @-> ptr double
   @-> int @-> returning void)

let interp2 =
  foreign "gr_interp2"
    (int @-> int @-> ptr double @-> ptr double @-> ptr double @-> int @-> int
   @-> ptr double @-> ptr double @-> ptr double @-> int @-> double
   @-> returning void)

(* 
    (* omitting meta_args_t for now *)
    DLLEXPORT gr_meta_args_t *gr_newmeta(void);
    DLLEXPORT void gr_deletemeta(gr_meta_args_t * );
  DLLEXPORT void gr_meta_args_push_arg(gr_meta_args_t *, const char *, ..);
  DLLEXPORT void gr_meta_args_push_arg_buf(
    gr_meta_args_t *, const char *, const void *, int);
  DLLEXPORT void gr_meta_args_push_kwarg(
    gr_meta_args_t *, const char *, const char *, .. );
  DLLEXPORT void gr_meta_args_push_kwarg_buf(
    gr_meta_args_t *, const char *, const char *, const void *, int);
  DLLEXPORT void *gr_openmeta(
    int, const char *, unsigned int, const char *( * )(const char *, unsigned int),
    int ( * )(const char *, unsigned int, const char * ));
  DLLEXPORT gr_meta_args_t *gr_recvmeta(const void *p, gr_meta_args_t * );
  DLLEXPORT int gr_sendmeta(const void *, const char *, .. );
  DLLEXPORT int gr_sendmeta_buf(const void *, const char *, const void *, int);
  DLLEXPORT int gr_sendmeta_ref(const void *, const char *, char, const void *, int);
  DLLEXPORT int gr_sendmeta_args(const void *p, const gr_meta_args_t * );
  DLLEXPORT void gr_closemeta(const void * );
  DLLEXPORT int gr_plotmeta(const gr_meta_args_t * );
  DLLEXPORT int gr_readmeta(gr_meta_args_t *, const char * );
  #ifndef NDEBUG
    DLLEXPORT void gr_dumpmeta(const gr_meta_args_t *, FILE * );
  DLLEXPORT void gr_dumpmeta_json(const gr_meta_args_t *, FILE * );
  #endif
  *)
let version = foreign "gr_version" (void @-> returning string)

let shade =
  foreign "gr_shade"
    (int @-> ptr double @-> ptr double @-> int @-> int @-> ptr double @-> int
   @-> int @-> ptr int @-> returning void)

let shadepoints =
  foreign "gr_shadepoints"
    (int @-> ptr double @-> ptr double @-> int @-> int @-> int
   @-> returning void)

let shadelines =
  foreign "gr_shadelines"
    (int @-> ptr double @-> ptr double @-> int @-> int @-> int
   @-> returning void)

let panzoom =
  foreign "gr_panzoom"
    (double @-> double @-> double @-> ptr double @-> ptr double @-> ptr double
   @-> ptr double @-> returning void)

(** {2} Helpers *)

let get_size_and_pointers x y =
  let xd = Bigarray.Genarray.dims x in
  let yd = Bigarray.Genarray.dims y in
  let n =
    match (xd, yd) with
    | [| lx |], [| ly |] when lx = ly -> lx
    | [| 1; lx |], [| 1; ly |] when lx = ly -> lx
    | [| lx; 1 |], [| ly; 1 |] when lx = ly -> lx
    | [| lx |], [| ly |] ->
        raise
        @@ Invalid_argument
             (Printf.sprintf "Arrays of different lenghts: %d, %d" lx ly)
    | [| 1; lx |], [| 1; ly |] ->
        raise
        @@ Invalid_argument
             (Printf.sprintf "Arrays of different lenghts: %d, %d" lx ly)
    | [| lx; 1 |], [| ly; 1 |] ->
        raise
        @@ Invalid_argument
             (Printf.sprintf "Arrays of different lenghts: %d, %d" lx ly)
    | _ ->
        raise @@ Invalid_argument (Printf.sprintf "Incompatible arrays shape")
  in
  let x = Ctypes.(bigarray_start genarray x) in
  let y = Ctypes.(bigarray_start genarray y) in
  (n, x, y)

let get_size_and_pointer x =
  let xd = Bigarray.Genarray.dims x in
  let n =
    match xd with
    | [| lx |] -> lx
    | [| 1; lx |] -> lx
    | [| lx; 1 |] -> lx
    | _ -> raise @@ Invalid_argument "Array of invalid dimension"
  in
  let x = Ctypes.(bigarray_start genarray x) in
  (n, x)
