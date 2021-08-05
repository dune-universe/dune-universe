(**
{1 Lowlevel bindings to the GR-framewokr C interface.}

For the documentation refer to the {{: https://gr-framework.org/c-gr.html} GR Reference}.

The lookup of the "libGR.so" library can be overriden providing the full path to the library so file by setting the [LIBGRPATH] env variable.
*)

type vertex_t

val vertex : vertex_t Ctypes.structure Ctypes.typ

val vertex_x : (float, vertex_t Ctypes.structure) Ctypes.field

val vertex_y : (float, vertex_t Ctypes.structure) Ctypes.field

val opengks : unit -> unit

val closegks : unit -> unit

val inqdspsize :
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  int Ctypes_static.ptr ->
  int Ctypes_static.ptr ->
  unit

val openws : int -> string -> int -> unit

val closews : int -> unit

val activatews : int -> unit

val deactivatews : int -> unit

val clearws : unit -> unit

val updatews : unit -> unit

val polyline : int -> float Ctypes_static.ptr -> float Ctypes_static.ptr -> unit

val polymarker :
  int -> float Ctypes_static.ptr -> float Ctypes_static.ptr -> unit

val text : float -> float -> string -> unit

val fillarea : int -> float Ctypes_static.ptr -> float Ctypes_static.ptr -> unit

val cellarray :
  float ->
  float ->
  float ->
  float ->
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  int Ctypes_static.ptr ->
  unit

val gdp :
  int ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  int ->
  int ->
  int Ctypes_static.ptr ->
  unit

val spline :
  int ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  int ->
  int ->
  unit

val gridit :
  int ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  int ->
  int ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  unit

val setlinetype : int -> unit

val setlinewidth : float -> unit

val setlinecolorind : int -> unit

val setmarkertype : int -> unit

val setmarkersize : float -> unit

val setmarkercolorind : int -> unit

val settextfontprec : int -> int -> unit

val setcharexpan : float -> unit

val setcharspace : float -> unit

val settextcolorind : int -> unit

val setcharheight : float -> unit

val setcharup : float -> float -> unit

val settextpath : int -> unit

val settextalign : int -> int -> unit

val setfillintstyle : int -> unit

val setfillstyle : int -> unit

val setfillcolorind : int -> unit

val setcolorrep : int -> float -> float -> float -> unit

val setwindow : float -> float -> float -> float -> unit

val inqwindow :
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  unit

val setviewport : float -> float -> float -> float -> unit

val inqviewport :
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  unit

val selntran : int -> unit

val setclip : int -> unit

val setwswindow : float -> float -> float -> float -> unit

val setwsviewport : float -> float -> float -> float -> unit

val createseg : int -> unit

val copysegws : int -> unit

val redrawsegws : unit -> unit

val setsegtran :
  int -> float -> float -> float -> float -> float -> float -> float -> unit

val closeseg : unit -> unit

val emergencyclosegks : unit -> unit

val updategks : unit -> unit

val setspace : float -> float -> int -> int -> int

val inqspace :
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  int Ctypes_static.ptr ->
  int Ctypes_static.ptr ->
  unit

val setscale : int -> int

val inqscale : int Ctypes_static.ptr -> unit

val textext : float -> float -> string -> int

val inqtextext :
  float ->
  float ->
  string ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  unit

val axes : float -> float -> float -> float -> int -> int -> float -> unit

val axeslbl :
  float ->
  float ->
  float ->
  float ->
  int ->
  int ->
  float ->
  (float -> float -> string -> float -> unit) ->
  (float -> float -> string -> float -> unit) ->
  unit

val grid : float -> float -> float -> float -> int -> int -> unit

val grid3d :
  float ->
  float ->
  float ->
  float ->
  float ->
  float ->
  int ->
  int ->
  int ->
  unit

val verrorbars :
  int ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  unit

val herrorbars :
  int ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  unit

val polyline3d :
  int ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  unit

val polymarker3d :
  int ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  unit

val axes3d :
  float ->
  float ->
  float ->
  float ->
  float ->
  float ->
  int ->
  int ->
  int ->
  float ->
  unit

val titles3d : string -> string -> string -> unit

val surface :
  int ->
  int ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  int ->
  unit

val contour :
  int ->
  int ->
  int ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  int ->
  unit

val contourf :
  int ->
  int ->
  int ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  int ->
  unit

val tricontour :
  int ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  int ->
  float Ctypes_static.ptr ->
  unit

val hexbin :
  int -> float Ctypes_static.ptr -> float Ctypes_static.ptr -> int -> int

val setcolormap : int -> unit

val inqcolormap : int Ctypes_static.ptr -> unit

val colorbar : unit -> unit

val inqcolor : int -> int Ctypes_static.ptr -> unit

val inqcolorfromrgb : float -> float -> float -> int

val hsvtorgb :
  float ->
  float ->
  float ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  unit

val tick : float -> float -> float

val validaterange : float -> float -> int

val adjustlimits : float Ctypes_static.ptr -> float Ctypes_static.ptr -> unit

val adjustrange : float Ctypes_static.ptr -> float Ctypes_static.ptr -> unit

val beginprint : string -> unit

val beginprintext : string -> string -> string -> string -> unit

val endprint : unit -> unit

val ndctowc : float Ctypes_static.ptr -> float Ctypes_static.ptr -> unit

val wctondc : float Ctypes_static.ptr -> float Ctypes_static.ptr -> unit

val wc3towc :
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  unit

val drawrect : float -> float -> float -> float -> unit

val fillrect : float -> float -> float -> float -> unit

val drawarc : float -> float -> float -> float -> int -> int -> unit

val fillarc : float -> float -> float -> float -> int -> int -> unit

val drawpath :
  int ->
  vertex_t Ctypes.structure Ctypes_static.ptr ->
  Unsigned.uchar Ctypes_static.ptr ->
  int ->
  unit

val setarrowstyle : int -> unit

val setarrowsize : float -> unit

val drawarrow : float -> float -> float -> float -> unit

val readimage :
  string ->
  int Ctypes_static.ptr ->
  int Ctypes_static.ptr ->
  int Ctypes_static.ptr Ctypes_static.ptr ->
  int

val drawimage :
  float ->
  float ->
  float ->
  float ->
  int ->
  int ->
  int Ctypes_static.ptr ->
  int ->
  unit

val importgraphics : string -> int

val setshadow : float -> float -> float -> unit

val settransparency : float -> unit

val setcoordxform : float Ctypes_static.ptr Ctypes_static.ptr -> unit

val begingraphics : string -> unit

val endgraphics : unit -> unit

val getgraphics : unit -> string

val drawgraphics : string -> int

val mathtex : float -> float -> string -> unit

val inqmathtex :
  float ->
  float ->
  string ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  unit

val beginselection : int -> int -> unit

val endselection : unit -> unit

val moveselection : float -> float -> unit

val resizeselection : int -> float -> float -> unit

val inqbbox :
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  unit

val precision : unit -> float

val setregenflags : int -> unit

val inqregenflags : unit -> int

val savestate : unit -> unit

val restorestate : unit -> unit

val selectcontext : int -> unit

val destroycontext : int -> unit

val uselinespec : string -> int

val delaunay :
  int ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  int Ctypes_static.ptr ->
  int Ctypes_static.ptr Ctypes_static.ptr ->
  unit

val reducepoints :
  int ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  int ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  unit

val trisurface :
  int ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  unit

val gradient :
  int ->
  int ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  unit

val quiver :
  int ->
  int ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  int ->
  unit

val interp2 :
  int ->
  int ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  int ->
  int ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  int ->
  float ->
  unit

val version : unit -> string

val shade :
  int ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  int ->
  int ->
  float Ctypes_static.ptr ->
  int ->
  int ->
  int Ctypes_static.ptr ->
  unit

val shadepoints :
  int ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  int ->
  int ->
  int ->
  unit

val shadelines :
  int ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  int ->
  int ->
  int ->
  unit

val panzoom :
  float ->
  float ->
  float ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  float Ctypes_static.ptr ->
  unit

val get_size_and_pointers :
  ('a, 'b, 'c) Bigarray.Genarray.t ->
  ('d, 'e, 'f) Bigarray.Genarray.t ->
  int * 'a Ctypes.ptr * 'd Ctypes.ptr

val get_size_and_pointer :
  ('a, 'b, 'c) Bigarray.Genarray.t -> int * 'a Ctypes.ptr
