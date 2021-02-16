(** Library for using Mlpost through standard library graphics API *)

(** Subset of the Graphics API signature supported by Mlpost_graphics *)
module type Graphics = sig
  (* val open_graph : string -> unit
   * val close_graph : unit -> unit
   * val set_window_title : string -> unit *)

  (* val resize_window : int -> int -> unit *)
  val clear_graph : unit -> unit

  (* val size_x : unit -> int
   * val size_y : unit -> int *)

  type color

  (* val rgb : int -> int -> int -> color *)
  (** [rgb r g b] returns the integer encoding the color with red
    component [r], green component [g], and blue component [b].
    [r], [g] and [b] are in the range [0..255]. *)

  val black : color

  val white : color

  val red : color

  val green : color

  val blue : color

  (* val yellow : color
   * val cyan : color
   * val magenta : color *)

  val moveto : int -> int -> unit
  (** Position the current point. *)

  (* val rmoveto : int -> int -> unit *)
  (** [rmoveto dx dy] translates the current point by the given vector. *)

  (* val current_x : unit -> int *)
  (** Return the abscissa of the current point. *)

  (* val current_y : unit -> int *)
  (** Return the ordinate of the current point. *)

  (* val current_point : unit -> int * int *)
  (** Return the position of the current point. *)

  val lineto : int -> int -> unit
  (** Draw a line with endpoints the current point and the given point,
    and move the current point to the given point. *)

  (* val rlineto : int -> int -> unit *)
  (** Draw a line with endpoints the current point and the
    current point translated of the given vector,
    and move the current point to this point. *)

  (* val curveto : int * int -> int * int -> int * int -> unit *)
  (** [curveto b c d] draws a cubic Bezier curve starting from
    the current point to point [d], with control points [b] and
    [c], and moves the current point to [d]. *)

  val draw_rect : int -> int -> int -> int -> unit
  (** [draw_rect x y w h] draws the rectangle with lower left corner
    at [x,y], width [w] and height [h].
    The current point is unchanged.
    Raise [Invalid_argument] if [w] or [h] is negative. *)

  (* val draw_poly_line : (int * int) array -> unit *)
  (** [draw_poly_line points] draws the line that joins the
    points given by the array argument.
    The array contains the coordinates of the vertices of the
    polygonal line, which need not be closed.
    The current point is unchanged. *)

  (* val draw_poly : (int * int) array -> unit *)
  (** [draw_poly polygon] draws the given polygon.
    The array contains the coordinates of the vertices of the
    polygon.
    The current point is unchanged. *)

  (* val draw_segments : (int * int * int * int) array -> unit *)
  (** [draw_segments segments] draws the segments given in the array
    argument. Each segment is specified as a quadruple
    [(x0, y0, x1, y1)] where [(x0, y0)] and [(x1, y1)] are
    the coordinates of the end points of the segment.
    The current point is unchanged. *)

  (* val draw_arc : int -> int -> int -> int -> int -> int -> unit *)
  (** [draw_arc x y rx ry a1 a2] draws an elliptical arc with center
    [x,y], horizontal radius [rx], vertical radius [ry], from angle
    [a1] to angle [a2] (in degrees). The current point is unchanged.
    Raise [Invalid_argument] if [rx] or [ry] is negative. *)

  (* val draw_ellipse : int -> int -> int -> int -> unit *)
  (** [draw_ellipse x y rx ry] draws an ellipse with center
    [x,y], horizontal radius [rx] and vertical radius [ry].
    The current point is unchanged.
    Raise [Invalid_argument] if [rx] or [ry] is negative. *)

  (* val draw_circle : int -> int -> int -> unit *)
  (** [draw_circle x y r] draws a circle with center [x,y] and
    radius [r]. The current point is unchanged.
    Raise [Invalid_argument] if [r] is negative. *)

  (* val set_line_width : int -> unit *)
  (** Set the width of points and lines drawn with the functions above.
    Under X Windows, [set_line_width 0] selects a width of 1 pixel
    and a faster, but less precise drawing algorithm than the one
    used when [set_line_width 1] is specified.
    Raise [Invalid_argument] if the argument is negative. *)

  (** {5 Text drawing} *)

  (* val draw_char : char -> unit *)
  (** See {!Graphics.draw_string}.*)

  val draw_string : string -> unit
  (** Draw a character or a character string with lower left corner
    at current position. After drawing, the current position is set
    to the lower right corner of the text drawn. *)

  val set_color : color -> unit

  (* val set_font : string -> unit *)
  (** Set the font used for drawing text.
    The interpretation of the argument to [set_font]
    is implementation-dependent. *)

  (* val set_text_size : int -> unit *)
  (** Set the character size used for drawing text.
    The interpretation of the argument to [set_text_size]
    is implementation-dependent. *)

  (* val text_size : string -> int * int *)
  (** Return the dimensions of the given text, if it were drawn with
    the current font and size. *)

  (** {5 Filling} *)

  (* val fill_rect : int -> int -> int -> int -> unit *)
  (** [fill_rect x y w h] fills the rectangle with lower left corner
    at [x,y], width [w] and height [h], with the current color.
    Raise [Invalid_argument] if [w] or [h] is negative. *)

  (* val fill_poly : (int * int) array -> unit *)
  (** Fill the given polygon with the current color. The array
    contains the coordinates of the vertices of the polygon. *)

  (* val fill_arc : int -> int -> int -> int -> int -> int -> unit *)
  (** Fill an elliptical pie slice with the current color. The
    parameters are the same as for {!Graphics.draw_arc}. *)

  (* val fill_ellipse : int -> int -> int -> int -> unit *)
  (** Fill an ellipse with the current color. The
    parameters are the same as for {!Graphics.draw_ellipse}. *)

  val fill_circle : int -> int -> int -> unit
  (** Fill a circle with the current color. The
      parameters are the same as for {!Graphics.draw_circle}. *)

  val get_cmds : unit -> Mlpost.Command.t

  val set_emit : string -> unit

  val synchronize : unit -> unit
end

module Virtual : Graphics
(** Create only mlpost figures *)

module Double : Graphics
(** Create mlpost figures and call graphics function *)
