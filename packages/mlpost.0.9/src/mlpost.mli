(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Johannes Kanig, Stephane Lescuyer                       *)
(*  Jean-Christophe Filliatre, Romain Bardou and Francois Bobot           *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

module Signature : sig
  type point

  module type Boxlike = sig
    type t

    val width : t -> float

    val height : t -> float

    val set_pos : point -> t -> t
  end
end

module File : sig
  type t

  val from_string : string -> t

  val to_string : t -> string
end

module Ctypes : sig
  type matrix = Cairo.matrix = {
    mutable xx : float;
    mutable yx : float;
    mutable xy : float;
    mutable yy : float;
    mutable x0 : float;
    mutable y0 : float;
  }

  type point = { x : float; y : float }
end

(** {2 Interfaces to basic Metapost datatypes} *)

(** Abstract numeric values *)
module Num : sig
  (** Numerics are a symbolic representation of numeric values.
    In many cases, but not always, an object of type {!Num.t} is intended to be
    a length in some unit. In addition, values of type {!Num.t} may actually be
    unknown to Mlpost. This is why there is no function that gives back a
    [float].
  *)

  type t = float
  (** The Mlpost numeric type is an abstract datatype *)

  (** {2 Conversion functions} *)

  val of_float : float -> t
  (** Convert a float into a {!Num.t} *)

  val bp : float -> t
  (** The base unit in Mlpost is bp. *)

  val pt : float -> t
  (** pt are PostScript points. This is the same unit as the pt unit in TeX *)

  val cm : float -> t

  val mm : float -> t

  val inch : float -> t

  (** The following are units dependent of the font used *)

  val em : float -> t
  (** the width of an "m" *)

  val ex : float -> t
  (** the height of an "x" *)

  (** {2 Useful operations on Nums} *)

  val addn : t -> t -> t

  val subn : t -> t -> t

  val multn : t -> t -> t

  val multf : float -> t -> t

  val divf : t -> float -> t

  val neg : t -> t

  val divn : t -> t -> t

  val maxn : t -> t -> t

  val minn : t -> t -> t

  val gmean : t -> t -> t
  (** the geometric mean of two nums : sqrt(a * a + b * b) *)

  val if_null : t -> t -> t -> t
  (** if_null n n1 n2 is equal to n1 if n is null, n2 othewise *)

  (** {3 Infix operators}  *)

  module Infix : sig
    (** Infix symbols for convenience *)

    val ( +/ ) : t -> t -> t
    (** alias for {!Num.addn} *)

    val ( -/ ) : t -> t -> t
    (** alias for {!Num.subn} *)

    val ( */ ) : t -> t -> t
    (** alias for {!Num.multn} *)

    val ( // ) : t -> t -> t
    (** alias for {!Num.divn} *)

    val ( *./ ) : float -> t -> t
    (** alias for {!Num.multf} *)

    val ( /./ ) : t -> float -> t
    (** alias for {!Num.divf} *)
  end

  (** {2 Useful constants and functions} *)

  val zero : t

  val one : t

  val two : t
  (** Shortcuts for [bp 0.], [bp 1.] and [bp 2.]. *)

  val pi : float
  (** 3 .14159 *)

  (** 3 .14159 *)
  val deg2rad : float -> float
  (** Converts degrees into radians *)

  type scale = float -> t

  module Scale : sig
    val bp : float -> scale

    val pt : float -> scale

    val cm : float -> scale

    val mm : float -> scale

    val inch : float -> scale
  end
end

(** Definitions of many colors *)
module Color : sig
  (** Colors *)

  type t
  (** the abstract type of colors *)

  val default : t
  (** the default color is black *)

  val rgb : float -> float -> float -> t
  (** [rgb r g b] constructs the color that corresponds to the color code
	RGB(r,g,b)  *)

  (** [rgb r g b] constructs the color that corresponds to the color code
	RGB(r,g,b)  *)
  val rgb8 : int -> int -> int -> t
  (** similar to [rgb], but takes integers between 0 and 255 as argument *)

  val cmyk : float -> float -> float -> float -> t
  (** [cmyk c m y k] constructs the color that corresponds to the color code
	CMYK(c,m,y,k)  *)

  (** WARNING : If you use transparency with .mps file in your latex
      document you need to add : \LoadMetaPostSpecialExtensions in the
      preamble *)

  val rgba : float -> float -> float -> float -> t
  (** similar to [rgb], but takes the factor of transparency *)

  val rgb8a : int -> int -> int -> int -> t
  (** similar to [rgb8], but takes the factor of transparency *)

  val cmyka : float -> float -> float -> float -> float -> t
  (** similar to [cmyk], but takes the factor of transparency *)

  val is_opaque : t -> bool
  (** test if the color is opaque *)

  val opaque : t -> t
  (** make a color opaque *)

  val transparent : float -> t -> t
  (** [transparent f c] multiplies by f the factor of transparency of c *)

  val hsv : float -> float -> float -> t
  (** hsv h s v convert an hsv color to an rgb.
        0 <= h < 360, 0 <= s,v <= 1*)

  (** {3 color generator} *)

  val color_gen : float -> float -> unit -> t

  (* color_gen s v creates a generator of colors which return a
     different color (with saturation s and value v) each time it is
     called. The goal is to have colors with a good contrast between
     them. For given s and v the resutl is deterministically *)

  (** {2 Predefined Colors} *)

  (** {3 base colors} *)

  val white : t

  val black : t

  val red : t

  val blue : t

  val green : t

  val cyan : t

  val yellow : t

  val magenta : t

  (** {3 lighter colors} *)

  val lightred : t

  val lightblue : t

  val lightgreen : t

  val lightcyan : t

  val lightyellow : t

  val lightmagenta : t

  (** {3 grays} *)

  val gray : float -> t

  val lightgray : t

  val mediumgray : t

  val darkgray : t

  (** {3 additional colors} *)

  val orange : t

  val purple : t

  (** {3 X11-named Colors} *)

  val color : string -> t
  (** [color n] returns the RGB color associated to name [n]
	(as defined in /etc/X11/rgb.txt). Raises [Not_found] if [n] does not
	correspond to a color.
        See {{:http://en.wikipedia.org/wiki/X11_color_names} this list} for an
        overview.*)
end

(** Points in the plane *)
module rec Point : sig
  type t = Signature.point
  (** The abstract type for points *)

  val pt : Num.t * Num.t -> t
  (**  Construct a point from two numeric values *)

  (** The following functions create points of length 1.
      They are especially useful to specify directions with [Path.Vec] *)

  val dir : float -> t
  (** [dir f] is the point at angle [f] on the unit circle.
      [f] shall be given in degrees *)

  (** The unitary vectors pointing up, down, left and right *)

  val up : t

  val down : t

  val left : t

  val right : t

  val origin : t

  val length : t -> Num.t
  (** [length p] is the length of vector from the origin to [p] *)

  val xpart : t -> Num.t
  (** [xpart p] is the x coordinate of point [p] *)

  val ypart : t -> Num.t
  (** [ypart p] is the y coordinate of point [p] *)

  (** {2 Operations on points} *)

  val transform : Transform.t -> t -> t
  (** Apply a transformation to a point *)

  val segment : float -> t -> t -> t
  (** [segment f p1 p2] is the point [(1-f)p1 + fp2]. Stated otherwise, if
     [p1] is at [0.] and [p2] is at [1.], return the point that lies at [f] *)

  val add : t -> t -> t

  val shift : t -> t -> t
  (** Sum two points *)

  val sub : t -> t -> t
  (** Substract two points *)

  val mult : Num.t -> t -> t

  val scale : Num.t -> t -> t
  (** Multiply a point by a scalar *)

  val rotate : float -> t -> t
  (** Rotate a point by an angle in degrees *)

  val rotate_around : t -> float -> t -> t
  (** [rotate_around p1 f p2] rotates [p2] around [p1] by an angle [f]
      in degrees *)

  val xscale : Num.t -> t -> t
  (** Scales the X coordinate of a point by a scalar *)

  val yscale : Num.t -> t -> t
  (** Scales the Y coordinate of a point by a scalar *)

  val normalize : t -> t
  (** Normalize the vector represented by the point.
      The origin becomes the origin *)

  (** {2 Convenient constructors} *)

  (** The following functions build a point at a
      given scale (see {!Num.t} for scales) *)

  val bpp : float * float -> t

  val inp : float * float -> t

  val cmp : float * float -> t

  val mmp : float * float -> t

  val ptp : float * float -> t

  (** Same as the previous functions but build list of points *)

  val map_bp : (float * float) list -> t list

  val map_in : (float * float) list -> t list

  val map_cm : (float * float) list -> t list

  val map_mm : (float * float) list -> t list

  val map_pt : (float * float) list -> t list

  val p : ?scale:(float -> Num.t) -> float * float -> t
  (** Builds a point from a pair of floats
      @param scale a scaling function to be applied to each float;
      see {!Num.t} for scaling functions for usual units *)

  val ptlist : ?scale:(float -> Num.t) -> (float * float) list -> t list
  (** Same as [p], but builds a list of points *)

  val draw : ?brush:Brush.t -> ?color:Color.t -> ?pen:Pen.t -> t -> Command.t
  (** Draw a point
	@param color the color of the point; default is black
	@param pen the pen used to draw the pen; default is
               [Brush.Pen.default]*)
end

(** MetaPaths: gradually build a path with constraints, get a real
    path at thxe end. *)
and MetaPath : sig
  (** MetaPaths are the objects used to describe lines, curves, and
      more generally almost everything that is drawn with Mlpost.
      A path ([Path.t]) is defined by points and control points.
      A metapath is defined by points (knots) and constraints on the links
      between the points. A metapath is an easy way to define a path gradually
      with only a few points, and apply heuristics afterwards to transform it
      into a real path (using [of_metapath]). *)

  type direction = Path.direction
  (** A [direction] is used to put constraints on metapaths:
      {ul {- [vec p] defines a direction by a point (interpreted as a vector)}
      {- [curl f] changes the curling factor of the extremity of a metapath;
      higher curling factor means flatter curves}
      {- [noDir] means no particular direction} } *)

  val vec : Point.t -> direction

  val curl : float -> direction

  val noDir : direction

  type knot = Path.knot
  (** A [knot] is the basic element of a metapath, and is simply a point
      with an incoming and outgoing direction constraint *)

  val knotp : ?l:direction -> ?r:direction -> Point.t -> knot
  (** Build a knot from a point; the optional arguments are the
      incoming directions.Warning they are going in the same direction. *)

  val knotlist : (direction * Point.t * direction) list -> knot list

  type joint = Path.joint
  (** A joint is the connection between two knots in a metapath. It is either
      {ul {- [jLine] for a straight line}
      {- [jCurve] for a spline curve}
      {- [jCurveNoInflex] to avoid inflexion points}
      {- [jTension f1 f2] to specify "tension" on the joint; [jCurve] uses a
      default tension of 1. Higher tension means less "wild" curves}
      {- [jControls p1 p2] to explicitely specify control points}} *)

  val jLine : joint

  val jCurve : joint

  val jCurveNoInflex : joint

  val jTension : float -> float -> joint

  val jControls : Point.t -> Point.t -> joint

  type t
  (** The abstract type of metapaths *)

  type path = Path.t

  (** In all the functions below :
        - noDir is the default direction
        - jCurve is the default joint *)

  (** {2 Labelled metapath constructors} *)

  val knot :
    ?l:direction ->
    ?r:direction ->
    ?scale:(float -> Num.t) ->
    float * float ->
    knot
  (** Build a knot from a pair of floats
      @param l an incoming direction
      @param r an outgoing direction
      @param scale a scaling factor applied to the floats *)

  val knotn : ?l:direction -> ?r:direction -> Num.t * Num.t -> knot
  (** Build a knot from a Num.t pair; the optional arguments are as in
      {!knot} *)

  val path :
    ?style:joint -> ?scale:(float -> Num.t) -> (float * float) list -> t
  (** Build a metapath from a list of pairs of floats
      @param style the joint style used for all joints in the metapath
      @param cycle if given, the metapath is closed using the given style
      @param scale permits to scale the whole metapath *)

  val pathn : ?style:joint -> (Num.t * Num.t) list -> t
  (** Same as [metapath], but uses a [Num.t] list *)

  val pathk : ?style:joint -> knot list -> t
  (** Same as [metapath], but uses a knot list *)

  val pathp : ?style:joint -> Point.t list -> t
  (** Same as [metapath] but uses a point list *)

  val jointpathk : knot list -> joint list -> t
  (** Build a metapath from [n] knots and [n-1] joints *)

  val jointpathp : Point.t list -> joint list -> t
  (** Build a metapath from [n] points and [n-1] joints,
      with default directions *)

  val jointpathn : (Num.t * Num.t) list -> joint list -> t

  val jointpath :
    ?scale:(float -> Num.t) -> (float * float) list -> joint list -> t
  (** Build a metapath from [n] float_pairs and [n-1] joints,
      with default directions *)

  val cycle : ?dir:direction -> ?style:joint -> t -> path
  (** Close a metapath using direction [dir] and style [style] *)

  (** {2 Primitive metapath constructors} *)

  val concat : ?style:joint -> t -> knot -> t
  (** Add a knot at the end of a metapath  *)

  val start : knot -> t
  (** Create a simple metapath with one knot *)

  val append : ?style:joint -> t -> t -> t
  (** Append a metapath to another using joint [style] *)

  (** {2 Predefined values} *)

  val defaultjoint : joint
  (** The default joint style ([JCurve]) *)

  (** {2 Conversions} *)

  val to_path : t -> path
  (** Compute the control point of the path
      for a good looking result according to the constraint
      on the direction, tension, curve *)

  val of_path : path -> t
  (** Obtain a metapath from a path with exactly the same
      control point. p = of_metapath (of_path p) is true but
      not the opposite.*)
end

(** Fixed Paths *)
and Path : sig
  (** Paths are the objects used to describe lines, curves, and
      more generally almost everything that is drawn with Mlpost *)

  type direction
  (** A [direction] is used to put constraints on paths:
      {ul {- [vec p] defines a direction by a point (interpreted as a vector)}
      {- [curl f] changes the curling factor of the extremity of a path;
      higher curling factor means flatter curves}
      {- [noDir] means no particular direction} } *)

  val vec : Point.t -> direction

  val curl : float -> direction

  val noDir : direction

  type knot
  (** A [knot] is the basic element of a path, and is simply a point
      with an incoming and outgoing direction constraint *)

  val knotp : ?l:direction -> ?r:direction -> Point.t -> knot
  (** Build a knot from a point; the optional arguments are the
      incoming directions. Warning they are going in the same direction. *)

  val knotlist : (direction * Point.t * direction) list -> knot list

  type joint
  (** A joint is the connection between two knots in a path. It is either
      {ul {- [jLine] for a straight line}
      {- [jCurve] for a spline curve}
      {- [jCurveNoInflex] to avoid inflexion points}
      {- [jTension f1 f2] to specify "tension" on the joint; [jCurve] uses a
      default
      tension of 1. Higher tension means less "wild" curves}
      {- [jControls p1 p2] to explicitely specify control points}} *)

  val jLine : joint

  val jCurve : joint

  val jCurveNoInflex : joint

  val jTension : float -> float -> joint

  val jControls : Point.t -> Point.t -> joint

  type t
  (** The abstract type of paths *)

  (** In all the functions below :
        - noDir is the default direction
        - jCurve is the default joint *)

  (** {2 Labelled path constructors} *)

  val knot :
    ?l:direction ->
    ?r:direction ->
    ?scale:(float -> Num.t) ->
    float * float ->
    knot
  (** Build a knot from a pair of floats
      @param l an incoming direction
      @param r an outgoing direction
      @param scale a scaling factor applied to the floats *)

  val knotn : ?l:direction -> ?r:direction -> Num.t * Num.t -> knot
  (** Build a knot from a Num.t pair; the optional arguments are as in
      {!knot} *)

  val path :
    ?style:joint ->
    ?cycle:joint ->
    ?scale:(float -> Num.t) ->
    (float * float) list ->
    t
  (** Build a path from a list of pairs of floats
      @param style the joint style used for all joints in the path
      @param cycle if given, the path is closed using the given style
      @param scale permits to scale the whole path *)

  val pathn : ?style:joint -> ?cycle:joint -> (Num.t * Num.t) list -> t
  (** Same as [path], but uses a [Num.t] list *)

  val pathk : ?style:joint -> ?cycle:joint -> knot list -> t
  (** Same as [path], but uses a knot list *)

  val pathp : ?style:joint -> ?cycle:joint -> Point.t list -> t
  (** Same as [path] but uses a point list *)

  val jointpathk : knot list -> joint list -> t
  (** Build a path from [n] knots and [n-1] joints *)

  val jointpathp : Point.t list -> joint list -> t
  (** Build a path from [n] points and [n-1] joints,
      with default directions *)

  val jointpathn : (Num.t * Num.t) list -> joint list -> t

  val jointpath :
    ?scale:(float -> Num.t) -> (float * float) list -> joint list -> t
  (** Build a path from [n] float_pairs and [n-1] joints,
      with default directions *)

  val cycle : ?dir:direction -> ?style:joint -> t -> t
  (** Close a path using direction [dir] and style [style] *)

  (** {2 Primitive path constructors} *)

  val concat : ?style:joint -> t -> knot -> t
  (** Add a knot at the end of a path  *)

  val start : knot -> t
  (** Create a simple path with one knot *)

  val append : ?style:joint -> t -> t -> t
  (** Append a path to another using joint [style] *)

  (** {2 More complex constructions on paths} *)

  val length : t -> Num.t
  (** Number of nodes in a path, minus one. *)

  val point : float -> t -> Point.t
  (** [point f p] returns a certain point on the path [p]; [f] is
      given "in control points": [0.] means the first control point,
      [1.] the second and so on; intermediate values are accepted. *)

  val pointn : Num.t -> t -> Point.t
  (** Same as [point] but for a [Num.t]. *)

  val direction : float -> t -> Point.t
  (** [direction f p] returns the direction of the tangent at [point f p]. *)

  val directionn : Num.t -> t -> Point.t
  (** Same as [direction] but for a [Num.t]. *)

  val subpath : float -> float -> t -> t
  (** [subpath start end path] selects the subpath of [path] that lies
      between [start] and [end]. [start] and [end] are given in
      control points, as in {!point}. *)

  val subpathn : Num.t -> Num.t -> t -> t
  (** Same as [subpathn] but using [Num.t]. *)

  val transform : Transform.t -> t -> t
  (** Apply a transformation to a path *)

  val scale : Num.t -> t -> t

  val rotate : float -> t -> t

  val shift : Point.t -> t -> t

  val yscale : Num.t -> t -> t

  val xscale : Num.t -> t -> t
  (** Shortcuts for transformations of Paths *)

  val cut_after : t -> t -> t
  (** [cut_after p1 p2] cuts [p2] after the intersection with [p1].
      To memorize the order of the arguments,
      you can read: "cut after [p1]" *)

  val cut_before : t -> t -> t
  (** Same as {!cut_after}, but cuts before *)

  val strip : Num.t -> t -> t
  (** [strip n p] removes two segments of length [n] at each end of path [p] *)

  val build_cycle : t list -> t
  (** Build a cycle from a set of intersecting paths *)

  (** {2 Predefined values} *)

  val defaultjoint : joint
  (** The default joint style ([JCurve]) *)

  val fullcircle : t
  (** A full circle of radius 1 and centered on the origin *)

  val halfcircle : t
  (** The upper half of [fullcircle] *)

  val quartercircle : t
  (** The right half of [halfcircle] *)

  val unitsquare : t
  (** A full square of size 1 and centered on the origin *)

  (** {2 Conversions} *)

  type metapath = MetaPath.t
  (** Compute the control point of the path
      for a good looking result according to the constraint
      on the direction, tension, curve *)

  val of_metapath : metapath -> t
  (** Compute the control point of the path
      for a good looking result according to the constraint
      on the direction, tension, curve *)

  val to_metapath : t -> metapath
  (** Obtain a metapath from a path with exactly the same
      control point. p = of_metapath (of_path p) is true but
      not the opposite.*)

  (** {2 Smart path } *)

  type orientation =
    | Up
    | Down
    | Left
    | Right
    | Upn of Num.t
    | Downn of Num.t
    | Leftn of Num.t
    | Rightn of Num.t

  val smart_path : ?style:joint -> orientation list -> Point.t -> Point.t -> t

  val draw :
    ?brush:Brush.t ->
    ?color:Color.t ->
    ?pen:Pen.t ->
    ?dashed:Dash.t ->
    t ->
    Command.t
  (** Draw a path
        @param brush the brush used to draw the path; the next argument
               redefined this one
	@param color the color of the path; default is black
	@param pen the pen used to draw the path; default is
               [Brush.Pen.default]
	@param dashed if given, the path is drawn using that dash_style. *)

  val fill : ?color:Color.t -> t -> Command.t
  (** Fill a contour given by a closed path
	@param color the color used to fill the area; default is black *)
end

(**/**)

(** Pens: change the way lines are drawn in Mlpost *)
and Pen : sig
  (** Pens are used to change the the way lines are drawn in Mlpost *)

  type t
  (** The abstract type of pens *)

  val transform : Transform.t -> t -> t
  (** Apply a transformation to pens *)

  (** Apply a transformation to pens *)
  val default : t
  (** The default pen; it corresponds to
      [Pen.scale (Num.bp 0.5) Pen.circle] *)

  (** The default pen; it corresponds to
      [Pen.scale (Num.bp 0.5) Pen.circle] *)
  val circle : t
  (** A circular pen of diameter 1 bp *)

  (** A circular pen of diameter 1 bp *)
  val square : t
  (** A pen in form of a square, of length 1 bp *)

  (** A pen in form of a square, of length 1 bp *)
  val from_path : Path.t -> t
  (** Construct a pen from a closed path *)

  val scale : Num.t -> t -> t

  val rotate : float -> t -> t

  val shift : Point.t -> t -> t

  val yscale : Num.t -> t -> t

  val xscale : Num.t -> t -> t
  (** Shortcuts for transformations of pens *)
end

(** Dash patterns *)
and Dash : sig
  (** This module permits to define dash patterns, that are used to draw lines
   in different styles *)

  type t = Brush.Dash.t
  (** The abstract type of dash patterns *)

  val evenly : t
  (** The pattern composed of evenly spaced dashes *)

  (** The pattern composed of evenly spaced dashes *)
  val withdots : t
  (** The pattern composed of evenly spaced dots *)

  val scaled : float -> t -> t
  (** Scale a dash pattern *)

  (** Scale a dash pattern *)
  val shifted : Point.t -> t -> t
  (** Shift a dash pattern *)

  type on_off

  val on : Num.t -> on_off

  val off : Num.t -> on_off

  val pattern : on_off list -> t
  (** This function, together with the type [on_off]  permits to construct
     custom dash patterns, by giving a list of [on] / [off] constructors, with
      corresponding lengths *)
end

(**/**)

(** Brushes : change the way lines are drawn in Mlpost *)
and Brush : sig
  (** Pens: change the way lines look like in Mlpost *)
  module Pen : sig
    (** Pens are used to change the the way lines are drawn in Mlpost *)

    type t = Pen.t

    (**/**)

    (**/**)

    (** The abstract type of pens *)

    val transform : Transform.t -> t -> t
    (** Apply a transformation to pens *)

    (** Apply a transformation to pens *)
    val default : t
    (** The default pen; it corresponds to
      [Pen.scale (Num.bp 0.5) Pen.circle] *)

    (** The default pen; it corresponds to
      [Pen.scale (Num.bp 0.5) Pen.circle] *)
    val circle : t
    (** A circular pen of diameter 1 bp *)

    (** A circular pen of diameter 1 bp *)
    val square : t
    (** A pen in form of a square, of length 1 bp *)

    (** A pen in form of a square, of length 1 bp *)
    val from_path : Path.t -> t
    (** Construct a pen from a closed path *)

    val scale : Num.t -> t -> t

    val rotate : float -> t -> t

    val shift : Point.t -> t -> t

    val yscale : Num.t -> t -> t

    val xscale : Num.t -> t -> t
    (** Shortcuts for transformations of pens *)
  end

  (** Dash patterns *)
  module Dash : sig
    (** This module permits to define dash patterns, that are used to
        draw lines in different styles *)

    type t
    (** The abstract type of dash patterns *)

    val evenly : t
    (** The pattern composed of evenly spaced dashes *)

    (** The pattern composed of evenly spaced dashes *)
    val withdots : t
    (** The pattern composed of evenly spaced dots *)

    val scaled : Num.t -> t -> t
    (** Scale a dash pattern *)

    (** Scale a dash pattern *)
    val shifted : Point.t -> t -> t
    (** Shift a dash pattern *)

    type on_off

    val on : Num.t -> on_off

    val off : Num.t -> on_off

    val pattern : on_off list -> t
    (** This function, together with the type [on_off] permits to
          construct custom dash patterns, by giving a list of [on] /
          [off] constructors, with corresponding lengths *)
  end

  type t

  val t :
    ?color:Color.t ->
    ?pen:Pen.t ->
    ?dash:Dash.t ->
    ?scale:Num.t ->
    ?brush:t ->
    unit ->
    t
  (** [t ~color ~pen ~dash ~scale ~brush] create a brush with color [color] if
      TODO *)

  val pen : t -> Pen.t option

  val dash : t -> Dash.t option

  val color : t -> Color.t option

  (** {2 Brushes with Predefined Colors} *)
  type brush_colored =
    ?pen:Pen.t -> ?dash:Dash.t -> ?scale:Num.t -> ?brush:t -> unit -> t
  (** {3 base colors} *)

  val white : brush_colored

  val black : brush_colored

  val red : brush_colored

  val blue : brush_colored

  val green : brush_colored

  val cyan : brush_colored

  val yellow : brush_colored

  val magenta : brush_colored

  (** {3 lighter colors} *)

  val lightred : brush_colored

  val lightblue : brush_colored

  val lightgreen : brush_colored

  val lightcyan : brush_colored

  val lightyellow : brush_colored

  val lightmagenta : brush_colored

  (** {3 grays} *)

  val gray : float -> brush_colored

  val lightgray : brush_colored

  val mediumgray : brush_colored

  val darkgray : brush_colored

  (** {3 additional colors} *)

  val orange : brush_colored

  val purple : brush_colored
end

(** Apply linear transformations to objects in Mlpost *)
and Transform : sig
  (** Transformations are an important way to modify objects in Mlpost.
      Objects can be scaled, shifted, rotated, etc, and any combination of
      these transformations is possible. Currently, transformations can be
      applied to Pictures, Pens and Paths. *)

  type t'
  (** The abstract type of a single transformation *)

  val scaled : Num.t -> t'
  (** Scale an object by a constant factor.
      @param scale a scaling function to be applied to each float;
      see {!Num.t} for scaling functions for usual units. This makes only sense
      when the object to be transformed is given in "bp" units *)

  (** Scale an object by a constant factor.
      @param scale a scaling function to be applied to each float;
      see {!Num.t} for scaling functions for usual units. This makes only sense
      when the object to be transformed is given in "bp" units *)
  val rotated : float -> t'
  (** Rotate an object by an angle given in degrees *)

  (** Rotate an object by an angle given in degrees *)
  val shifted : Point.t -> t'
  (** Shift an object with respect to a point *)

  (** Shift an object with respect to a point *)
  val slanted : Num.t -> t'
  (** Slant an object: the point [(x,y)] becomes [(x+ay,y)], with slanting
        factor [a] *)

  (** Slant an object: the point [(x,y)] becomes [(x+ay,y)], with slanting
        factor [a] *)
  val xscaled : Num.t -> t'
  (** Scale an object by a constant factor, but only in the [x] direction *)

  (** Scale an object by a constant factor, but only in the [x] direction *)
  val yscaled : Num.t -> t'
  (** Scale an object by a constant factor, but only in the [y] direction *)

  (** Scale an object by a constant factor, but only in the [y] direction *)
  val zscaled : Point.t -> t'
  (** Zscaled multiplies points of the object by the given point, using
        "complex" multiplication: [(x,y) * (a,b) = (ax - by, bx + ay)];
        its effect is to rotate and scale so as to map [(1,0)] into [(a,b)] *)

  (** Zscaled multiplies points of the object by the given point, using
        "complex" multiplication: [(x,y) * (a,b) = (ax - by, bx + ay)];
        its effect is to rotate and scale so as to map [(1,0)] into [(a,b)] *)
  val reflect : Point.t -> Point.t -> t'
  (** Reflect an object with respect to the line that goes through the two
        given points *)

  (** Reflect an object with respect to the line that goes through the two
        given points *)
  val rotate_around : Point.t -> float -> t'
  (** Rotate an object by an angle given in degrees, around a given point *)

  type matrix = Ctypes.matrix

  val explicit : matrix -> t'

  type t = t' list
  (** A transformation is a list of single transformations *)

  val id : t
  (** The identity transformation  *)
end

(** Functions to manipulate commands as if they were pictures  *)
and Picture : sig
  (** Pictures are a powerful way to reuse and modify parts of a figure *)

  type t = Command.t
  (** The abstract type of pictures *)

  val make : Command.t -> t
  (** Make a picture from a drawing command *)

  val tex : string -> t
  (** Take a string in Latex format and transform it into a picture *)

  val transform : Transform.t -> t -> t
  (** Apply a transformation to a picture *)

  val bbox : t -> Path.t
  (** Get the bounding box of a picture (with default padding, as
	in MetaPost) *)

  val corner_bbox : ?dx:Num.t -> ?dy:Num.t -> t -> Path.t
  (** Get the bounding box of a picture, according to its corners
        and supplied padding [dx] and [dy]. *)

  val center : Point.t -> t -> t
  (** Place a picture centered at some point *)

  val place_up_left : Point.t -> t -> t
  (** Place a picture with its upper left corner at some point *)

  val place_up_right : Point.t -> t -> t
  (** Place a picture with its upper right corner at some point *)

  val place_bot_left : Point.t -> t -> t
  (** Place a picture with its bottom left corner at some point *)

  val place_bot_right : Point.t -> t -> t
  (** Place a picture with its bottom right corner at some point *)

  val beside : t -> t -> t
  (** [beside p1 p2] returns a picture in which [p2] is placed right
        to [p1] *)

  val below : t -> t -> t
  (** [below p1 p2] returns a picture in which [p2] is placed below [p1] *)

  (** {2 Special points of the bounding box of a picture} *)

  val ctr : t -> Point.t
  (** @img ctr.png *)

  (** @img ctr.png *)
  val north : t -> Point.t
  (** @img north.png *)

  (** @img north.png *)
  val south : t -> Point.t
  (** @img south.png *)

  (** @img south.png *)
  val west : t -> Point.t
  (** @img west.png *)

  (** @img west.png *)
  val east : t -> Point.t
  (** @img east.png *)

  (** @img east.png *)
  val north_west : t -> Point.t
  (** @img north_west.png *)

  (** @img north_west.png *)
  val south_west : t -> Point.t
  (** @img south_west.png *)

  (** @img south_west.png *)
  val north_east : t -> Point.t
  (** @img north_east.png *)

  (** @img north_east.png *)
  val south_east : t -> Point.t
  (** @img south_east.png *)

  val corner : Command.position -> t -> Point.t

  (** {2 Special points of the bounding box of a picture (Deprecated)} *)

  val ulcorner : t -> Point.t
  (** These have been superseded by the preceding functions *)

  val llcorner : t -> Point.t

  val urcorner : t -> Point.t

  val lrcorner : t -> Point.t

  val clip : t -> Path.t -> t
  (** [clip pic path] limits [pic] to the cyclic path [path]; all elements
      outside of [path] are cut off. *)

  (** {2 Dimensions} *)

  val width : t -> Num.t

  val height : t -> Num.t

  (** Predefined Transformations *)

  val scale : Num.t -> t -> t

  val rotate : float -> t -> t

  val shift : Point.t -> t -> t

  val yscale : Num.t -> t -> t

  val xscale : Num.t -> t -> t

  val spin : float -> t -> t

  type escaped = [ `Backslash | `Underscore ]

  val escape_latex : escaped list -> string -> string

  val escape_all : string -> string

  val set_pos : Point.t -> t -> t
  (** alias of center *)
end

(** Basic drawing commands *)
and Command : sig
  (** General Commands to build figures *)

  type t
  (** The abstract commands type *)

  (*
    val logo : figure
  (** The Mlpost logo. *)

  *)
  (** {2 Drawing Commands} *)

  val draw :
    ?brush:Brush.t ->
    ?color:Color.t ->
    ?pen:Pen.t ->
    ?dashed:Dash.t ->
    Path.t ->
    t
  (** Draw a path
	@param color the color of the path; default is black
	@param pen the pen used to draw the path; default is
        [Brush.Pen.default]
	@param dashed if given, the path is drawn using that dash_style. *)

  (*
    val draw_arrow : ?color:Color.t -> ?pen:Pen.t -> ?dashed:Dash.t -> Path.t
    -> t
  (** Draw a path with an arrow head; the optional arguments
    are the same as for {!draw} *)
  *)

  val fill : ?color:Color.t -> Path.t -> t
  (** Fill a contour given by a closed path
	@param color the color used to fill the area; default is black *)

  val draw_pic : Picture.t -> t
  (** draws a picture *)

  val externalimage :
    string ->
    [ `None
    | `Width of Num.t  (** keep the proportion of the image *)
    | `Height of Num.t
    | `Inside of Num.t * Num.t
      (** must be inside a box of this height and width *)
    | `Exact of Num.t * Num.t ] ->
    t
  (** insert an image given its filename - *EXPERIMENTAL* *)

  (** {2 Manipulating Commands} *)

  val nop : t
  (** A command that has no effect *)

  val append : t -> t -> t
  (** Append two commands to form a compound command *)

  val ( ++ ) : t -> t -> t
  (** Abbreviation for [append] *)

  val seq : t list -> t
  (** Group a list of commands to a single command *)

  val iter : int -> int -> (int -> t) -> t
  (** [iter m n f] builds a command that corresponds to the sequence
	of commands [f m; f (m+1); ... ; f(n)] *)

  val iterl : ('a -> t) -> 'a list -> t
  (** [iterl f l] builds a command that corresponds to the sequence
	of commands [f x1; f x2; ... ; f xn] for [l = [x1;x2;...;xn]] *)

  (** {2 Labels} *)

  type hposition = [ `Center | `West | `East | `Left | `Right ]

  type vposition = [ `Center | `North | `South | `Top | `Bot | `Bottom ]

  type position =
    [ hposition
    | vposition
    | `Northwest
    | `Northeast
    | `Southwest
    | `Southeast
    | `Upperleft
    | `Upperright
    | `Lowerleft
    | `Lowerright
    | `Topleft
    | `Topright
    | `Bottomleft
    | `Bottomright
    | `Upleft
    | `Upright
    | `Lowleft
    | `Lowright ]
  (** Positions - they are used at many places in Mlpost to indicate a
          direction or position. *)

  val label : ?pos:position -> Picture.t -> Point.t -> t
  (** [label ~pos:`West pic p] puts picture [pic] at the left of the
      point [p] *)

  val dotlabel : ?pos:position -> Picture.t -> Point.t -> t
  (** Works like [label], but puts a dot at point [p] as well *)
end

(** {2 Advanced graphical components} *)

(** Rectangles, Circles, etc. *)
module Shapes : sig
  (** Various Basic Geometric Shapes *)

  val round_rect : Num.t -> Num.t -> Num.t -> Num.t -> Path.t
  (** [round_rect w h rx ry] returns a rectangle of width [w] and
	height [h] with rounded corners. The rounded corners are arcs
	of an ellipse of radii [rx] and [ry]. [rx] (resp. [ry]) should
	be positive and smaller than [w/2] (resp. [h/2]).
    *)

  val rectangle : Num.t -> Num.t -> Path.t
  (** [rectangle w h] returns a rectangle of width [w] and height [h].
    *)

  val ellipse : Num.t -> Num.t -> Path.t
  (** [ellipse rx ry] returns an ellipse of great axis [rx] and small axis
        [ry]. The ellipse is centered on the origin and aligned with the x
        axis.
	@param fill the color with which to fill the ellipse ; if no color
	  is provided, it is not filled.
	@param stroke the color with which the ellipse's outline shall be
	  drawn ; default is black.
	@param thickness the thickness of the pen used to draw
	  the outline ; 1. is default
    *)

  val circle : Num.t -> Path.t

  val patatoid : Num.t -> Num.t -> Path.t
  (** See {!Box.patatoid}. *)

  val patatoid2 : Num.t -> Num.t -> Path.t
  (** See {!Box.patatoid2}. *)

  (*
  val arc_ellipse :
    ?fill:Color.t -> ?stroke:Color.t -> ?thickness:float -> ?close:bool ->
    Num.t -> Num.t -> float -> float -> Picture.t
    (** [arc_ellipse rx ry th1 th2] draws an arc of the ellipse
	of great axis [rx] and small axis [ry] starting at angle [th1] and
	ending at angle [th2] (in radians).
	The ellipse is centered on the origin and aligned with the x axis.
	@param fill the colod with which to fill the ellipse ; if no color
	  is provided, it is not filled.
	@param stroke the color with which the ellipse's outline shall be
	  drawn ; default is black.
	@param thickness the thickness of the pen used to draw
	  the outline ; 1. is default
	@param close if true, the extremities of the arc are joined to
	the origin by straight lines, thus closing path. If [fill] is provided,
	then [close] will be true by default ; otherwise it is false.
    *)

*)
end

(** A Box is a rectangle with some content and a (not necessarily rectangular)
    frame. Boxes can be placed, aligned and modified.   *)
module Box : sig
  (** Boxes *)

  type t
  (** The abstract type of boxes *)

  (** {2 Creating boxes} *)

  type style =
    | Rect
    | Circle
    | RoundRect
    | Patatoid
    | Patatoid2
    | Ellipse
    | RoundBox
    | Custom of (Num.t -> Num.t -> Num.t * Num.t * Path.t)

  type 'a box_creator =
    ?dx:Num.t ->
    ?dy:Num.t ->
    ?name:string ->
    ?brush:Brush.t ->
    ?stroke:Color.t option ->
    ?pen:Pen.t ->
    ?dash:Dash.t ->
    ?fill:Color.t ->
    'a ->
    t
  (** All functions used to create boxes take the following optional
	parameters : [dx] (resp. [dy]) is the horizontal
	(resp. vertical) padding between the box border and its
	contents ; [name], if present, is associated with the box and
	can be used to retrieve it using [get] ; [stroke] is the color
	used to draw the outline of the box ; when equal to [None],
	the outline will not be drawn ; [pen] is the pen used to draw
	the box's outline, if absent [Brush.Pen.default] is used ;
	[fill], if present, is the color used to fill the box.
    *)

  val empty :
    ?width:Num.t ->
    ?height:Num.t ->
    ?style:style ->
    ?name:string ->
    ?brush:Brush.t ->
    ?stroke:Color.t option ->
    ?pen:Pen.t ->
    ?dash:Dash.t ->
    ?fill:Color.t ->
    unit ->
    t
  (** the empty box *)

  val empty_from_box :
    ?style:style ->
    ?name:string ->
    ?brush:Brush.t ->
    ?stroke:Color.t option ->
    ?pen:Pen.t ->
    ?dash:Dash.t ->
    ?fill:Color.t ->
    t ->
    t
  (** the empty box with the same position and dimension as the box.
        The special points are kept *)

  val pic : ?style:style -> Picture.t box_creator
  (** [pic p] creates a new box containing the picture [p] *)

  val path : ?style:style -> Path.t box_creator
  (** [path p] creates a new box containing the path [p] *)

  val tex : ?style:style -> string box_creator
  (** [tex s] creates a new box containing the LaTeX string [s]
     @img tex.png *)

  val box : ?style:style -> t box_creator
  (** [box b] creates a new box containing the box [b] *)

  val circle : t box_creator
  (** [circle pic] creates a circle box containing the picture
    [pic]. Optional padding is given by arguments [dx] and [dy];
    default is 2bp.
    @img circle.png *)

  val ellipse : t box_creator
  (** [ellipse pic] creates a elliptic box containing the picture
	[pic]. Optional padding is given by arguments [dx] and [dy];
	default is 2bp
    @img ellipse.png *)

  val rect : t box_creator
  (** [rect pic] creates a rectangular box containing the picture
	[pic]. Optional padding is given by arguments [dx] and [dy];
	default is 2bp.
    @img rect.png *)

  val round_rect : t box_creator
  (** [round_rect pic] creates a rectangular box containing the picture
	[pic], with rounded corners. Optional padding is given by [dx]
	and [dy]; default is 2bp
    @img round_rect.png *)

  val patatoid : t box_creator
  (** [patatoid pic] creates an undefined, vaguely rectangular box
	containing the picture [pic]. It may happen that the content
	overlaps with the box.
    @img patatoid.png *)

  val patatoid2 : t box_creator
  (** [patatoid2 pic] creates an undefined, vaguely rectangular box
	containing the picture [pic], which is guaranteed to be fully
        contained in the patatoid. *)

  val round_box : t box_creator

  (***
      val round_rect_gen : ?dx:Num.t -> ?dy:Num.t -> ?rx:Num.t -> ?ry:Num.t ->
        Point.t -> Picture.t -> t
        (** [round_rect_gen p pic] creates a rectangular box of center [p] and
    	of contents [pic], with rounded corners of radii [rx] and [ry].
    	Optional padding is given by [dx] and [dy] ; default is 2bp *)
    ***)

  (** Get the bounding path of a box *)
  val bpath : t -> Path.t
  (** Set the bounding path of a box *)

  val set_bpath : Path.t -> t -> t
  (** Set the bounding path of a box *)

  (** {2 Special points on a box} *)

  val ctr : t -> Point.t
  (** @img ctr.png *)

  (** @img ctr.png *)
  val north : t -> Point.t
  (** @img north.png *)

  (** @img north.png *)
  val south : t -> Point.t
  (** @img south.png *)

  (** @img south.png *)
  val west : t -> Point.t
  (** @img west.png *)

  (** @img west.png *)
  val east : t -> Point.t
  (** @img east.png *)

  (** @img east.png *)
  val north_west : t -> Point.t
  (** @img north_west.png *)

  (** @img north_west.png *)
  val south_west : t -> Point.t
  (** @img south_west.png *)

  (** @img south_west.png *)
  val north_east : t -> Point.t
  (** @img north_east.png *)

  (** @img north_east.png *)
  val south_east : t -> Point.t
  (** @img south_east.png *)

  type vposition = [ Command.vposition | `Custom of t -> Num.t ]

  type hposition = [ Command.hposition | `Custom of t -> Num.t ]

  type position = [ Command.position | `Custom of t -> Point.t ]

  val corner : position -> t -> Point.t

  val opposite_position : position -> position
  (** Return the opposite position of a position (west for east, southeast
        for northwest, center for center, ...). *)

  (** {2 Operators} *)

  val height : t -> Num.t
  (** return the height of the box
   @img height.png *)

  (** return the height of the box
   @img height.png *)
  val width : t -> Num.t
  (** return the width of the box
   @img width.png *)

  (** return the width of the box
   @img width.png *)
  val shift : Point.t -> t -> t
  (** [shift pt x] shifts the box [x] about the point [pt]
   @img shift.png *)

  (** [shift pt x] shifts the box [x] about the point [pt]
   @img shift.png *)
  val center : Point.t -> t -> t
  (** [center pt x] centers the box [x] at the point [pt]
   @img center.png *)

  val draw : ?debug:bool -> t -> Command.t
  (** Draws a box
	@param debug if set to to true, the bounding
	path and the center of the box are drawn as well, default is false
    *)

  val group : ?style:style -> t list box_creator
  (** [group bl] groups a list of boxes [bl] into a single box *)

  (** {2 Boxes alignment} *)

  val halign : ?pos:vposition -> Num.t -> t list -> t list
  (** [halign ~pos y l] vertically moves the boxes in [l] such that
        the vertical position given by [pos] is equal to [y]. The default
        value of [pos] is `Center, so by default this function moves each
        box such that the y coordinate of its center is [y]. The
        horizontal position of each box is unchanged. @img halign.png *)

  val valign : ?pos:hposition -> Num.t -> t list -> t list
  (** the vertical counterpart of [valign]. *)

  val hplace :
    ?padding:Num.t ->
    ?pos:position ->
    ?min_width:Num.t ->
    ?same_width:bool ->
    t list ->
    t list
  (** [hplace l] places the boxes of [l] horizontally, from left to right
       following the order of list elements, without changing their vertical
       position.
       @param min_width minimum width of all boxes; default is zero
       @param same_width if [true], all boxes are of same width,
               and at least of [min_width]; default is false
       @img hplace.png
               *)

  (** [hplace l] places the boxes of [l] horizontally, from left to right
       following the order of list elements, without changing their vertical
       position.
       @param min_width minimum width of all boxes; default is zero
       @param same_width if [true], all boxes are of same width,
               and at least of [min_width]; default is false
       @img hplace.png
               *)
  val vplace :
    ?padding:Num.t ->
    ?pos:position ->
    ?min_height:Num.t ->
    ?same_height:bool ->
    t list ->
    t list
  (** the vertical counterpart of [hplace] *)

  val hbox :
    ?padding:Num.t ->
    ?pos:position ->
    ?style:style ->
    ?min_width:Num.t ->
    ?same_width:bool ->
    t list box_creator
  (** places the given boxes horizontally, aligning them horizontally, and
          returns a box containing these boxes as sub-components. Leave the
          first box at its place. [hbox l] actually gives the same result as
          [group (hplace (halign l))].
	  @param padding horizontal padding used to separate the boxes;
	  defaults to 0
	  @param pos used to determine the way boxes are aligned; defaults to
	  [`Center]
      @img hbox.png
      *)

  val hbox_list :
    ?padding:Num.t ->
    ?pos:position ->
    ?min_width:Num.t ->
    ?same_width:bool ->
    t list ->
    t list
  (** as [hbox], but does not group the resulting boxes into a
        surrounding box; it returns the list of placed boxes instead.
        [hbox_list l] is equal to [hplace (halign l)]. *)

  val vbox :
    ?padding:Num.t ->
    ?pos:position ->
    ?style:style ->
    ?min_height:Num.t ->
    ?same_height:bool ->
    t list box_creator
  (** aligns the given boxes vertically and returns a box containing
	  these boxes as sub-components. Leave the first box at its place.
	  @param padding vertical padding used to separate the boxes
	  @param pos used to determine the way boxes are aligned
      *)

  val vbox_list :
    ?padding:Num.t ->
    ?pos:position ->
    ?min_height:Num.t ->
    ?same_height:bool ->
    t list ->
    t list

  (* as vbox_list, but does not group the resulting boxes into a surrounding
     box *)

  val tabular :
    ?hpadding:Num.t ->
    ?vpadding:Num.t ->
    ?pos:Command.position ->
    ?style:style ->
    ?name:string ->
    ?stroke:Color.t option ->
    ?pen:Pen.t ->
    ?dash:Dash.t ->
    ?fill:Color.t ->
    t array array ->
    t
  (** aligns the given boxes both vertically and horizontally and returns
	a box containing all these boxes (with rows as first sub-components,
	and then individual boxes as sub-components of each row).
	Columns (resp. rows) are separated by [hpadding] (resp. [vpadding]);
	both default to 0.
	Alignment within columns and rows is controlled using [pos].
	The arrays for rows must have the same lengths; otherwise
	[Invalid_argument] is raised. *)

  val tabularl :
    ?hpadding:Num.t ->
    ?vpadding:Num.t ->
    ?pos:Command.position ->
    ?style:style ->
    ?name:string ->
    ?stroke:Color.t option ->
    ?pen:Pen.t ->
    ?dash:Dash.t ->
    ?fill:Color.t ->
    t list list ->
    t
  (** similar to [tabular], but using lists instead of arrays *)

  val tabulari :
    ?hpadding:Num.t ->
    ?vpadding:Num.t ->
    ?pos:Command.position ->
    ?style:style ->
    ?name:string ->
    ?stroke:Color.t option ->
    ?pen:Pen.t ->
    ?dash:Dash.t ->
    ?fill:Color.t ->
    int ->
    int ->
    (int -> int -> t) ->
    t
  (** similar to [tabular], but using a matrix defined with a function *)

  val hblock :
    ?padding:Num.t ->
    ?pos:Command.position ->
    ?name:string ->
    ?stroke:Color.t option ->
    ?pen:Pen.t ->
    ?dash:Dash.t ->
    ?min_width:Num.t ->
    ?same_width:bool ->
    t list ->
    t
  (** [hblock bl] aligns the boxes of [bl] horizontally and surround
	them with new rectangular boxes of the same height; all these new
	boxes are packed together into the returned box.
        @param min_width minimum width of all boxes; default is zero
        @param same_width if [true], all boxes are of same width,
               and at least of [min_width]; default is false*)

  val vblock :
    ?padding:Num.t ->
    ?pos:Command.position ->
    ?name:string ->
    ?stroke:Color.t option ->
    ?pen:Pen.t ->
    ?dash:Dash.t ->
    ?min_height:Num.t ->
    ?same_height:bool ->
    t list ->
    t
  (** similar to [hblock], with vertical alignment.
      @param min_height minimum height of all boxes; default is zero
      @param same_height if [true], all boxes are of same height, and at
      least of [min_height]; default is false*)

  val grid :
    ?hpadding:Num.t ->
    ?vpadding:Num.t ->
    ?pos:Command.position ->
    ?stroke:Color.t option ->
    ?pen:Pen.t ->
    ?dash:Dash.t ->
    t array array ->
    t
  (** Aligns the given boxes in a way that is similar to [hblock]
	and [vblock]: boxes are aligned in a grid where all cells have
	the same size. Each one of these cells is a box containing the
	original corresponding box. *)

  val gridl :
    ?hpadding:Num.t ->
    ?vpadding:Num.t ->
    ?pos:Command.position ->
    ?stroke:Color.t option ->
    ?pen:Pen.t ->
    ?dash:Dash.t ->
    t list list ->
    t
  (** similar to [grid], but using lists instead of arrays *)

  val gridi :
    ?hpadding:Num.t ->
    ?vpadding:Num.t ->
    ?pos:Command.position ->
    ?stroke:Color.t option ->
    ?pen:Pen.t ->
    ?dash:Dash.t ->
    int ->
    int ->
    (int -> int -> t) ->
    t
  (** similar to [gridi], but using a matrix defined with a function *)

  val place : position -> ?pos:position -> ?padding:Num.t -> t -> t -> t
  (** Place a box relatively to another box.

        [place `East a] is a function which places a box at the east
        of [a]. Thus, [place `East a b] returns a copy of [b] placed
        at the east of [a].

        [place posa ~pos: posb ~padding a b] returns a new box [c] which is
        obtained by moving [b] to place the [posa] point of [a] on top of the
        [posb] point of [b], and then padding the result by [padding] in
        direction [posa].

        Default value of [posb] is the opposite direction of [posa] wrt. the
        center (see {!opposite_position}).
        Default value of [padding] is zero.

        The padding argument multiplies a unit vector which goes from
        the center of [a] to the corner of [a] indicated by [posa].
        This effectively places point [posa] of [a] at exactly
        [padding] units of point [posb] of [b], in direction [posa].
        This also means that for diagonal directions, the actual
        direction will change according to the width / height ratio of
        [a]. *)

  val relative : t -> t -> t
  (** After using one of the previous function which give you a group
      [g] of box from multiple box (one of them must be [b]), you can
      use [relative b g] to translate [g] so that [b] inside [g] is
      at the same place than before.
      Indeed it's just {!sub} and a translation.

      {!relative} is really useful when someone make a slideshow from a
      succession of figures. *)

  (** {2 Sub-boxes accessors} *)

  val nth : int -> t -> t
  (** [nth i b] returns the [i]-th sub-box of [b]. The first sub-box
	has index 0. Raises [Invalid_argument] if there is no such sub-box. *)

  val get : string -> t -> t
  (** [get n b] returns the sub-box of [b] of name [n], if any, and
	raises [Invalid_argument] otherwise. The behavior is not specified
	if [b] contains several sub-boxes with name [n]. *)

  val sub : t -> t -> t
  (** [sub b1 b] returns the sub-box of [b] which has the same name as [b1],
     if any, and raises [Invalid_argument] otherwise. The behavior is not
     specified if [b] contains several sub-boxes with the name of [b1]. *)

  val elts : t -> t array
  (** [elts b] returns the sub-boxes of [b]; returns the empty array for
        the empty box or a box containing a picture. *)

  (** {2 Specials Points} *)

  val setp : string -> Point.t -> t -> t

  val getp : string -> t -> Point.t

  val getpx : string -> t -> Num.t

  val getpy : string -> t -> Num.t

  (** {2 Box properties} *)

  val get_fill : t -> Color.t option

  val set_fill : Color.t -> t -> t

  val get_stroke : t -> Color.t option

  val set_stroke : Color.t -> t -> t

  val clear_stroke : t -> t

  val get_name : t -> string option

  val set_name : string -> t -> t

  val get_pen : t -> Pen.t option

  val set_pen : Pen.t -> t -> t

  val set_height : Command.vposition -> Num.t -> t -> t

  val set_width : Command.hposition -> Num.t -> t -> t

  val get_dash : t -> Dash.t option

  val set_dash : Dash.t -> t -> t

  val clear_dash : t -> t

  val set_height2 : vposition -> Num.t -> vposition -> Num.t -> t -> t
  (** set_height2 `North y1 `South y2 b return the box b with its height and
     center chosen such as the ordinate of the top (because of `North) of the
     box is at y1 and the ordinate of its south is at y2*)

  val set_width2 : hposition -> Num.t -> hposition -> Num.t -> t -> t

  val set_size : Command.position -> width:Num.t -> height:Num.t -> t -> t

  val same_height : ?pos:vposition -> t list -> t list

  val same_width : ?pos:hposition -> t list -> t list

  val same_size : ?pos:position -> t list -> t list

  val henlarge : t list -> t list
  (** [henlarge l] set the west boundaries of the box in [l] to the
      westest boundaries of the box in [l]. same for the east
      boundaries *)

  val venlarge : t list -> t list
  (** same as {!henlarge} for vertical boundaries *)

  val set_post_draw : (t -> Command.t) -> t -> t

  val add_post_draw : (t -> Command.t) -> t -> t

  val clear_post_draw : t -> t

  val set_pre_draw : (t -> Command.t) -> t -> t

  val clear_pre_draw : t -> t

  (** {2 Misc.} *)

  val shadow : t -> t

  val cpath :
    ?style:Path.joint ->
    ?outd:Path.direction ->
    ?ind:Path.direction ->
    ?sep:Num.t ->
    t ->
    t ->
    Path.t
  (** the path that connects 2 boxes and stops at the box boundaries *)

  val cpath_left :
    ?style:Path.joint ->
    ?outd:Path.direction ->
    ?ind:Path.direction ->
    ?sep:Num.t ->
    t ->
    Point.t ->
    Path.t
  (** the path that connects a box and a point and stops at the box
        boundaries *)

  val cpath_right :
    ?style:Path.joint ->
    ?outd:Path.direction ->
    ?ind:Path.direction ->
    ?sep:Num.t ->
    Point.t ->
    t ->
    Path.t
  (** the path that connects a box and a point and stops at the box
        boundaries *)

  val transform : Transform.t -> t -> t

  val scale : Num.t -> t -> t

  val rotate : float -> t -> t

  val yscale : Num.t -> t -> t

  val xscale : Num.t -> t -> t

  (** {2 Boxlike : An argument for functor of object that are similar to box} *)

  val set_pos : Point.t -> t -> t
  (** same as center *)
end

(** Tree-like, triangular shapes,
    with a root on top and an horizontal bottom line. *)
module Triangle : sig
  type t = Box.t
  (** A triangular shape is a box.
      In the following, we simply call it a "triangle". *)

  val create :
    ?brush:Brush.t ->
    ?stroke:Color.t option ->
    ?pen:Pen.t ->
    ?dash:Dash.t ->
    ?fill:Color.t ->
    ?left:float ->
    ?right:float ->
    ?width:Num.t ->
    depth:Num.t ->
    unit ->
    Box.t
  (** [create depth] creates a triangular shape of depth [depth].
        The width is equal to [depth], unless specified using [width].
        [left] and [right] control the horizontal position of the root;
        they default to [0.5] i.e. the root is centered. Negative values for
        [left] and [right] are allowed. *)

  val pic :
    ?brush:Brush.t ->
    ?stroke:Color.t option ->
    ?pen:Pen.t ->
    ?dash:Dash.t ->
    ?fill:Color.t ->
    ?dx:float ->
    ?dy:float ->
    Picture.t ->
    t
  (** builds a triangle surrounding a picture *)

  (** builds a triangle surrounding a picture *)
  val tex :
    ?brush:Brush.t ->
    ?stroke:Color.t option ->
    ?pen:Pen.t ->
    ?dash:Dash.t ->
    ?fill:Color.t ->
    ?dx:float ->
    ?dy:float ->
    string ->
    t
  (** builds a triangle surrounding a LaTeX label *)

  val root : t -> Point.t

  val bottom_left : t -> Point.t

  val bottom_right : t -> Point.t
  (** Special points in a triangle. *)

  val draw : ?debug:bool -> t -> Command.t

  val root_label : ?pos:Command.position -> Picture.t -> t -> t

  val tex_root_label : ?pos:Command.position -> string -> t -> t
  (** Attach a label to the root. *)

  val x_depth : ?x:float -> ?depth:float -> t -> Point.t
  (** a point inside a triangle is denoted using two coordinates
        [x] and [depth]. Coordinate [x] controls horizontal placement,
        with 0.0 for left and 1.0 for right (negative values are allowed).
        Coordinate [depth] controls vertical placement, with 0.0 for root
        and 1.0 for bottom line (negative values are allowed).
        [x] defaults to [0.5] (centered) and [depth] defaults to [1.0]
        (bottom). *)

  val label :
    ?x:float -> ?depth:float -> ?pos:Command.position -> Picture.t -> t -> t

  val tex_label :
    ?x:float -> ?depth:float -> ?pos:Command.position -> string -> t -> t
  (** Attach a label to the point (x, depth).
        Coordinate [depth] defaults to 0.7, so that label is likely to fit
        nicely in the triangle (otherwise, you may consider using functions
        [pic] and [tex] above). *)

  val anchor : ?x:float -> ?depth:float -> t -> t -> t
  (** [anchor t2 t1] moves triangle [t1] at position (x, depth) in
        triangle [t2] *)

  val pose : ?x:float -> ?depth:float -> t -> t -> t
  (** [pose t2 t1] moves triangle [t1] so that its south point is
        at position (x, depth) in triangle [t2] *)

  val pose_left : ?x:float -> ?depth:float -> t -> t -> t
  (** [pose t2 t1] moves triangle [t1] so that its south point is
        at position (x, depth) in triangle [t2] *)

  val pose_right : ?x:float -> ?depth:float -> t -> t -> t
  (** similarly, using [bottom_left] and [bottom_right] instead of
        south point *)
end

(** Draw arrows and build new forms of arrows. *)
module Arrow : sig
  (** Draw simple or complex arrows. *)

  (** To draw an arrow, choose your arrow [kind], then call the [draw] function
      (giving the path that the arrow will follow) or the [draw2] function
      (giving the starting and ending points of the arrow). If your favorite
      arrow [kind] does not exist, use the tools from this module to build your
      own! *)

  type kind
  (** The abstract type for arrow kinds. *)

  (** {2 Drawing Arrows} *)

  (** If you need to place a label which is not TeX but any picture, if
      you need to place it at a symbolic position on the path,
      or if you need to place more than one label, you cannot do it directly
      using the [draw] commands. First draw the arrow, then use functions such
      as {!Command.label}. *)

  val simple :
    ?color:Color.t ->
    ?brush:Brush.t ->
    ?pen:Pen.t ->
    ?dashed:Dash.t ->
    Path.t ->
    Command.t
  (** Draw a simple arrow following the given path.
        @param color the color of the arrow
        @param pen the pen to use to draw the body of the arrow
        @param dashed the dash pattern to use to draw the body of the arrow *)

  val draw :
    ?kind:kind ->
    ?tex:string ->
    ?pos:float ->
    ?anchor:Command.position ->
    Path.t ->
    Command.t
  (** Draw an arrow following the given path.
        @param kind the kind of arrow (default is {!triangle_full})
        @param tex add a LaTeX label
        @param pos label position on the path
        @param anchor label anchor *)

  type ('a, 'b) arrow_from_to =
    ?kind:kind ->
    ?tex:string ->
    ?pos:float ->
    ?anchor:Command.position ->
    ?style:Path.joint ->
    ?outd:Path.direction ->
    ?ind:Path.direction ->
    ?sep:Num.t ->
    'a ->
    'b ->
    Command.t
  (**
     {ul {- [kind] the kind of arrow (default is {!triangle_full})}
     {- [tex] add a LaTeX label}
     {- [pos] label position on the path}
     {- [anchor] label anchor}
     {- [outd] the outgoing direction, at the beginning of the arrow}
     {- [ind] the ingoing direction, at the end of the arrow}
     {- [sep] add a little separation of the given distance}
     }
    *)

  val point_to_point : (Point.t, Point.t) arrow_from_to
  (** Use [point_to_point a b] to draw an arrow from [a] to [b]. *)

  val box_to_box : (Box.t, Box.t) arrow_from_to
  (** Use [box_to_box] to draw an arrow from [a] to [b], stopping at the
        box boundaries. The arguments are the same as those of
        [point_to_point]. *)

  val point_to_box : (Point.t, Box.t) arrow_from_to
  (** Use [point_to_box] to draw an arrow from [a] to [b], stopping at the
        box boundaries. The arguments are the same as those of
        [point_to_point]. *)

  val box_to_point : (Box.t, Point.t) arrow_from_to
  (** Use [box_to_point] to draw an arrow from [a] to [b], stopping at the
        box boundaries. The arguments are the same as those of
        [point_to_point]. *)

  (** {2 Built-in Kinds} *)

  val classic : kind
  (** A simple arrow with one line and two straight lines for the head. *)

  val triangle : kind
  (** A simple arrow with a triangular head. Same as [classic] but with an
        extra line and some clipping. *)

  val triangle_full : kind
  (** A simple arrow with a triangular head filled with black. *)

  val implies : kind
  (** An arrow made of two parallel lines and a classic head. *)

  val iff : kind
  (** An arrow made of two parallel lines, a classic head and a classic
        foot. *)

  val mk_classic : ?color:Color.t -> unit -> kind

  val mk_implies : ?color:Color.t -> unit -> kind

  (** {2 Heads} *)

  type head_description
  (** The type of head descriptions (see [make_head] and [head] below). *)

  val make_head : ?cut:Path.t -> Command.t -> head_description
  (** Head description constructor. The command parameter is used to draw
        the head.
        @param cut a path that can be used to cut the arrow body lines
        (only used by heads and feet, not by belts) *)

  type head = Point.t -> Point.t -> head_description
  (** If [h] is a head, [h p d] returns a head description used to
        draw the head at point [p] with direction [d]. Direction [d]
        is normalized before being given to the function. *)

  val head_classic :
    ?color:Color.t ->
    ?brush:Brush.t ->
    ?pen:Pen.t ->
    ?dashed:Dash.t ->
    ?angle:float ->
    ?size:Num.t ->
    head
  (** A simple head with two straight lines.
        @param color the color of the head; default is black
        @param pen the pen used to draw the head; default is
               [Brush.Pen.default]
        @param dashed if given, the head is drawn using that dash_style
        @param angle the angle between the two lines in degrees, default is 60
        degrees
        @param size the length of the two lines, default is 4bp *)

  val head_triangle :
    ?color:Color.t ->
    ?brush:Brush.t ->
    ?pen:Pen.t ->
    ?dashed:Dash.t ->
    ?angle:float ->
    ?size:Num.t ->
    head
  (** Same as [head_classic] except that the two lines are joined together
      to form a triangle. *)

  val head_triangle_full : ?color:Color.t -> ?angle:float -> ?size:Num.t -> head
  (** Same as [head_triangle] except that the triangle is not drawn (hence the
      absence of pen properties) but is filled with the given [color]. *)

  (** {2 Building Your Own Kinds} *)

  (** Start from the empty kind [empty] and add features to it using
      [add_line], [add_head], ... *)

  val empty : kind
  (** The empty kind with no line nor head. *)

  val add_line :
    ?brush:Brush.t ->
    ?dashed:Dash.t ->
    ?color:Color.t ->
    ?pen:Pen.t ->
    ?from_point:float ->
    ?to_point:float ->
    ?dist:Num.t ->
    kind ->
    kind
  (** Add a line to a body. The line will be parallel to the path used
        to draw the arrow.
        @param dashed the dash style used to draw the line (default is plain)
        @param color the color of the line (default is black)
        @param pen the pen used to draw the line
               (default is [Brush.Pen.default])
        @param from_point from [0.] (foot of the arrow) to [1.] (head of the
          arrow), the line will start from this point
        @param to_point from [0.] (foot of the arrow) to [1.] (head of the
          arrow), the line will end at this point
        @param dist the distance between the path of the arrow and this line
          (positive values are on the right of the arrows) *)

  val add_head : ?head:head -> kind -> kind
  (** Add a head at the end of the arrow.
        @param head the kind of head to add (default is {!head_classic}) *)

  val add_foot : ?head:head -> kind -> kind
  (** Add a foot (an inverted head) at the beginning of the arrow.
        @param head the kind of head to add (default is {!head_classic}) *)

  val add_belt :
    ?clip:bool -> ?rev:bool -> ?point:float -> ?head:head -> kind -> kind
  (** Add an arrow head at any point of an arrow.
        @param clip if [true], the arrow lines will be clipped after the belt
          (or before if the [rev] is [true]) (default is [false])
        @param rev if [true], the head will be drawn in the opposite direction
          (default is [false])
        @param point the point where to draw the arrow ([0.] for the beginning,
          and [1.] for the end, or any number in-between) (default is [0.5])
        @param head the kind of head to add (default is {!head_classic}) *)

  (** {2 Miscellaneous} *)

  (** Warning: the following functions might be either deleted, modified
      and / or moved somewhere else. Don't use them if you need some backward
      compatibility. *)

  val draw_thick :
    ?style:Path.joint ->
    ?boxed:bool ->
    ?line_color:Color.t ->
    ?fill_color:Color.t ->
    ?outd:Path.direction ->
    ?ind:Path.direction ->
    ?width:Num.t ->
    ?head_length:Num.t ->
    ?head_width:Num.t ->
    Point.t ->
    Point.t ->
    Command.t
  (** Draw a thick arrow. *)
end

(** {2 Helpers and Extensions} *)

(** A few helper functions *)
module Helpers : sig
  val dotlabels :
    ?pos:Command.position -> string list -> Point.t list -> Command.t

  val draw_simple_arrow :
    ?color:Color.t ->
    ?pen:Pen.t ->
    ?dashed:Dash.t ->
    ?style:Path.joint ->
    ?outd:Path.direction ->
    ?ind:Path.direction ->
    ?sep:Num.t ->
    Point.t ->
    Point.t ->
    Command.t

  val draw_label_arrow :
    ?color:Color.t ->
    ?pen:Pen.t ->
    ?dashed:Dash.t ->
    ?style:Path.joint ->
    ?outd:Path.direction ->
    ?ind:Path.direction ->
    ?pos:Command.position ->
    ?sep:Num.t ->
    Picture.t ->
    Point.t ->
    Point.t ->
    Command.t

  val draw_labelbox_arrow :
    ?color:Color.t ->
    ?pen:Pen.t ->
    ?dashed:Dash.t ->
    ?style:Path.joint ->
    ?outd:Path.direction ->
    ?ind:Path.direction ->
    ?pos:Command.position ->
    ?sep:Num.t ->
    Box.t ->
    Point.t ->
    Point.t ->
    Command.t

  val box_arrow :
    ?within:Box.t ->
    ?color:Color.t ->
    ?pen:Pen.t ->
    ?dashed:Dash.t ->
    ?style:Path.joint ->
    ?outd:Path.direction ->
    ?ind:Path.direction ->
    ?sep:Num.t ->
    Box.t ->
    Box.t ->
    Command.t
  (** Draw an arrow between two boxes. The options [pen], [dashed], [color]
    change the drawing of the arrow. [outd] and [ind] specify the outgoing and
    ingoing direction. [sep] specifies the distance of the arrow ends to both
    boxes. If [within] is set, the boxes will be searched within the box
    [within]. *)

  val box_line :
    ?within:Box.t ->
    ?color:Color.t ->
    ?pen:Pen.t ->
    ?dashed:Dash.t ->
    ?style:Path.joint ->
    ?outd:Path.direction ->
    ?ind:Path.direction ->
    ?sep:Num.t ->
    Box.t ->
    Box.t ->
    Command.t
  (** Draw an arrow between two boxes. The options [pen], [dashed], [color]
    change the drawing of the arrow. [outd] and [ind] specify the outgoing and
    ingoing direction. [sep] specifies the distance of the arrow ends to both
    boxes. If [within] is set, the boxes will be searched within the box
    [within]. *)

  val box_point_line :
    ?within:Box.t ->
    ?color:Color.t ->
    ?pen:Pen.t ->
    ?dashed:Dash.t ->
    ?style:Path.joint ->
    ?outd:Path.direction ->
    ?ind:Path.direction ->
    ?sep:Num.t ->
    Box.t ->
    Point.t ->
    Command.t

  val point_box_line :
    ?within:Box.t ->
    ?color:Color.t ->
    ?pen:Pen.t ->
    ?dashed:Dash.t ->
    ?style:Path.joint ->
    ?outd:Path.direction ->
    ?ind:Path.direction ->
    ?sep:Num.t ->
    Point.t ->
    Box.t ->
    Command.t

  val box_label_arrow :
    ?within:Box.t ->
    ?color:Color.t ->
    ?pen:Pen.t ->
    ?dashed:Dash.t ->
    ?style:Path.joint ->
    ?outd:Path.direction ->
    ?ind:Path.direction ->
    ?sep:Num.t ->
    ?pos:Command.position ->
    Picture.t ->
    Box.t ->
    Box.t ->
    Command.t

  val box_label_line :
    ?within:Box.t ->
    ?color:Color.t ->
    ?pen:Pen.t ->
    ?dashed:Dash.t ->
    ?style:Path.joint ->
    ?outd:Path.direction ->
    ?ind:Path.direction ->
    ?sep:Num.t ->
    ?pos:Command.position ->
    Picture.t ->
    Box.t ->
    Box.t ->
    Command.t

  val box_labelbox_arrow :
    ?within:Box.t ->
    ?color:Color.t ->
    ?pen:Pen.t ->
    ?dashed:Dash.t ->
    ?style:Path.joint ->
    ?outd:Path.direction ->
    ?ind:Path.direction ->
    ?sep:Num.t ->
    ?pos:Command.position ->
    Box.t ->
    Box.t ->
    Box.t ->
    Command.t

  val box_pointer_arrow :
    ?within:Box.t ->
    ?color:Color.t ->
    ?pen:Pen.t ->
    ?dashed:Dash.t ->
    ?style:Path.joint ->
    ?outd:Path.direction ->
    ?ind:Path.direction ->
    Box.t ->
    Box.t ->
    Command.t

  val box_loop :
    ?within:Box.t ->
    ?color:Color.t ->
    ?pen:Pen.t ->
    ?dashed:Dash.t ->
    ?style:Path.joint ->
    ?outd:Path.direction ->
    ?ind:Path.direction ->
    ?sep:Num.t ->
    ?pos:Box.position ->
    ?dist:float ->
    ?angle:float ->
    Box.t ->
    Command.t
  (** Position [pos] defaults to [`South].
      [angle] is the opening angle of the loop, in degrees; it defaults to 90.
      [dist] is the relative distance between the box center and the
      furthest point of the loop (when compared to the distance between the
      center and the corner [pos]); it defaults to 2. *)

  val box_label_loop :
    ?within:Box.t ->
    ?color:Color.t ->
    ?pen:Pen.t ->
    ?dashed:Dash.t ->
    ?style:Path.joint ->
    ?outd:Path.direction ->
    ?ind:Path.direction ->
    ?sep:Num.t ->
    ?pos:Box.position ->
    ?dist:float ->
    ?angle:float ->
    Picture.t ->
    Box.t ->
    Command.t

end

(** Create and draw trees *)
module Tree : sig
  (** This module provides high-level means for creating and drawing Trees *)

  type t
  (** The type of trees *)

  (** The style of arrows between nodes *)
  type arrow_style =
    | Directed
        (** edges are directed and an
		       arrow is drawn at the end of an edge *)
    | Undirected  (** edges are undirected and no arrow is drawn *)

  (** There are several styles available for edges *)
  type edge_style =
    | Straight  (** edges are straight lines between nodes *)
    | Curve  (** edges are curved lines between nodes *)
    | Square
        (** edges are straight lines and
		      branch out from the sides of nodes *)
    | HalfSquare
        (** edges are straight lines and
		      branch out from below nodes *)

  (** {2 Creation} *)
  val leaf : Box.t -> t
  (** [leaf b] creates a leaf with Box [b]. *)

  val node :
    ?ls:Num.t ->
    ?cs:Num.t ->
    ?arrow_style:arrow_style ->
    ?edge_style:edge_style ->
    ?stroke:Color.t ->
    ?brush:Brush.t ->
    ?pen:Pen.t ->
    ?sep:Num.t ->
    Box.t ->
    t list ->
    t
  (** [node label children] creates a node with label [label] and a list
	of children [children].
        Default arrow_style is [Directed].
	Default edge_style is [Straight].
	- [ls] (level sep): vertical distance between levels.
        The default value is 1.0. A negative value draws the tree upward.
	- [cs] (children sep): horizontal distance between siblings.
        The default value is 0.2.
	Optional arguments are the same as in [leaf]. *)

  val nodel :
    ?ls:Num.t ->
    ?cs:Num.t ->
    ?arrow_style:arrow_style ->
    ?edge_style:edge_style ->
    ?stroke:Color.t ->
    ?brush:Brush.t ->
    ?pen:Pen.t ->
    ?sep:Num.t ->
    Box.t ->
    (t * (Command.position * Picture.t)) list ->
    t
  (** Similar to [node] but with labels on edges.
        Labels are taken into account only when [edge_style] is [Straight]. *)

  val bin :
    ?ls:Num.t ->
    ?cs:Num.t ->
    ?arrow_style:arrow_style ->
    ?edge_style:edge_style ->
    ?stroke:Color.t ->
    ?brush:Brush.t ->
    ?pen:Pen.t ->
    ?sep:Num.t ->
    Box.t ->
    t ->
    t ->
    t
  (** [bin label l r] creates a binary node with label [label] and
	children [l] and [r].
	Optional arguments are the same as in [leaf]. *)

  val to_box : t -> Box.t

  val draw : ?debug:bool -> t -> Command.t

  module Simple : sig
    type t

    val leaf : Box.t -> t

    val node :
      ?ls:Num.t ->
      ?cs:Num.t ->
      ?arrow_style:arrow_style ->
      ?edge_style:edge_style ->
      ?stroke:Color.t ->
      ?brush:Brush.t ->
      ?pen:Pen.t ->
      ?sep:Num.t ->
      ?valign:Command.position ->
      ?halign:Command.position ->
      Box.t ->
      t list ->
      t
    (** a simple tree placement algorithm: align all subtrees horizontally ,
        and place the parent node above.
        Default arrow_style is [Directed].
	Default edge_style is [Straight].
	@param ls (level sep): vertical distance between levels.
        The default value is 1.0. A negative value draws the tree upward.
	@param cs (children sep): horizontal distance between siblings.
        The default value is 0.2.
      @param halign change alignment of children (default: [`Top])
      @param valign change alignment of parent node wrt. children
               (default: [`Children])
    *)

    val bin :
      ?ls:Num.t ->
      ?cs:Num.t ->
      ?arrow_style:arrow_style ->
      ?edge_style:edge_style ->
      ?stroke:Color.t ->
      ?brush:Brush.t ->
      ?pen:Pen.t ->
      ?sep:Num.t ->
      Box.t ->
      t ->
      t ->
      t

    (* [bin t1 t2] is the same as [node [t1;t2] ] *)

    val to_box : t -> Box.t

    val draw : ?debug:bool -> t -> Command.t
  end
end

(** EXPERIMENTAL: A new way of placing trees. *)
module Tree_adv : sig
  (** This module provides even more high-level means for placing trees. *)

  type 'a t = Node of 'a * 'a t list  (** The type of polymorphic trees *)

  (** {2 Functions for placement} *)

  module Place (X : Signature.Boxlike) : sig
    val gen_place : place:(Box.t t -> Box.t) -> X.t t -> X.t t
    (** This is a generic function for placing trees, provided that the user
          can give us the following functions:
          @param place a function which knows how to place a tree of boxes - it
          should return a box where all the boxes of the input tree appear.
      *)

    val place :
      ?ls:Num.t ->
      ?cs:Num.t ->
      ?valign:Box.position ->
      ?halign:Box.position ->
      X.t t ->
      X.t t
    (** This is an instance of [gen_place] using the tree drawing algorithm
          from the module {!Tree}. *)
  end

  val gen_draw_arrows :
    'c ->
    style:(Point.t -> Point.t -> 'c) ->
    corner:(Box.position -> 'a -> Point.t) ->
    'a t ->
    'c t
  (** draws arrows from a node to its children with a given style *)

  val draw : ('a -> Box.t) -> 'a t -> Command.t
  (** Draws a tree that has already been placed when one knows how to draw its
     elements. *)

  (** {2 Useful functions} *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** apply a function everywhere in the tree *)

  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  (** [map2 f] takes two trees of identical structure and applies the function
    [f] to every pair of nodes. Raise [Invalid_argument] if the trees do not
    have the same structure. *)

  val combine : 'a t -> 'b t -> ('a * 'b) t
  (** Transform a pair of trees into a tree of pairs. Raise [Invalid_argument]
     if the trees do not have the same structure. *)

  val split : ('a * 'b) t -> 'a t * 'b t
  (** Transform a tree of pairs  into a pair of trees. *)

  val root_map : ('a option -> 'a -> 'b) -> 'a t -> 'b t
  (** [root_map f t] calls [f (Some father) node] for each node of [t] and its
     father. It calls [f None root], where [root] is the root of the once, once
     at the beginning. A tree having the same structure is built with the
     results. *)

  val map_children : ('a -> 'a list -> 'b) -> 'a t -> 'b t
  (** [map_children f t] calls [f node children] for each node of [t] and its
     children. A tree having the same structure is built with the results *)

  val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  (** Traverse the tree in a bottom-up, left-to-right order *)

  val fold_child : ('a -> 'b -> 'b -> 'a) -> 'a -> 'b t -> 'a
  (** Traverse the pairs of parent-child in the tree in a bottom-up,
        left-to-right order *)

  val filter : ('a -> bool) -> 'a t -> 'a t
  (** filter f t
        If for a node n of t f n is false then it doesn't appear in the result
        as well as its descendants. If f is false for the root node,
        invalid_argument is raised *)

  val filter_option : ('a -> 'b option) -> 'a t -> 'b t
  (** Suppress a subtree depending on a condition on the node *)

  val wrap_corner_box :
    ('a -> Box.t) -> (corner:(Box.position -> 'a -> Point.t) -> 'c) -> 'c
  (** [wrap_corner_box give_box f] returns [f] where its argument corner has
     been set *)

  (** Tools for overlay aware trees  *)
  module Overlays : sig
    (** This module provides a type to associate an interval of time
        to a value, to control its visibility *)

    (** This type describes an interval of discrete points of time *)
    type interval =
      | Bet of int * int  (** \[|a,b|\] *)
      | Bef of int  (** \]|-oo,a|\] *)
      | Aft of int  (** \[|a,+oo|\[ *)
      | Nev  (** emptyset *)
      | Alw  (** N *)

    val in_interval : int -> interval -> bool
    (** test if an integer is in an interval *)

    val min_interval : int -> interval -> int
    (** The minimum between the integer argument and the beginning of the
        interval; returns the integer argument in the cases [Nev] and [Alw] *)

    val max_interval : int -> interval -> int
    (** The dual of [min_interval] *)

    val min_tree : ('a -> interval) -> 'a t -> int
    (** The first moment of the tree to appear, not considering [Nev] and
           [Alw] *)

    val max_tree : ('a -> interval) -> 'a t -> int
    (** The last moment of the tree to appear, not considering [Nev] and
           [Alw] *)

    type 'a spec = (interval * 'a) list
    (** A spec is a list of objects associated with a visibility
                interval *)

    val assoq : int -> 'a spec -> 'a
    (** returns the first element which is visible in the specification;
              raises [Not_found] if no element is visible *)

    val max : ('a -> Num.t) -> ('b * 'a) list -> Num.t
    (** given a function to compute a numeric from an ['a], and a
              list of objects [('b,'a)], return the maximal numeric from
              that list; intended to be used with width and height functions
              for objects and with a ['a spec list] *)

    val set_pos : (Point.t -> 'a -> 'b) -> Point.t -> 'a spec -> 'b spec
    (** Given a function to move objects of type ['a], return a
              function to move functions of type ['a spec] *)
  end

  module Overlays_Boxlike (X : Signature.Boxlike) :
    Signature.Boxlike with type t = X.t Overlays.spec
end

(** Create simple diagrams by placing objects in a table. Deprecated. *)
module Diag : sig
  (** Diagrams. *)

  (** This module permits to create diagrams in a very simple and yet quite
   flexible fashion. It permits to specify content, form and color of nodes as
   well as color, form and labels of arrows between nodes. Nodes have to be
   placed by hand, though *)

  (** {2 Creation} *)

  type node
  (** The abstract type of nodes *)

  type node_style = Box.t -> Box.t
  (** The type for node styles; It corresponds to the type of the box
    creation functions in the {!Box} module *)

  val node :
    ?style:node_style ->
    ?fill:Color.t ->
    ?boxed:bool ->
    float ->
    float ->
    Box.t ->
    node
  (** Construct a node at a given position with a given content in Latex
        format and a box style *)

  type t
  (** The abstract type of diagrams *)

  val create : node list -> t
  (** Create a diagram that consists of the given nodes *)

  type dir = Up | Down | Left | Right | Angle of float

  val arrow :
    t ->
    ?lab:string ->
    ?line_width:Num.t ->
    ?boxed:bool ->
    ?line_color:Color.t ->
    ?fill_color:Color.t ->
    ?pos:Command.position ->
    ?head:bool ->
    ?dashed:Dash.t ->
    ?outd:dir ->
    ?ind:dir ->
    node ->
    node ->
    unit
  (** [arrow d n1 n2] adds an arrow between n1 and n2 in the diagram d, by
        side effect.
	@param lab The label of the arrow, in Latex format
	@param pos The position of the label, relative to the arrow
	@param line_width Draws a thick arrow of that width, if present
               (experimental)
	@param head If true, the arrow has a head. Otherwise, it's just a line.
	@param outd The outgoing direction of the arrow
	@param ind The ingoing direction of the arrow *)

  (** {2 Drawing} *)

  val draw :
    ?scale:(float -> Num.t) ->
    ?style:node_style ->
    ?boxed:bool ->
    ?fill:Color.t ->
    ?stroke:Color.t ->
    ?pen:Pen.t ->
    t ->
    Command.t
  (** Draws the diagram.
        @param scale The distance between nodes; default is 40 bp
	@param style The style of nodes: circular or rectangular
	(default is circular)
	@param boxed The border is drawn if set (default is true)
	@param fill The color to fill nodes
	@param stroke The color to draw arrows
	@param pen The pen used for arrows *)
end

(** A simple and limited way of plotting functions from int to int. *)
module Plot : sig
  (** Plots. *)

  (** This module helps drawing grids and plotting functions. *)

  type skeleton
  (** The abstract skeleton for grids, axes and functions *)

  val mk_skeleton : int -> int -> Num.t -> Num.t -> skeleton
  (** [mk_skeleton w h dx dy] builds a skeleton of width [w] and height [h],
	each cell being [dx] units wide and [dy] units high. *)

  type labels = int -> Num.t -> Picture.t option

  type ticks = (Num.t * Pen.t) option

  type drawing = Stepwise | Normal

  val draw_grid :
    ?hdash:(int -> Dash.t) ->
    ?vdash:(int -> Dash.t) ->
    ?hpen:(int -> Pen.t) ->
    ?vpen:(int -> Pen.t) ->
    ?color:Color.t ->
    skeleton ->
    Command.t

  val draw_axes :
    ?hpen:Pen.t ->
    ?vpen:Pen.t ->
    ?hlabel:labels ->
    ?vlabel:labels ->
    ?ticks:ticks ->
    ?closed:bool ->
    ?hcaption:Picture.t ->
    ?vcaption:Picture.t ->
    skeleton ->
    Command.t

  val draw_simple_axes :
    ?hpen:Pen.t -> ?vpen:Pen.t -> string -> string -> skeleton -> Command.t

  val draw_func :
    ?pen:Pen.t ->
    ?drawing:drawing ->
    ?style:Path.joint ->
    ?dashed:Dash.t ->
    ?color:Color.t ->
    ?label:Picture.t * Command.position * int ->
    ?from_x:int ->
    ?to_x:int ->
    (int -> float) ->
    skeleton ->
    Command.t
end

(** A simple and limited way of plotting functions from float to
    float. *)
module Real_plot : sig
  type 'a curve
  (** 'a store the information about :
       - the way the curve is drawn (style and color)
       - the label used in the legend
    *)

  val curve : (float -> float) -> 'a -> 'a curve
  (** create a curve from a function and some information of
        drawing *)

  val curve_opt : (float -> float option) -> 'a -> 'a curve
  (** create a curve from a function and some information of
        drawing. If the function return None the function is not
        defined on this value *)

  val curve_l : (float -> float option) list -> 'a -> 'a curve
  (** create a curve from multiple function and some information of
        drawing. The different functions symbolize different part of
        the curve which mustn't be connected *)

  val draw :
    ?logarithmic:bool ->
    (* use a logarithmic scale for ordinate *)
    ?curve_brush:('a -> Brush.t) ->
    (* how to draw a curve *)
    ?label:('a -> string) ->
    (* return the label to use in the legend.
       If no function is given the legend is not drawn *)
    ?ymin:float ->
    ?ymax:float ->
    xmin:float ->
    xmax:float ->
    pitch:float ->
    width:Num.t ->
    height:Num.t ->
    'a curve list ->
    Command.t

  (* Draw a graph. If concrete is supported (Concrete.supported) the
     label of ticks on the axes will not overlap *)
end

(** Draw Bar diagrams (Histograms). *)
module Hist : sig
  (** Histograms. *)

  (** This module draws histograms. *)

  type 'a labels = Values | User of 'a list

  val simple :
    ?width:Num.t ->
    ?height:Num.t ->
    ?padding:Num.t ->
    ?fill:Color.t list ->
    ?perspective:bool ->
    ?hcaption:Picture.t ->
    ?vcaption:Picture.t ->
    ?histlabel:Command.vposition * Picture.t labels ->
    ?vlabel:Plot.labels ->
    ?hlabel:Picture.t list ->
    float list ->
    Command.t
  (** [simple l] draws an histogram from a list [l] of floating-point values.
	@param width Total width of the histogram (default: 100 bp)
	@param height Total height for the histogram (default: 200 bp)
	@param fill The colors used to draw the successive blocks;
	  it is used circularly
	@param padding Horizontal space between two blocks
	@param hcaption See module Plot
	@param vcaption See module Plot
	@param hlabel Labels for each block
	@param vlabel See module Plot
	@param histlabel Add a label to each block; the first component
          controls the placement of the label; the second component, of type
          [insideBox], controls the label itself, which is either the numerical
          value of the block (i.e. the float) or a user picture
    *)

  val compare :
    ?width:Num.t ->
    ?height:Num.t ->
    ?padding:Num.t ->
    ?fill:Color.t list ->
    ?perspective:bool ->
    ?hcaption:Picture.t ->
    ?vcaption:Picture.t ->
    ?histlabel:Command.vposition * Picture.t list labels ->
    ?vlabel:Plot.labels ->
    ?hlabel:Picture.t list ->
    float list list ->
    Command.t
  (** [compare l] draws a comparative histogram from a list [l]
	of floating-point lists.
	For optional arguments, see function [simple] above.
    *)

  val stack :
    ?width:Num.t ->
    ?height:Num.t ->
    ?padding:Num.t ->
    ?fill:Color.t list ->
    ?perspective:bool ->
    ?hcaption:Picture.t ->
    ?vcaption:Picture.t ->
    ?histlabel:Command.vposition * Picture.t list labels ->
    ?vlabel:Plot.labels ->
    ?hlabel:Picture.t list ->
    float list list ->
    Command.t
  (** [compare l] draws a stacked histogram from a list [l]
	of floating-point lists.
	For optional arguments, see function [simple] above.
    *)
end

(** Radar diagrams. *)
module Radar : sig
  (** This module draws radar diagrams.*)

  val stack :
    ?radius:Num.t ->
    ?color:Color.t list ->
    ?pen:Pen.t ->
    ?style:Dash.t list ->
    ?ticks:float ->
    ?label:string list ->
    ?scale:float list ->
    float list list ->
    Picture.t
  (** [stack l] builds a picture from a list [l] of floating-point lists.
	The radars are all drawn on the same picture.
	Each sublist represents one radar datas.
	All sublists must have the same length.
	@param radius The radius of the whole picture
	@param pen The pen used to draw the radars
	@param color The colors used to draw each radar; it is used circularly
	@param style The dash-styles used to draw each radar; it is used
	  circularly
	@param ticks The interval between each ticks along the axes
	  (relative to the values)
	@param scale The size of every axe, relative to the values;
	  when not specified, the maximal value along each axe is chosen
    *)

  val compare :
    ?radius:Num.t ->
    ?color:Color.t list ->
    ?fill:bool ->
    ?pen:Pen.t ->
    ?style:Dash.t list ->
    ?ticks:float ->
    ?label:string list ->
    ?scale:float list ->
    float list list ->
    Picture.t list
  (** [stack l] builds a list of pictures from a list [l] of
	floating-point lists.
	Each picture represents one radar, and all picture have the same size.
	Each sublist represents one radar datas, and all sublists must have
	the same length.
	For most optional arguments, see function [stack] above.
	@param fill Fill the radar with its color.
    *)
end

(** Build a legend for diagrams. *)
module Legend : sig
  val legend :
    ?ensstroke:Color.t ->
    ?colstroke:Color.t ->
    ?fill:Color.t ->
    (Color.t * string) list ->
    Picture.t
end

(** {2 Metapost generation} *)

(* Misc does not appear in the documentation *)
(**/**)

module Misc : sig
  val print_option :
    string ->
    (Format.formatter -> 'a -> unit) ->
    Format.formatter ->
    'a option ->
    unit

  val print_list :
    ('a -> unit -> unit) -> ('a -> 'c -> unit) -> 'a -> 'c list -> unit

  val space : Format.formatter -> unit -> unit

  val comma : Format.formatter -> unit -> unit

  val fold_from_to : ('a -> int -> 'a) -> 'a -> int -> int -> 'a
  (** [fold_from_to f acc i j] is equivalent to
      [List.fold_left f acc [i; i +1; .. j] ],
     where i <= j *)

  val call_cmd : ?inv:bool -> ?outv:bool -> ?verbose:bool -> string -> int
  (** [fold_from_to f acc i j] is equivalent to
      [List.fold_left f acc [i; i +1; .. j] ],
     where i <= j *)
end

(**/**)

(** Functions to generate Metapost files *)
module Metapost : sig
  (**/**)

  (** This code is deprecated. *)
  val set_filename_prefix : string -> unit
  (** Add to the filename given to the emit function this prefix.
        This function is here just for convenience *)

  (* I believe these functions were only used internally. Commented. *)
  (*
  val generate_mp :
    string ->
    ?prelude:string ->
    ?eps:bool ->
    (int * Command.t) list -> unit
    *)

  val generate :
    ?prelude:string ->
    ?verbose:bool ->
    ?clean:bool ->
    string ->
    (string * Command.t) list ->
    unit

  val emit : string -> Command.t -> unit

  val dump :
    ?prelude:string ->
    ?pdf:bool ->
    ?eps:bool ->
    ?verbose:bool ->
    ?clean:bool ->
    string ->
    unit
  (** [dump ?prelude ?pdf f] builds a Metapost file [f.mp] for all figures,
	then runs Metapost on it, and renames figure files according to the
	names specified to [emit]. The file suffix is [.mps] if [pdf] is
	set, and [.1] otherwise. *)

  type job = string * Command.t

  type jobs = job list
  (** Generate files of corresponding type, using the argument of type [jobs],
     and return information about the created files *)

  (* (* FIXME export this function, too *)
     val mp :
       string -> ?prelude:string -> jobs -> File.t * string Misc.IntMap.t
  *)
  val mps : ?prelude:string -> ?verbose:bool -> string -> jobs -> File.t list
  (** Generate files of corresponding type, using the argument of type [jobs],
     and return information about the created files *)

  val pdf : ?prelude:string -> ?verbose:bool -> string -> jobs -> File.t list

  val png : ?prelude:string -> ?verbose:bool -> string -> jobs -> File.t list

  (* (* FIXME export this function, too *)
     val temp_mp :
       ?prelude:string -> ?verbose:bool -> ?clean:bool -> string -> jobs ->
         File.t * string Misc.IntMap.t
  *)
  val temp_mps :
    ?prelude:string ->
    ?verbose:bool ->
    ?clean:bool ->
    string ->
    jobs ->
    File.t list
  (** Same as above, but use a temporary directory *)

  val temp_pdf :
    ?prelude:string ->
    ?verbose:bool ->
    ?clean:bool ->
    string ->
    jobs ->
    File.t list

  val temp_png :
    ?prelude:string ->
    ?verbose:bool ->
    ?clean:bool ->
    string ->
    jobs ->
    File.t list

  val dump_mp :
    ?prelude:string -> ?verbose:bool -> ?clean:bool -> string -> unit
  (** Same as above, but use a temporary directory and take jobs from the job
     queue *)

  val dump_mps :
    ?prelude:string -> ?verbose:bool -> ?clean:bool -> string -> unit

  val dump_png :
    ?prelude:string -> ?verbose:bool -> ?clean:bool -> string -> unit

  val dump_pdf :
    ?prelude:string -> ?verbose:bool -> ?clean:bool -> string -> unit

  val read_prelude_from_tex_file : string -> string
  (** read the prelude from a tex file, until the end of file or the text
       "\begin\{document\}" if it is outside a comment *)

  val dump_tex : ?prelude:string -> string -> unit
  (** [dump_tex ?prelude f] builds a LaTeX file [f.tex] for all the figures,
	using LaTeX prelude [prelude] if given. *)

  val slideshow : Command.t list -> int -> (int * Command.t) list
  (** takes a list of figures and returns a list of figures of exactly the
        same size (the size of the biggest figure). Shared objects are
        hopefully placed at the same absolute location across figures. The
        resulting figures are numbered with consecutive increasing integers,
        starting with the given value. *)

  val emit_slideshow : string -> Command.t list -> unit
  (** emit the list of figures as a slideshow, using the [slideshow]
        function.*)

  val dumpable : unit -> unit

  val depend : string -> unit

  val figures_names : unit -> string list

  (**/**)
end

(** Compute concrete values of numerics, points and paths;
   not always available *)
module Concrete : sig
  val supported : bool

  (** The module of concrete points *)
  module CPoint : sig
    type t = Ctypes.point

    val add : t -> t -> t

    val sub : t -> t -> t

    val opp : t -> t

    val mult : float -> t -> t

    val div : t -> float -> t

    module Infix : sig
      val ( +/ ) : t -> t -> t
      (** alias for {!Concrete.CPoint.add} *)

      val ( -/ ) : t -> t -> t
      (** alias for {!Concrete.CPoint.sub} *)

      val ( */ ) : float -> t -> t
      (** alias for {!Concrete.CPoint.mult} *)

      val ( // ) : t -> float -> t
      (** alias for {!Concrete.CPoint.div} *)
    end

    val print : Format.formatter -> t -> unit
  end

  (** Concrete Paths *)
  module CPath : sig
    (* A path is a succession of splines *)
    type t

    type abscissa = float

    val length : t -> float

    val is_closed : t -> bool

    val is_a_point : t -> CPoint.t option

    val intersection : t -> t -> (abscissa * abscissa) list
    (** intersection p1 p2 return a list of pair of abscissa. In each
          pairs (a1,a2), a1 (resp. a2) is the abscissa in p1 (resp. p2) of
          one intersection point between p1 and p2. Additionnal point of
          intersection (two point for only one real intersection) can
          appear in degenerate case. *)

    val one_intersection : t -> t -> abscissa * abscissa
    (** one_intersection p1 p2 return one of the intersections
          between p1 and p2 or raise Not_found if none exists*)

    val reverse : t -> t
    (** reverse p return the path p reversed *)

    val iter :
      (CPoint.t -> CPoint.t -> CPoint.t -> CPoint.t -> unit) -> t -> unit
    (** iter on all the splines of a path: iter f p applies f
          successively to the splines of p with :
          - the start point of the spline as first argument
          - the control point of the start point as second argument
          - the control point of the end point as third argument
          - the end point as fourth argument *)

    val fold_left :
      ('a -> CPoint.t -> CPoint.t -> CPoint.t -> CPoint.t -> 'a) ->
      'a ->
      t ->
      'a
    (** fold on all the splines of a path *)

    val cut_before : t -> t -> t

    val cut_after : t -> t -> t
    (** remove the part of a path before the first intersection
          or after the last*)

    val split : t -> abscissa -> t * t

    val subpath : t -> abscissa -> abscissa -> t

    val direction_of_abscissa : t -> abscissa -> CPoint.t

    val point_of_abscissa : t -> abscissa -> CPoint.t

    val bounding_box : t -> CPoint.t * CPoint.t

    val dist_min_point : t -> CPoint.t -> float * abscissa

    val dist_min_path : t -> t -> float * (abscissa * abscissa)

    val print : Format.formatter -> t -> unit
  end

  module CTransform : sig
    type t = Ctypes.matrix
  end

  (** {2 Compute the concrete value} *)

  val float_of_num : Num.t -> float

  val cpoint_of_point : Point.t -> CPoint.t

  val cpath_of_path : Path.t -> CPath.t

  val ctransform_of_transform : Transform.t -> CTransform.t

  (** {2 Compute the baselines of a tex} *)

  val baselines : string -> float list

  (** {2 Simple functions for the opposite operations} *)

  val num_of_float : float -> Num.t
  (** Same as [Num.bp]. *)

  val point_of_cpoint : CPoint.t -> Point.t

  val path_of_cpath : CPath.t -> Path.t

  val transform_of_ctransform : CTransform.t -> Transform.t

  (** {2 Some options (the mlpost tool takes care of them)} *)

  (** These functions are deprecated. *)

  val set_verbosity : bool -> unit
  (** (deprecated) *)

  val set_prelude : string -> unit
  (** (deprecated) [set_prelude filename] uses the prelude of the file
        filename for compilation of the tex snippets *)

  val set_prelude2 : string option -> unit
  (** (deprecated) [set_prelude2 prelude] uses this prelude
        for compilation of the tex snippets *)

  val set_t1disasm : string option -> unit
  (** (deprecated) *)
end

(** Use the Cairo backend to draw your figures; not always available *)
module Cairost : sig
  val supported : bool

  val emit_pdf : ?msg_error:float -> string -> Command.t -> unit

  (* The optional argument set the replacement the figure by the
     text of the exception in a paragraph of width msg_error *)
  val emit_ps : string -> Command.t -> unit

  val emit_png : string -> Command.t -> unit

  val emit_svg : string -> Command.t -> unit

  val emit_pdfs : string -> Command.t list -> unit
  (** One figure by page *)

  val dump_pdf : unit -> unit

  val dump_ps : unit -> unit

  val dump_png : unit -> unit

  val dump_svg : unit -> unit

  val dump_pdfs : string -> unit
  (** Use the figures recorded by the function emit of metapost *)

  val generate_pdfs : string -> (int * Command.t) list -> unit

  type cairo_t = Cairo.context

  val emit_cairo : cairo_t -> float * float -> Command.t -> unit
end

(**/**)

module Metapost_tool : sig
  val read_prelude_from_tex_file : string -> string
end

module Version : sig
  val version : string

  (* val date : string *)
end

module Mps : sig
  val dump : unit -> unit

  val generate : (string * Command.t) list -> unit
end

module Pgf : sig
  val dump : unit -> unit

  val generate : (string * Command.t) list -> unit
end

(**/**)

module Defaults : sig
  val set_prelude : string -> unit

  val set_filename_prefix : string -> unit

  val set_required_files : string list -> unit

  val set_verbosity : bool -> unit

  val set_t1disasm : string option -> unit
end
