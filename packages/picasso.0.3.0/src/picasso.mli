(** Picasso is an Abstract element drawing library. It handles most of the
    boilerplate you usually write to draw abstract elements and allows you to
    view those. *)

open Apron

(** Colors handling *)
module Colors : sig
  (** Type of colors, using rgb format *)
  type t = int * int * int

  val rgb : int -> int -> int -> t
  (** Constructor *)

  val red : t
  (** {1 Predefined colors} *)

  val green : t

  val blue : t

  val white : t

  val black : t
end

(** Module of drawable abstractions *)
module Drawable : sig
  (** {1 Types} *)

  (** The type of drawable abstractions *)
  type t

  (** Drawing dimensions are given string identifiers *)
  type var = string

  (** Multi-dimensional points *)
  type point = float list

  (** Float ranges *)
  type range = float * float

  (** {1 Constructors} *)

  val of_box : Box.t Abstract1.t -> t
  (** Drawable from an element of the Boxes abstract domain *)

  val of_oct : Oct.t Abstract1.t -> t
  (** Drawable from an element of the Octagon abstract domain *)

  val of_pol : Polka.strict Polka.t Abstract1.t -> t
  (** Drawable from an element of the Polyhedra abstract domain *)

  val of_gens : Generator1.t list -> t
  (** Builds the drawable space corresponding to the polyhedra defined by the
      list of generators. *)

  val of_lcons : Lincons1.t list -> t
  (** Builds the drawable space corresponding to the polyhedra defined by the
      conjunction of a list of constraints. *)

  val of_hull : string list -> point list -> t
  (** Same as of_gens, but build a convex hull from a list of a variables
      defining an environment and a list of points.

      @raise Invalid_arg if one points or more do not have as many dimension
      as the number of variables*)

  val of_ranges : string list -> range list -> t
  (** Builds a drawable hypercube from a list of variable and a list of
      ranges.

      @raise Invalid_arg if the range list and the variable list do not have
      the same length *)

  (** {1 Operations} *)

  val union : t -> t -> t
  (** Merges two drawable into one drawable, where the elements will be drawn
      one after the other *)

  val product : t -> t -> t
  (** Merges two drawable into one drawable, where only the intersection of
      the elements will be drawn. *)
end

(** Module for 2d drawing of abstract elements, handles the 'camera'
    settings, 2D projection, and some graphical options *)
module Rendering : sig
  (** Type of 2D scenes *)
  type t

  val create :
       ?title:string
    -> ?padding:float
    -> ?grid:bool
    -> ?axis:bool
    -> abciss:string
    -> ordinate:string
    -> float
    -> float
    -> t
  (** Initalizes an empty 2d scenes. *)

  val add : ?autofit:bool -> t -> Colors.t * Drawable.t -> t
  (** Registers an abstract element, associated to a color, into a scene.
      Automatically changes the camera settings to encompass the newly added
      abstract element. You can cancel this behaviour by settings the
      optional argument [autofit] to [false]*)

  val add_l : ?autofit:bool -> t -> (Colors.t * Drawable.t) list -> t
  (** Registers a list of element into a scene. [add_l r l] is equivalent to
      calling [add] successively on the elements of [l] *)

  (** {1 Camera settings} *)

  val translate : float * float -> t -> t
  (** translation of the whole scene *)

  val scale : t -> float -> t
  (** zoom / unzoom *)
end

(** Module for 3D model generation of abstract elements *)
module Rendering3d : sig
  (** Type of 3D scenes *)
  type t

  val create : abciss:string -> ordinate:string -> height:string -> unit -> t
  (** Initalizes an empty 3D scenes. *)

  val add : t -> Colors.t * Drawable.t -> t
  (** Registers an abstract element into a scene *)

  val add_l : t -> (Colors.t * Drawable.t) list -> t
  (** Registers a list of element into a scene. [add_l r l] is equivalent to
      calling [add] successively on the elements of [l] *)
end

(** raised by rendering function when a backend is not installed or when an
    internal error occurs *)
exception BackendError of string

(** {1 Drawing utilities} *)

val show : Rendering.t -> unit
(** Main drawing function. Displays a Rendering.t using one of the backend
    available. It first tries with gtk, and if lablgtk is not installed,
    retries using graphics. If none of the backend is installed, outputs a
    .svg file in the current directory, named after the title of the window
    if specified, otherwise name "picasso[number].svg" *)

val to_latex : ?tikz_only:bool -> Rendering.t -> string -> unit
(** Outputs a tex file with a tikz figure corresponding to a Rendering.t. If
    the tikz_only option is set to false (default is true), it outputs the
    full tex document and not only the tikz figure *)

val to_svg : Rendering.t -> string -> unit
(** Outputs a svg file with a figure corresponding to a Rendering.t *)

val to_obj : Rendering3d.t -> string -> unit
(** Builds an obj file corresponding to a Rendering3D context *)

(** {2 Backend specific drawing functions}*)

val in_gtk_canvas : Rendering.t -> unit
(** Displays a Rendering.t within a scrollable, zoomable gtk canvas.

    @raise BackendError if the lablgtk library is not installed *)

val in_graphics_canvas : Rendering.t -> unit
(** Displays a Rendering.t within a graphics window. The window created can
    be exited cleanly by pressing any key.

    @raise BackendError if the graphics library is not installed *)
