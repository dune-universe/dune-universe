(** Picasso is an Abstract element drawing library. It handles most of
   the boilerplate you usually write to draw abstract elements and
   allows you to view those.  *)

open Apron

(** Colors handling *)
module Colors : sig

  (** Type of colors, using rgb format *)
  type t = int * int * int

  (** Constructor *)
  val rgb : int -> int -> int -> t

  (** {1 Predefined colors} *)
  val red : t
  val green : t
  val blue : t
  val white : t
  val black : t
end

(** Module of drawable abstractions *)
module Drawable : sig

  (** {1 Types} *)

  (** The type of drawable abstractions  *)
  type t

  (** Drawing dimensions are given string identifiers *)
  type var = string

  (** Multi-dimensional points *)
  type point = float list

  (** Float ranges *)
  type range = float * float

  (** {1 Constructors} *)

  (** Drawable from an element of the Boxes abstract domain *)
  val of_box : Box.t Abstract1.t -> t

  (** Drawable from an element of the Octagon abstract domain *)
  val of_oct : Oct.t Abstract1.t -> t

  (** Drawable from an element of the Polyhedra abstract domain *)
  val of_pol : Polka.strict Polka.t Abstract1.t -> t

  (** Builds the drawable space corresponding to the polyhedra defined
   by the list of generators. *)
  val of_gens : Generator1.t list -> t

  (** Builds the drawable space corresponding to the polyhedra defined
   by the conjunction of a list of constraints. *)
  val of_lcons : Lincons1.t list -> t

  (** Same as of_gens, but build a convex hull from a list of a
     variables defining an environment and a list of points.
     @raise Invalid_arg if one points or more do not have as many dimension as
     the number of variables*)
  val of_hull : string list -> point list -> t

  (** Builds a drawable hypercube from a list of variable and a list
     of ranges.
     @raise Invalid_arg if the range list and the variable list do not have
     the same length *)
  val of_ranges : string list -> range list -> t

  (** {1 Operations } *)

  (** Merges two drawable into one drawable, where the elements will
     be drawn one after the other *)
  val union : t -> t -> t

  (** Merges two drawable into one drawable, where only the
     intersection of the elements will be drawn. *)
  val product : t -> t -> t
end

(** Module for 2d drawing of abstract elements, handles the 'camera'
   settings, 2D projection, and some graphical options *)
module Rendering : sig
  (** Type of 2D scenes *)
  type t

  (** Initalizes an empty 2d scenes. *)
  val create: ?title:string -> ?padding:float -> ?grid:bool -> ?axis:bool ->
              abciss:string -> ordinate:string -> float -> float -> t

  (** Registers an abstract element, associated to a color, into a
     scene. Automatically changes the camera settings to encompass the
     newly added abstract element. You can cancel this behaviour by
     settings the optional argument [autofit] to [false]*)
  val add : ?autofit:bool -> t -> Colors.t * Drawable.t -> t

  (** Camera settings *)
  val translate : float * float -> t -> t
  val scale : t -> float -> t
end

(** Module for 3D model generation of abstract elements *)
module Rendering3d : sig
   (** Type of 3D scenes *)
  type t

  (** Initalizes an empty 3D scenes. *)
  val create : abciss:string -> ordinate:string -> height:string -> unit -> t

  (** Registers an abstract element into a scene *)
  val add : t -> Drawable.t -> t
end

(** {1 Drawing utilities} *)

(** Displays a Rendering.t within a scrollable, zoomable gtk canvas *)
val in_gtk_canvas : Rendering.t -> unit

(** Outputs a tex file with a tikz figure corresponding to a
   Rendering.t. If the tikz_only option is set to false (default is
   true), it outputs the full tex document and not only the tikz
   figure *)
val to_latex : ?tikz_only:bool -> Rendering.t -> string -> unit

(** Builds an obj file rorresponding to a Rendering3D context *)
val to_obj : Rendering3d.t -> string -> unit
