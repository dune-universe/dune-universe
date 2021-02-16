type point = Ctypes.point

type abscissa = float

type t
(** The type of Splines *)

val inter_depth : int ref
(** A mesure to decide how many iterations do to in intersection computations;
 * higher means more precise *)

val debug : bool

val print : Format.formatter -> t -> unit

val create : point -> point -> point -> point -> t
(** [create a b c d] creates a spline with points a and d and
    control points b and c. By default, the abscissa of the spline
    starts at [0.] and ends at [1.].

*)

val create_with_offset : float -> point -> point -> point -> point -> t
(** create a spline with abscissa between [ [f,f+1] ] *)

val explode : t -> point * point * point * point
(** return the four points of the spline; left point, left control
    point, second point, second control point*)

val left_point : t -> point

val left_control_point : t -> point

val right_point : t -> point

val right_control_point : t -> point
(** the four points of a spline *)

val reverse : (float -> float) -> t -> t
(** reverse a spline, using a conversion function for max and min *)

val point_of : t -> abscissa -> point
(** compute the location of the given abscissa on a spline *)

val point_of_s : t -> abscissa -> point
(** compute the location of the given abscissa on a spline, but
    convert abscissa to [0,1] interval first *)

val direction : t -> abscissa -> point
(** give the direction (derivative) of the spline at the given abscissa *)

val bounding_box : t -> float * float * float * float
(** a bounding_box of the given spline *)

val precise_bounding_box : t -> float * float * float * float
(** a more precise bounding_box of the given spline *)

val one_intersection : t -> t -> float * float
(** compute a single intersection of the two splines; raise
    [Not_found] if there is no intersection. *)

val intersection : t -> t -> (float * float) list
(** compute all intersections of the two splines; raise [Not_found] if there
    * is no intersection. *)

val apply4 : (point -> point -> point -> point -> 'a) -> t -> 'a
(** apply a function to the four points of the spline *)

type split =
  | Min
  | Max
  | InBetween of t * t
      (** the type which caracterizes a split of a spline -
     Min - we have splitted at the left end
     Max - we have splitted at the right end
     InBetween (s1,s2) - we have splitted somewhere in between, and the
     resulting two new splines are [s1] and [s2]
   *)

val split : t -> abscissa -> split
(** split a spline at the given abscissa *)

val dist_min_point : point -> t -> float * float
(** [dist_min_point p s] computes the minimal distance of [p] to [s],
    as well as the abscissa which corresponds to this minimal
    distance; the return value is [distance, abscissa].
*)

val dist_min_spline : t -> t -> float * (float * float)
(** [dist_min_path p1 p2] computes the minimal distance of [p1] to
    [p2], as well as the two abscissa which correspond to this minimal
    distance; the return value is [distance, (abscissa_on_p1,
    abscissa_on_p2)].
*)

val translate : point -> t -> t
(** translate all points of the spline *)

val transform : Matrix.t -> t -> t
(** transform all points of the spline *)
