val initialize : strict:bool -> dims:string list -> maxrows:int -> unit
  (* [initialize strict lnames maxrows] initializes the library and
     sets up the correspondance between variables names and ranks.
     The order of variables in vectors is given by the order in the
     parameter list *)

val nbdims : int ref
  (* Number of variables in database. All the objects created by this
     module will be of this dimension. *)

val in_assoc : string -> int
val out_assoc : int -> string
  (* Association between variable names and ranks *)

val vector_of_constraint : string -> Vector.t
val vector_of_frame : string -> Vector.t
val vector_of_expr : string -> Vector.t
  (* Conversion from strings to vectors *)

val matrix_of_lconstraints : string list -> Matrix.t
val matrix_of_lframes : string list -> Matrix.t
  (* Conversion from list of strings to matrices *)

val poly_of_lconstraint : string list -> Poly.t
val poly_of_lframe : string list -> Poly.t
  (* Conversion from list of strings to polyhedra *)

val vector_print_constraint : Format.formatter -> Vector.t -> unit
val vector_print_frame : Format.formatter -> Vector.t -> unit
val vector_print_expr : Format.formatter -> Vector.t -> unit
val matrix_print_constraints : Format.formatter -> Matrix.t -> unit
val matrix_print_frames : Format.formatter -> Matrix.t -> unit
val poly_print_constraint : Format.formatter -> Poly.t -> unit
val poly_print_frame : Format.formatter -> Poly.t -> unit
val poly_print : Format.formatter -> Poly.t -> unit
  (* Printing functions *)
