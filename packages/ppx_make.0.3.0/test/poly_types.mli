type 'a poly_opt = 'a option [@@deriving_inline make]

include sig
  [@@@ocaml.warning "-32"]

  val make_poly_opt : ?value:'a -> unit -> 'a poly_opt
end
[@@ocaml.doc "@inline"]

[@@@end]

type 'a poly_tuple = 'a * int [@@deriving_inline make]

include sig
  [@@@ocaml.warning "-32"]

  val make_poly_tuple : v0:'a -> v1:int -> unit -> 'a poly_tuple
end
[@@ocaml.doc "@inline"]

[@@@end]

type ('a, 'b) poly_rec = { r_a : 'a; r_b : 'b; r_int : int }
[@@deriving_inline make]

include sig
  [@@@ocaml.warning "-32"]

  val make_poly_rec : r_a:'a -> r_b:'b -> r_int:int -> unit -> ('a, 'b) poly_rec
end
[@@ocaml.doc "@inline"]

[@@@end]

type ('a, 'b) poly_var = Ok of 'a | Error of 'b | Other
[@@deriving_inline make]

include sig
  [@@@ocaml.warning "-32"]

  val make_ok_of_poly_var : v0:'a -> unit -> ('a, 'b) poly_var

  val make_error_of_poly_var : v0:'b -> unit -> ('a, 'b) poly_var

  val make_other_of_poly_var : unit -> ('a, 'b) poly_var
end
[@@ocaml.doc "@inline"]

[@@@end]
