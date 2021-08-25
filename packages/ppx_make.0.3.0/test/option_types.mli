type a = int option [@@deriving_inline make]

include sig
  [@@@ocaml.warning "-32"]

  val make_a : ?value:int -> unit -> a
end
[@@ocaml.doc "@inline"]

[@@@end]

type d = (int option[@default 7]) [@@deriving_inline make]

include sig
  [@@@ocaml.warning "-32"]

  val make_d : ?value:int -> unit -> d
end
[@@ocaml.doc "@inline"]

[@@@end]
