type b = int * int [@@deriving_inline make]

include sig
  [@@@ocaml.warning "-32"]

  val make_b : v0:int -> v1:int -> unit -> b
end
[@@ocaml.doc "@inline"]

[@@@end]

type o = int option * int option [@@deriving_inline make]

include sig
  [@@@ocaml.warning "-32"]

  val make_o : ?v0:int -> ?v1:int -> unit -> o
end
[@@ocaml.doc "@inline"]

[@@@end]

type l = int list * int list [@@deriving_inline make]

include sig
  [@@@ocaml.warning "-32"]

  val make_l : ?v0:int list -> ?v1:int list -> unit -> l
end
[@@ocaml.doc "@inline"]

[@@@end]

type s = string * string [@@deriving_inline make]

include sig
  [@@@ocaml.warning "-32"]

  val make_s : ?v0:string -> ?v1:string -> unit -> s
end
[@@ocaml.doc "@inline"]

[@@@end]

type d = (int[@default 42]) * (int[@default 420]) [@@deriving_inline make]

include sig
  [@@@ocaml.warning "-32"]

  val make_d : ?v0:(int[@default 42]) -> ?v1:(int[@default 420]) -> unit -> d
end
[@@ocaml.doc "@inline"]

[@@@end]

type r = int option * (string[@required]) * (int option[@required])
[@@deriving_inline make]

include sig
  [@@@ocaml.warning "-32"]

  val make_r :
    ?v0:int -> v1:(string[@required]) -> v2:(int option[@required]) -> unit -> r
end
[@@ocaml.doc "@inline"]

[@@@end]

type complex =
  int
  * int option
  * int list
  * string
  * (int[@default 1024])
  * (string[@required])
[@@deriving_inline make]

include sig
  [@@@ocaml.warning "-32"]

  val make_complex :
    v0:int ->
    ?v1:int ->
    ?v2:int list ->
    ?v3:string ->
    ?v4:(int[@default 1024]) ->
    v5:(string[@required]) ->
    unit ->
    complex
end
[@@ocaml.doc "@inline"]

[@@@end]
