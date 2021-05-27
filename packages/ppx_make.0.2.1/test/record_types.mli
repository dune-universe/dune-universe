type b = { b1 : int } [@@deriving_inline make]

include sig
  [@@@ocaml.warning "-32"]

  val make_b : b1:int -> unit -> b
end
[@@ocaml.doc "@inline"]

[@@@end]

type o = { o1 : int option } [@@deriving_inline make]

include sig
  [@@@ocaml.warning "-32"]

  val make_o : ?o1:int -> unit -> o
end
[@@ocaml.doc "@inline"]

[@@@end]

type l = { l1 : int list } [@@deriving_inline make]

include sig
  [@@@ocaml.warning "-32"]

  val make_l : ?l1:int list -> unit -> l
end
[@@ocaml.doc "@inline"]

[@@@end]

type s = { s1 : string } [@@deriving_inline make]

include sig
  [@@@ocaml.warning "-32"]

  val make_s : ?s1:string -> unit -> s
end
[@@ocaml.doc "@inline"]

[@@@end]

(*type a = { a1 : int array }[@@deriving_inline make][@@@end]*)

(*type q = { q1 : int seq }[@@deriving_inline make][@@@end]*)

type d = { answer : int [@default 42] } [@@deriving_inline make]

include sig
  [@@@ocaml.warning "-32"]

  val make_d : ?answer:int -> unit -> d
end
[@@ocaml.doc "@inline"]

[@@@end]

type m = { m1 : int; [@main] m2 : int [@main] } [@@deriving_inline make]

include sig
  [@@@ocaml.warning "-32"]

  val make_m : int -> int -> m
end
[@@ocaml.doc "@inline"]

[@@@end]

type r = {
  r1 : int option;
  r2 : string; [@required]
  r3 : int option; [@required]
}
[@@deriving_inline make]

include sig
  [@@@ocaml.warning "-32"]

  val make_r : ?r1:int -> r2:string -> r3:int option -> unit -> r
end
[@@ocaml.doc "@inline"]

[@@@end]

type complex = {
  c1 : int;
  c2 : int option;
  c3 : int list;
  c4 : string;
  c5 : int; [@default 1024]
  c6 : string; [@required]
}
[@@deriving_inline make]

include sig
  [@@@ocaml.warning "-32"]

  val make_complex :
    c1:int ->
    ?c2:int ->
    ?c3:int list ->
    ?c4:string ->
    ?c5:int ->
    c6:string ->
    unit ->
    complex
end
[@@ocaml.doc "@inline"]

[@@@end]

type complex_with_main = {
  cm1 : int; [@main]
  cm2 : int option;
  cm3 : int option; [@main]
}
[@@deriving_inline make]

include sig
  [@@@ocaml.warning "-32"]

  val make_complex_with_main :
    ?cm2:int -> int -> int option -> complex_with_main
end
[@@ocaml.doc "@inline"]

[@@@end]
