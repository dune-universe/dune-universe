type tuple_v =
  | T_baisc of int
  | T_option of int option
  | T_complex of
      int
      * int option
      * int list
      * string
      * (int[@default 1024])
      * (string[@required])
[@@deriving_inline make]

include sig
  [@@@ocaml.warning "-32"]

  val make_t_baisc_of_tuple_v : v0:int -> unit -> tuple_v

  val make_t_option_of_tuple_v : ?v0:int -> unit -> tuple_v

  val make_t_complex_of_tuple_v :
    v0:int ->
    ?v1:int ->
    ?v2:int list ->
    ?v3:string ->
    ?v4:(int[@default 1024]) ->
    v5:(string[@required]) ->
    unit ->
    tuple_v
end
[@@ocaml.doc "@inline"]

[@@@end]

type record_v =
  | R_basic of { b1 : int }
  | R_complex of {
      c1 : int;
      c2 : int option;
      c3 : int list;
      c4 : string;
      c5 : int; [@default 1024]
      c6 : string; [@required]
    }
  | R_complex_with_main of {
      cm1 : int; [@main]
      cm2 : int option;
      cm3 : int option; [@main]
    }
[@@deriving_inline make]

include sig
  [@@@ocaml.warning "-32"]

  val make_r_basic_of_record_v : b1:int -> unit -> record_v

  val make_r_complex_of_record_v :
    c1:int ->
    ?c2:int ->
    ?c3:int list ->
    ?c4:string ->
    ?c5:int ->
    c6:string ->
    unit ->
    record_v

  val make_r_complex_with_main_of_record_v :
    ?cm2:int -> int -> int option -> record_v
end
[@@ocaml.doc "@inline"]

[@@@end]
