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

let _ = fun (_ : tuple_v) -> ()

let (make_t_baisc_of_tuple_v : v0:int -> unit -> tuple_v) =
 fun ~v0 _ -> T_baisc v0

let _ = make_t_baisc_of_tuple_v

let (make_t_option_of_tuple_v : ?v0:int -> unit -> tuple_v) =
 fun ?v0 _ -> T_option v0

let _ = make_t_option_of_tuple_v

let (make_t_complex_of_tuple_v :
      v0:int ->
      ?v1:int ->
      ?v2:int list ->
      ?v3:string ->
      ?v4:(int[@default 1024]) ->
      v5:(string[@required]) ->
      unit ->
      tuple_v) =
 fun ~v0 ?v1 ?(v2 = []) ?(v3 = "") ?(v4 = 1024) ~v5 _ ->
  T_complex (v0, v1, v2, v3, v4, v5)

let _ = make_t_complex_of_tuple_v

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
[@@deriving make]
