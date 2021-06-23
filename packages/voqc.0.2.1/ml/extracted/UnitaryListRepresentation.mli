open Datatypes

type 'u gate_app =
| App1 of 'u * int
| App2 of 'u * int * int
| App3 of 'u * int * int * int

type 'u gate_list = 'u gate_app list

val uc_well_typed_l_b : int -> 'a1 gate_list -> bool

val next_single_qubit_gate :
  'a1 gate_list -> int -> (('a1 gate_list * 'a1) * 'a1 gate_list) option

val last_single_qubit_gate :
  'a1 gate_list -> int -> (('a1 gate_list * 'a1) * 'a1 gate_list) option

val next_two_qubit_gate :
  'a1 gate_list -> int -> (((('a1 gate_list * 'a1) * int) * int) * 'a1
  gate_list) option

val next_gate :
  'a1 gate_list -> (int -> bool) -> (('a1 gate_list * 'a1 gate_app) * 'a1
  gate_list) option

val does_not_reference_appl : int -> 'a1 gate_app -> bool

val does_not_reference : 'a1 gate_list -> int -> bool

val try_rewrites :
  'a1 gate_list -> ('a1 gate_list -> 'a1 gate_list option) list -> 'a1
  gate_list option

val try_rewrites2 :
  'a1 gate_list -> ('a1 gate_list -> ('a1 gate_list * 'a1 gate_list) option)
  list -> ('a1 gate_list * 'a1 gate_list) option

val propagate :
  'a1 gate_list -> ('a1 gate_list -> ('a1 gate_list * 'a1 gate_list) option)
  list -> ('a1 gate_list -> 'a1 gate_list option) list -> int -> 'a1 gate_app
  list option

val remove_prefix :
  'a1 gate_list -> 'a1 gate_list -> (int -> 'a1 -> 'a1 -> bool) -> 'a1
  gate_list option

val remove_suffix :
  'a1 gate_list -> 'a1 gate_list -> (int -> 'a1 -> 'a1 -> bool) -> 'a1
  gate_app list option

val replace_pattern :
  'a1 gate_list -> 'a1 gate_list -> 'a1 gate_list -> (int -> 'a1 -> 'a1 ->
  bool) -> 'a1 gate_list option

val get_matching_prefix :
  'a1 gate_list -> 'a1 gate_list -> (int -> 'a1 -> 'a1 -> bool) -> ('a1
  gate_app list * 'a1 gate_app list) * 'a1 gate_list

val coq_LCR :
  'a1 gate_list -> ('a1 gate_list -> 'a1 gate_list) -> (int -> 'a1 -> 'a1 ->
  bool) -> (('a1 gate_app list * 'a1 gate_app list) * 'a1 gate_list) option
