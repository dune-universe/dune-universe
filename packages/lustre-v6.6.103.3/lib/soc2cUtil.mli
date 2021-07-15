(* Time-stamp: <modified the 13/09/2016 (at 16:42) by Erwan Jahier> *)

(** *)


val type_to_string : Data.t -> string -> string
val data_type_to_c:  Data.t -> string -> string
val lic_type_to_c: Lic.type_  -> string -> string

val string_of_flow_decl : string * Data.t -> string
val string_of_flow_decl_w7annot : Soc.gao list -> string * Data.t -> string

type var_kind = (* XXX poor names: fixme! *)
  | ML_IO of Soc.key (* for idents that are part of a MemoryLess soc interface *)
  | M_IO (* for idents that are part of a soc interface with Memories*)
  | Local (* for soc local variables *)


(**  Dependending on Lv6MainArgs.global.soc2_no_switch 
    [gen_c_switch "c" ["case1","stmt1" ; "case2","stmt2" ; ... ]
returns

    switch(c) {
      case case1: stmt1 ; break;
      case case2: stmt2 ; break;
      ...
    }

or

    if (c == case1) { stmt1 } else {
    if (c == case2) { stmt2 } else {
    ...
    nop;
    }....}

*)
val gen_c_switch : string -> (string * string) list -> string
