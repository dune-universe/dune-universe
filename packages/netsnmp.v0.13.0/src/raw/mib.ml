(** Netsnmp_mib provides the low level interface to net-snmp's MIB and ASN.1
    handling functions *)

(* These map to the corresponding C API functions *)

external netsnmp_init_mib : unit -> unit = "caml_netsnmp_init_mib"
external shutdown_mib : unit -> unit = "caml_shutdown_mib"
external add_mibdir_c : string -> int = "caml_add_mibdir"
external read_objid : string -> Oid.t = "caml_read_objid"
external get_node : string -> Oid.t = "caml_get_node"

external get_module_node : string -> string -> Oid.t = "caml_get_module_node"
external netsnmp_read_module : string -> unit = "caml_netsnmp_read_module"
external read_mib : string -> unit = "caml_read_mib"
external read_all_mibs : unit -> unit = "caml_read_all_mibs"
external snmp_set_mib_errors : int -> unit = "caml_snmp_set_mib_errors"
external snmp_set_mib_warnings : int -> unit = "caml_snmp_set_mib_warnings"
external print_mib_c : fd:int -> unit = "caml_print_mib"
external fprint_objid : fd:int -> Oid.t -> unit = "caml_fprint_objid"
external snprint_description : Oid.t -> string = "caml_snprint_description"
external snprint_objid : Oid.t -> string = "caml_snprint_objid"
external snmp_set_save_descriptions : bool -> unit = "caml_snmp_set_save_descriptions"
external add_module_replacement : string -> string -> string -> int -> unit = "caml_add_module_replacement"

let print_mib ~fd () = print_mib_c ~fd

let add_mibdir dir =
  let count = add_mibdir_c dir in
  if count < 0 then raise (Netsnmp_exceptions.Not_found dir)
  else count
