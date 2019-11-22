open Import

module type S = sig
  module IO : Io_intf.S

  val netsnmp_init_mib : unit -> unit IO.t
  val shutdown_mib : unit -> unit IO.t
  val add_mibdir : string -> int IO.t
  val read_objid : string -> Oid.t IO.t
  val get_node : string -> Oid.t IO.t
  val get_module_node : string -> string -> Oid.t IO.t
  val netsnmp_read_module : string -> unit IO.t
  val read_mib : string -> unit  IO.t
  val read_all_mibs : unit -> unit  IO.t
  val print_mib : fd:int -> unit IO.t
  val fprint_objid : fd:int -> Oid.t -> unit IO.t
  val snprint_description : Oid.t -> string IO.t
  val snprint_objid : Oid.t -> string IO.t
  val snmp_set_mib_errors : int -> unit IO.t
  val snmp_set_mib_warnings : int -> unit IO.t
  val snmp_set_save_descriptions : bool -> unit  IO.t
  val add_module_replacement : string -> string -> string -> int -> unit IO.t
end
