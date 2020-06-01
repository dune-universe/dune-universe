(** Netsnmp_mib provides the low level interface to net-snmp's MIB and ASN.1
    handling functions. See the the add_mibdir(3) man page unless otherwise noted *)

(** [netsnmp_init_mib] initialises the net-snmp mib library and must be called before
    {i most} MIB functions, see per function documentation for exceptions.
    More information can be found in the net-snmp manual page: init_mib(3) *)
val netsnmp_init_mib : unit -> unit

(** [shutdown_mib] cleans up and free memory assocated with the net-snmp mib library.
    Don't call any MIB functions afterwards. See shutdown_mib(3) for more details.  *)
val shutdown_mib : unit -> unit

(** [add_mibdir] adds a directory to the list of directories to search when loading
    MIB modules.  This must be called {i after} [netsnmp_init_mib] even though the man
    page says the opposite. Raises [Netsnmp_exceptions.Not_found] if the directory
    is missing.  See add_mibdir(3) for more details *)
val add_mibdir : string -> int

(** [read_objid] raises an [Netsnmp_exceptions.Not_found] exception on failure.
    See read_objid(3) for more details *)
val read_objid : string -> Oid.t

(** [get_node] raises an [Netsnmp_exceptions.Not_found] exception on failure.
    See get_module_node(3) for more details *)
val get_node : string -> Oid.t

(** [get_module_node] raises an [Netsnmp_exceptions.Not_found] exception on failure
    See get_module_node(3) for more details *)
val get_module_node : string -> string -> Oid.t

(** The netsnmp [netsnmp_read_module] and read_mib functions do not provide reliable
    error reporting, sigh.  See netsnmp_read_module(3), read_mib(3) and read_all_mibs(3)
    for more details *)
val netsnmp_read_module : string -> unit
val read_mib : string -> unit
val read_all_mibs : unit -> unit

(** [snmp_set_mib_errors] and [snmp_set_mib_warnings] set the error and warning
    level. Note that the netsnmp libraries have a bad habit of outputing errors
    even when these are set to 0. See snmp_set_mib_errors(3) and snmp_set_mib_warnings(3)
    for more details
 *)
val snmp_set_mib_errors : int -> unit
val snmp_set_mib_warnings : int -> unit

(** [print_mib] takes a file descriptor unlike the C API, flush any output
    before calling these functions to avoid output ordering issues.
    See print_mib(3) for more details
*)
val print_mib : fd:int -> unit -> unit

(** [fprint_objid] takes a file descriptor unlike the C API.
    See fprint_objid(3) for more details *)
val fprint_objid : fd:int -> Oid.t -> unit

(** [snprint_objid] converts a Oid.t to the textual representation. See snprint_objid(3) for more details
*)
val snprint_objid : Oid.t -> string

(* [snprint_description] converts a Oid.t to the textual representation including the
   additional information from the mib entry.  See snprint_description(3) for more details *)
val snprint_description : Oid.t -> string

(** [snmp_set_save_descriptions] causes the descriptions in the MIB modules to
    be loaded as well. This {i must} be called before [netsnmp_init_mib].
    This function influences the output of the print functions as well as
    the snprint_* ones.  See snmp_set_save_descriptions(3) for more details *)
val snmp_set_save_descriptions : bool -> unit

(** [add_module_replacement].  See add_module_replacement(3) for more details *)
val add_module_replacement : string -> string -> string -> int -> unit
