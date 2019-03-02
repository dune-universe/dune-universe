module Mib(IO : Io_intf.S) : Mib_intf.S with module IO := IO = struct

  module Mib = Netsnmp_raw.Mib

  let netsnmp_init_mib () = IO.wrap Mib.netsnmp_init_mib ()
  let shutdown_mib () = IO.wrap Mib.shutdown_mib ()
  let add_mibdir dir = IO.wrap Mib.add_mibdir dir
  let read_objid oid_s = IO.wrap Mib.read_objid oid_s
  let get_node oid_s = IO.wrap Mib.get_node oid_s
  let get_module_node oid_s module_s = IO.wrap (Mib.get_module_node oid_s) module_s
  let netsnmp_read_module module_s = IO.wrap Mib.netsnmp_read_module module_s
  let read_mib file = IO.wrap Mib.read_mib file
  let read_all_mibs () = IO.wrap Mib.read_all_mibs ()
  let print_mib ~fd = IO.wrap (Mib.print_mib ~fd) ()
  let fprint_objid ~fd oid = IO.wrap (Mib.fprint_objid ~fd) oid
  let snprint_description oid = IO.wrap Mib.snprint_description oid
  let snprint_objid oid = IO.wrap Mib.snprint_objid oid
  let snmp_set_mib_errors level = IO.wrap Mib.snmp_set_mib_errors level
  let snmp_set_mib_warnings level = IO.wrap Mib.snmp_set_mib_warnings level
  let snmp_set_save_descriptions v = IO.wrap Mib.snmp_set_save_descriptions v
  let add_module_replacement omod nmod tag len =
    IO.wrap (Mib.add_module_replacement omod nmod tag) len

end
