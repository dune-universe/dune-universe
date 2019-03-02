open! Import

module Session(IO : Io_intf.S) : Session_intf.S with module IO := IO = struct
  include Session

  (** [snmp_sess_open], [snmp_sess_close] and [snmp_sess_synch_response] are
   *  thread safe. [snmp_sess_init] is not as the first call loads the various
   *  mibs
  *)
  let snmp_sess_init () = IO.wrap Session.snmp_sess_init ()

  let snmp_sess_open ~netsnmp_session ~version ~retries ~timeout ~peername ~localname
    ~local_port ~community ~securityName ~securityAuthProto ~securityAuthPassword () =
    IO.wrap_mt (Session.snmp_sess_open
      ~netsnmp_session ~version ~retries ~timeout ~peername ~localname ~local_port
      ~community ~securityName ~securityAuthProto
      ~securityAuthPassword) ()

  let snmp_sess_close handle = IO.wrap_mt Session.snmp_sess_close handle
  let snmp_sess_synch_response handle pdu =
    IO.wrap_mt (Session.snmp_sess_synch_response handle) pdu

end

