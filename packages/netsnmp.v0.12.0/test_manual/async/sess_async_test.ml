open! Core
open Async
open Netsnmp_async

let run hostname community =
  let version = Raw.Session.Snmp_version.Version_2c in
  let retries = 3 in
  let timeout = 3_000_000 in
  let peername = hostname in
  let localname = "" in
  let local_port = 0 in
  let community = community in
  let securityName = "" in
  let securityAuthProto = Raw.Session.Snmp_sec_auth_proto.Ignore in
  let securityAuthPassword = "" in
  let add_oid oids pdu = Mib.get_node oids >>= Raw.Pdu.snmp_add_null_var pdu in
  Mib.netsnmp_init_mib ()
  >>= fun () ->
  Raw.Session.snmp_sess_init ()
  >>= fun netsnmp_session ->
  Raw.Session.snmp_sess_open
    ~netsnmp_session ~version ~retries ~timeout ~peername ~localname ~local_port
    ~community ~securityName ~securityAuthProto ~securityAuthPassword ()
  >>= fun sess ->
  Raw.Pdu.snmp_pdu_create Raw.Pdu.Pdu_type.Get
    >>= add_oid "sysDescr.0"
    >>= add_oid "sysDescr.1"
    >>= add_oid "tcpRtoAlgorithm.0"
    >>= add_oid "SNMPv2-MIB::sysORID.1"
  >>= fun pdu ->
    printf "start list%!\n";
    Raw.Session.snmp_sess_synch_response sess pdu
    >>= Deferred.List.iter ~f:(fun (oid, value) ->
      Mib.snprint_objid oid
      >>= fun oid_s ->
      printf "snmp_sess_synch_response: %s -> [%s(%s)]%!\n"
        oid_s
        (Netsnmp_raw.ASN1_value.type_to_string value)
        (Netsnmp_raw.ASN1_value.to_string value);
         Deferred.unit)
    >>| fun () -> printf "end list%!\n"

let () =
  Command.async_spec
    ~summary:"session access"
    Command.Spec.(
      empty
      +> flag "-c" (required string) ~doc:"SNMP v2c community"
      +> anon ("hostname" %: string)
    )
    (fun community hostname () -> run hostname community)
  |> Command.run
