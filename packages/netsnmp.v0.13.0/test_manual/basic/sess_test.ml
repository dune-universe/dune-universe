open Netsnmp_raw_monad

let die msg =
  prerr_endline msg;
  exit 1

let run hostname community =
  let version = Session.Snmp_version.Version_2c in
  let retries = 3 in
  let timeout = 3_000_000 in
  let peername = hostname in
  let localname = "" in
  let local_port = 0 in
  let community = community in
  let securityName = "" in
  let securityAuthProto = Session.Snmp_sec_auth_proto.Ignore in
  let securityAuthPassword = "" in
  let add_oid oids pdu = Mib.get_node oids |> Pdu.snmp_add_null_var pdu in
  let netsnmp_session = Session.snmp_sess_init () in
  let sess = Session.snmp_sess_open
    ~netsnmp_session ~version ~retries ~timeout ~peername ~localname ~local_port
    ~community ~securityName ~securityAuthProto ~securityAuthPassword ()
  in
  let () = Mib.netsnmp_init_mib () in
  let pdu = Pdu.snmp_pdu_create Pdu.Pdu_type.Get
    |> add_oid "sysDescr.0"
    |> add_oid "sysDescr.1"
    |> add_oid "tcpRtoAlgorithm.0"
    |> add_oid "SNMPv2-MIB::sysORID.1"
  in
    Printf.eprintf "start list%!\n";
    Session.snmp_sess_synch_response sess pdu
    |> List.iter (fun (oid, value) ->
      Printf.eprintf "snmp_sess_synch_response: %s -> [%s(%s)]%!\n"
        (Mib.snprint_objid oid)
        (Netsnmp_raw.ASN1_value.type_to_string value)
        (Netsnmp_raw.ASN1_value.to_string value));
    Printf.eprintf "end list%!\n"

let () =
  let usage_message = "Usage: session_test -c community hostname\n" in
  let hostname = ref None in
  let set_hostname s = hostname := Some s in
  let community = ref None in
  let set_community s = community := Some s in
  let specs = ["-c", Arg.String set_community, "SNMP v2c community"] in
  Arg.parse specs set_hostname usage_message;
  match !hostname, !community with
  | Some hostname, Some community -> run hostname community
  | _ -> die ("invalid args\n" ^ usage_message ^ "\n")
