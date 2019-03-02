open Netsnmp_raw_monad

let die msg =
  prerr_endline msg;
  exit 1

let add_var oids pdu =
  let oid = Mib.read_objid oids in
    Pdu.snmp_add_null_var pdu oid


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
  let netsnmp_session = Session.snmp_sess_init () in
  let sess = Session.snmp_sess_open
    ~netsnmp_session ~version ~retries ~timeout ~peername ~localname ~local_port
    ~community ~securityName ~securityAuthProto ~securityAuthPassword ()
  in
  let pdu = Pdu.snmp_pdu_create Pdu.Pdu_type.Get in
  let () = Mib.netsnmp_init_mib () in
  let oid = Mib.get_node "sysDescr.0" in
  let pdu =
    Pdu.snmp_add_null_var pdu oid
    |> add_var ".1.3.6.1.4.1.8072.2.99.1.0"
    |> add_var ".1.3.6.1.4.1.8072.2.99.2.0"
    |> add_var ".1.3.6.1.4.1.8072.2.99.3.0"
    |> add_var ".1.3.6.1.4.1.8072.2.99.4.0"
    |> add_var ".1.3.6.1.4.1.8072.2.99.5.0"
    |> add_var ".1.3.6.1.4.1.8072.2.99.6.0"
    |> add_var ".1.3.6.1.4.1.8072.2.99.7.0"
    |> add_var ".1.3.6.1.4.1.8072.2.99.8.0"
    |> add_var ".1.3.6.1.4.1.8072.2.99.9.0"
    |> add_var ".1.3.6.1.4.1.8072.2.99.10.0"
    |> add_var ".1.3.6.1.4.1.8072.2.99.11.0"
  (* These are no longer supported on the client side
    |> add_var ".1.3.6.1.4.1.8072.2.99.12.0"
    |> add_var ".1.3.6.1.4.1.8072.2.99.13.0"
  *)
    |> add_var ".1.3.6.1.4.1.8072.2.99.14.0"
    |> add_var ".1.3.6.1.4.1.8072.2.99.15.0"
    |> add_var ".1.3.6.1.4.1.8072.2.99.16.0"
  in
    Printf.eprintf "start list%!\n";
    Printf.eprintf "sysDescr.0 objid_len = %d\n%!" (Netsnmp_raw.Oid.length oid);
    Session.snmp_sess_synch_response sess pdu
    |> List.iter (fun (oid, value) ->
      Printf.eprintf "snmp_sess_synch_response: %s -> [%s(%s)]%!\n"
        (Mib.snprint_objid oid)
        (Netsnmp_raw.ASN1_value.type_to_string value)
        (Netsnmp_raw.ASN1_value.to_string value));
    Printf.eprintf "end list%!\n";
  let pdu = Pdu.snmp_pdu_create Pdu.Pdu_type.Get in
  let _:Pdu.t = add_var ".1.3.6.1.4.1.8072.2.99.4.0" pdu in
    Session.snmp_sess_synch_response sess pdu
    |> List.iter (fun (_oid, value) -> match value with
      | Netsnmp_raw.ASN1_value.ASN_Counter64 count ->
        Printf.eprintf "Counter64: %08x,%08x\n" count.high count.low
      | _ ->
        Printf.eprintf "Unexpected type: %s\n"
         (Netsnmp_raw.ASN1_value.type_to_string value))

let () =
  let usage_message = "Usage: var_types -c community hostname\n" in
  let hostname = ref None in
  let set_hostname s = hostname := Some s in
  let community = ref None in
  let set_community s = community := Some s in
  let specs = ["-c", Arg.String set_community, "SNMP v2c community"] in
  Arg.parse specs set_hostname usage_message;
  match !hostname, !community with
  | Some hostname, Some community -> run hostname community
  | _ -> die ("invalid args\n" ^ usage_message ^ "\n")
