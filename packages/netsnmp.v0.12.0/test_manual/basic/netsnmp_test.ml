open Netsnmp

let die msg =
  prerr_endline msg;
  exit 1

let run hostname community =
  let auth = { Netsnmp.Snmp_v1_2c_auth_data.community = community } in
  let version_auth = Netsnmp.Snmp_version_auth.Version_2c auth in
  let cinfo =
    {
      Netsnmp.Connection_info.version_auth
    ; peername = hostname
    ; localname = None
    ; local_port = None
    ; retries = None
    ; timeout = None
  }
  in
  let () = Netsnmp.add_mib_paths ["."] in
  let conn = Netsnmp.Connection.connect cinfo in
  let result = Netsnmp.get_s conn
    ["sysDescr.0"; "sysDescr.1"; "tcpRtoAlgorithm.0"; "SNMPv2-MIB::sysORID.1"]
  in
    Printf.eprintf "start list%!\n";
    result
    |> List.iter (fun (oid, value) ->
      Printf.eprintf "snmp_sess_synch_response: %s -> [%s(%s)]%!\n"
        (Netsnmp.Mib.snprint_objid oid)
        (Netsnmp.ASN1_value.type_to_string value)
        (Netsnmp.ASN1_value.to_string value));
    Printf.eprintf "end list%!\n";
    Netsnmp.Connection.close conn

let () =
  let usage_message = "Usage: netsnmp_test -c community hostname\n" in
  let hostname = ref None in
  let set_hostname s = hostname := Some s in
  let community = ref None in
  let set_community s = community := Some s in
  let specs = ["-c", Arg.String set_community, "SNMP v2c community"] in
  Arg.parse specs set_hostname usage_message;
  match !hostname, !community with
  | Some hostname, Some community -> run hostname community
  | _ -> die ("invalid args\n" ^ usage_message ^ "\n")
