open Netsnmp_raw_monad

let die msg =
  prerr_endline msg;
  exit 1

let add_mib_paths paths =
  paths |> List.iter (fun path ->
    let num = Mib.add_mibdir path in
    Printf.eprintf "mibs(%s) = %d\n%!" path num)
;;

let oid_module oidstr =
  let re = Re.(compile (rep1 (char ':'))) in
  match Re.split re oidstr with
    | oidm::_::_ -> Some oidm
    | _ -> None
;;

let get_oid f oidstr errmsg =
  try Some (f oidstr) with
  | Netsnmp_exceptions.Not_found err -> Printf.eprintf "%s failed (%s): %s\n%!" errmsg oidstr err;
    None

let display_oid oidstr =
  Printf.eprintf "\n--- OID: %s ---\n" oidstr;
  let oid =
    match oid_module oidstr with
    | Some oidm -> begin
      try
        Mib.netsnmp_read_module oidm;
        get_oid Mib.read_objid oidstr "read_objid"
      with
        | Netsnmp_exceptions.Not_found err -> Printf.eprintf "Module not found: %s (%s)%!" oidm err; None
      end
    | None -> get_oid Mib.get_node oidstr "get_node"
  in
  match oid with
  | Some oid ->
    Printf.eprintf "%s objid_len = %d\n" oidstr (Netsnmp_raw.Oid.length oid);
    Printf.eprintf "fprint_objid:%!";
    Mib.fprint_objid ~fd:2 oid;
    let str = Mib.snprint_objid oid in
      Printf.eprintf "snprint_objid:%s\n%!" str;
    let str = Mib.snprint_description oid in
      Printf.eprintf "snprint_description:\n%s\n%!" str
  | None ->
    Printf.eprintf "%s not found\n%!" oidstr
;;

let () =
  let usage_message = "Usage: mib_test [-save-descr] [-mib-path PATH] [oid...]" in
  let save_descr = ref false in
  let mib_paths = ref [] in
  let set_mib_paths s = mib_paths := !mib_paths @ [s] in
  let oids = ref [ "sysDescr.0"; "SNMPv2-MIB::sysDescr.0" ] in
  let set_oids oid = oids := !oids @ [oid] in
  let specs = [
      "-save-descr", Arg.Set save_descr, "set mib save_description "
    ; "-mib-path", Arg.String set_mib_paths, "directory to search for mibs"
    ]
  in
  Arg.parse specs set_oids usage_message;
  if !save_descr then Mib.snmp_set_save_descriptions true;
  Mib.netsnmp_init_mib ();
  add_mib_paths !mib_paths;
  List.iter display_oid !oids;
