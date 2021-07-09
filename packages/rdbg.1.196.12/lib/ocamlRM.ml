(* Time-stamp: <modified the 27/08/2019 (at 16:01) by Erwan Jahier> *)

let debug = false

let plugin_tbl =  
  if debug then (Printf.printf "Creating the plugin table\n"; flush stdout);
  Hashtbl.create 1


let (reg_plugin : string -> RdbgPlugin.t -> unit) =
  fun cmxs p -> 
    if debug then (
      Printf.printf "OcamlRM.reg_plugin %s\n" cmxs;
      flush stdout 
    );
    Hashtbl.replace plugin_tbl cmxs p


let (get_plugin: string -> RdbgPlugin.t) =
  fun cmxs -> 
    if debug then (
      Printf.printf "OcamlRM.get_plugin %s\n" cmxs;
      flush stdout 
    );
    try Hashtbl.find plugin_tbl cmxs
    with Not_found ->  
      let reg_plug = Hashtbl.fold (fun cmxs _ acc -> cmxs::acc) plugin_tbl [] in
      let reg_plug = String.concat "," reg_plug in
      let msg = Printf.sprintf 
        "OcamlRM.get_plugin cannot find the rdbg plugin '%s' (registrered plugins: %s)" 
        cmxs reg_plug
      in
      failwith msg

