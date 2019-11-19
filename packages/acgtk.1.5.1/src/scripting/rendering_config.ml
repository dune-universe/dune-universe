open UtilsLib
open AcgData


type engine = STRINGS | LOGIC | DERIVED_TREES | TREES | DEFAULT

let get_engine s =
  match String.lowercase_ascii s with
  | "strings" -> STRINGS
  | "logic" -> LOGIC
  | "unranked trees" -> DERIVED_TREES
  | "trees" ->  TREES
  | _ -> DEFAULT

    
type config = {bg:int * int * int;node:int*int*int;engines:engine Utils.StringMap.t}

let default_map =
  List.fold_left
    (fun acc (lst,engine) ->
      List.fold_left
	(fun acc n -> Utils.StringMap.add n engine acc)
	acc
	lst)
    Utils.StringMap.empty
    [
      (["Strings";"strings";"anglais";"francais"],STRINGS);
      (["labelled_logic";"logic";"logique";"HybridLogic";"semantics"],LOGIC);
      (["Trees";"Derived_trees";"trees";"derived_trees"],DERIVED_TREES);
      (["discourse_grammar";"Derivations";"derivations";"Derivation_trees";"derivation_trees";"TAG";"DSTAG"],TREES);
    ]

    
let default = {bg=(255,255,255);
	       node=(239,239,239);
	       engines=default_map}

            
let get_color key colors default_col =
  match Yojson.Basic.Util.([colors] |> filter_member key |> flatten)  with
  | (`Int r)::(`Int g)::(`Int b)::_ -> (r,g,b)
  | _ -> default_col
            
let get_config filename includes =
  try
    let fullname = Utils.find_file filename includes in
    let json_val = Yojson.Safe.(to_basic (from_channel ~fname:fullname (open_in fullname))) in
    (try 
      let conf = Yojson.Basic.Util.to_assoc json_val in
      let signatures = List.assoc_opt "signatures" conf in
      let engines =
        match signatures with
        | None ->
           let () = Warnings.(issue_warning (Config (Missing_key (fullname,[],"signatures")))) in
           let () = Warnings.(issue_warning (Config Default_engines)) in
           default_map
        | Some signatures ->
           List.fold_left
             (fun acc json ->
               try
                 let _json_acc = Yojson.Basic.Util.member "name" json in
                 let _json_acc = Yojson.Basic.Util.member "engine" json in
                 let sig_name =
                   try Yojson.Basic.Util.(to_string_option (member "name" json)) with
                   | Yojson.Basic.Util.Type_error (s,j) ->
                      let () = Warnings.(issue_warning (Config (Bad_group (fullname,["signatures";"name"],s,j,"A json object string was expected","Skipping this signature name")))) in
                      None in
                 let sig_engine =
                   try Yojson.Basic.Util.(to_string_option (member "engine" json)) with
                   | Yojson.Basic.Util.Type_error (s,j) ->
                      let () = Warnings.(issue_warning (Config (Bad_group (fullname,["signatures";"engine"],s,j,"A json object string was expected","Skipping this engine")))) in
                      None in
                 match sig_name,sig_engine with
                 | Some n, Some e ->  Utils.StringMap.add n (get_engine e) acc
                 | None, Some e ->
                    let () = Warnings.(issue_warning
                                         (Config (Missing_name (fullname,["signatures"],"name",e)))) in
                    acc
                 | Some n, None ->
                    let () = Warnings.(issue_warning
                                         (Config (Missing_engine (fullname,["signatures"],"engine",n)))) in
                    acc
                 | _,_ -> acc
               with
               | Yojson.Basic.Util.Type_error (s,j) ->
                  let () = Warnings.(issue_warning (Config (Bad_group (fullname,["signatures"],s,j,"A json object with fields \"name\" and \"engine\" was expected","Skipping this signature name/engine association")))) in
                  acc)
             Utils.StringMap.empty
             (Yojson.Basic.Util.to_list signatures) in
      let colors = List.assoc_opt "colors" conf in
      let bg,node_color =
        match colors with
        | None ->
           let () = Warnings.(issue_warning (Config (Missing_key (fullname,[],"colors")))) in
           let () = Warnings.(issue_warning (Config Default_colors)) in
           default.bg,default.node
        | Some colors ->
           let bg = get_color "background" colors (255,255,255) in
           let node = get_color "node-background" colors (239,239,239) in
           bg,node in
      {bg=bg;
       node=node_color;
       engines=engines}
     with 
     | Yojson.Basic.Util.Type_error (s,j) ->
        let () = Warnings.(issue_warning (Config (Bad_group (fullname,[],s,j,"A json object with fields \"signatures\" and \"colors\" was expected","Using default signature to engine mapping")))) in
        default)
  with
  | Utils.No_file(f,msg) ->
     let e = Error.System_error (Printf.sprintf "No such file \"%s\" in %s. Using default configuration." f msg) in
     let () = Logs.err (fun m -> m "Error: %s\n%!" (Error.error_msg e filename)) in
     default
  | Yojson.Json_error s ->
     let () = Warnings.(issue_warning (Config (Json_error s))) in
     default
       
let background_color {bg} = bg
let node_color {node} = node
let engines {engines} = engines
