open UtilsLib
open AcgData

let background = Ocf.(triple ~doc: "Comment: Background color as a triple of int as RGB values" Wrapper.int Wrapper.int Wrapper.int (255,255,255))
  
let node_background = Ocf.(triple ~doc: "Comment: Node background color as a triple of int as RGB values" Wrapper.int Wrapper.int Wrapper.int (128,128,128))


let color_options =
  let g = Ocf.add Ocf.group ["background"] background in
  Ocf.add g ["node-background"] node_background



let name = Ocf.string ~doc: "Comment: name of the signature" "dummy name"
let engine = Ocf.string ~doc: "Comment: name of the rendering engine (trees, logic, syntactic) " "DUMMY NAME"
  
(* This function defines the group options for signatures. It contains
   the name of the signatue ("name" option) and its rendering engine
   ("engine" option*)
let engine_group =
  let rendering_option =
    let g = Ocf.add Ocf.group ["name"] name in
    Ocf.add g ["engine"] engine in
  rendering_option
    
let engine_group_wrapper =
  let _default_engine_group = engine_group in
  Ocf.Wrapper.make
    Ocf.to_json
    (fun ?def:g j ->
      match g with
      | None ->
	 let () = Ocf.from_json engine_group j in
	 let n = Ocf.get name in
	 let e = Ocf.get engine in
	 Ocf.(add (add group ["name"] (string n)) ["engine"] (string e))
      | Some actual_g ->
	 let () = Printf.printf "Actual Branch\n%!" in
	 let () = Ocf.from_json actual_g j in actual_g)
    
let make_sig_list_options lst =
  Ocf.(list
	 ~doc: "List of signatures with a specific rendering engine"
	 engine_group_wrapper
	 (List.map (fun (n,e) ->
	   let () = Ocf.set name n in
	   let () = Ocf.set engine e in
	   Ocf.group ) lst))

let sig_options = make_sig_list_options [("dummy","DUMMY")]
  
let default_config_group =
  let g = Ocf.(add_group group ["colors"] color_options) in
  Ocf.add g ["signatures"] sig_options


type engine = STRINGS | LOGIC | DERIVED_TREES | TREES | DEFAULT

let get_engine s =
  match String.lowercase_ascii s with
  | "strings" -> STRINGS
  | "logic" -> LOGIC
  | "unranked trees" -> DERIVED_TREES
  | "trees" ->  TREES
  | _ -> DEFAULT

    
type config = {bg:int * int * int;node:int*int*int;engines:engine Utils.StringMap.t}


let get_sig_engines m =
  let grp_lst = Ocf.get sig_options in
  List.fold_left
    (fun acc g -> 
      let j = Ocf.to_json g  in
      let () = Ocf.from_json engine_group j in
      let n = Ocf.get name in
      let e = Ocf.get engine in
      Utils.StringMap.add n (get_engine e) acc) m grp_lst

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
  
let get_config filename includes =
  try
    let fullname = Utils.find_file filename includes in
    let () = Ocf.from_file default_config_group fullname in
    let bg = Ocf.get background in
    let node_color = Ocf.get node_background in
    let engines = get_sig_engines Utils.StringMap.empty in
    {bg=bg;
     node=node_color;
     engines=engines}
  with
  | Ocf.Error e -> let () = Printf.fprintf stderr "Ocf error: %s\n%!" (Ocf.string_of_error e) in default
  | Utils.No_file(f,msg) ->
     let e = Error.System_error (Printf.sprintf "No such file \"%s\" in %s" f msg) in
     let () = Printf.fprintf stderr "Error: %s\n%!" (Error.error_msg e filename) in
     default
       

let background_color {bg} = bg
let node_color {node} = node
let engines {engines} = engines
