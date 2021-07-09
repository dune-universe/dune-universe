(* Time-stamp: <modified the 07/05/2021 (at 18:49) by Erwan Jahier> *)
(* Opium/morphine like debugger *)
 
open RdbgArg
open RdbgEvent
open Mypervasives

(* a few shortcuts *)
let info_string () =
    let msg =  "The current session parameters are:
     sut: "^ (String.concat "," (List.map reactive_program_to_string args.suts)) ^ "
     env: "^ (String.concat "," (List.map reactive_program_to_string args.envs)) ^ "
     oracle: "^ (String.concat "," (List.map reactive_program_to_string args.oracles)) ^ "
     test length: " ^  (string_of_int args.step_nb) ^ "
     precision: " ^ (string_of_int args.precision) ^ "
     verbosity level: " ^ (string_of_int (args.verbose)) ^ "
     rif file name: " ^ args.output ^ "
     overwrite rif file? "
      ^ (if args.overwrite_output then "yes" else "no") ^ "
     coverage file name: " ^ args.cov_file ^ "
     do we stop  when an oracle returns false? " 
      ^ (if args.stop_on_oracle_error then "yes" else "no") ^ "
     display local var? " ^ (if (args.display_local_var) then "yes" else "no") ^ "

"
    in
    msg

let info () =
  output_string args.ocr (info_string ());
  flush args.ocr


let i = info

let run () = RdbgRun.start ()
let _next e = e.next
let _data e = e.data
let _terminate e = e.terminate
let off () = RdbgArg.args.RdbgArg.rdbg <- false
let _on () = RdbgArg.args.RdbgArg.rdbg <- true

let _ = 
  RdbgArg.args.RdbgArg.rdbg <- true


(* XXX rm? *)
let (get_val_event : RdbgEvent.t -> unit) =
  fun e -> 
    let nl,vl = List.split e.data in
    let nl = List.map (fun n -> String.uncapitalize_ascii n) nl in
    let vstrl = List.map (Data.val_to_string (string_of_float)) vl in
    let str =
      "let " ^ (String.concat "," nl) ^ 
        " = " ^ (String.concat "," vstrl) ^ ";;\n"
    in
    print_string (str)

(* (Ugly?) hack to be able to display online help

   I suppose that "rdbg.mli" is formatted like this:

(** cmd help message *)
val cmd : <cmd profile>

where 
   - val begins a line,
   - the cmd profile fits on a line
   - the space before the ":" matters
   
*)

let rdbg_lib_dir = 
  try
    let path = Mypervasives.run ("ocamlfind query rdbg") (fun s -> Some s) in
    let path = List.hd path in
    path 
  with _  -> (* try something else *)
    try
      let path = Mypervasives.run ("ocamlc -where") (fun s -> Some s) in
      let path = List.hd path in
      let sep = "/" in
      let l = Str.split (Str.regexp sep) path in
      let l = "rdbg-plugin"::(List.rev (List.tl l)) in
      let l = List.rev l in
      String.concat sep l
    with _ ->  (* try something else *)
      try
        let path = Mypervasives.run ("locate rdbg") (fun s -> Some s) in
        let path = List.hd path in
        path 
      with _ -> "" 
                
let rdbg_mli = 
  try readfile (Filename.concat rdbg_lib_dir "rdbgMain.mli"),"rdbgMain.ml"
  with _ -> "",""
let rdbg_utils_mli = 
  try readfile (Filename.concat rdbg_lib_dir "rdbgStdLib.mli"),"rdbgStdLib.ml"
  with _ -> "",""
let callgraph_mli = 
  try readfile (Filename.concat rdbg_lib_dir "callgraph.mli"),"callgraph.ml"
  with _ -> "",""



(* name, (profile,msg,category,file)  *)
type docu = string * (string * string * string * string)

(* parse the mli to get documentation *)
let (get_cmd_list: string * string -> docu list) =
  fun (mli, file) -> 
  let rec aux acc i0 = 
    try
      let i3 = Str.search_forward (Str.regexp ("^val ")) mli i0 in
      let i1 = Str.search_backward (Str.regexp ("^(\\*\\*")) mli i3 in
      let i1bis = (Str.search_forward (Str.regexp (":")) mli i1) - 1 in
      let i2 = Str.search_forward (Str.regexp ("\\*)")) mli i1 in
      let i4 = (Str.search_forward (Str.regexp ":") mli i3) - 1 in
      let i5 = Str.search_forward (Str.regexp "\n") mli i4 in
      if i1bis = i4 then ( (* no category => don't appear in the online doc *)
        if args.debug_rdbg then
          Printf.eprintf "No category for %s\n%!" (String.sub mli (i3+4) (i4-i3-4));
        aux acc i5
      )
      else if i1 < i0 then (
        if args.debug_rdbg then
          Printf.eprintf "No  help msg for for %s\n%!" (String.sub mli (i3+4) (i4-i3-4));
        aux acc i5
      ) else 
      (try 
         let help,cat =
           String.sub mli (i1bis+2) (i2-i1bis-2),
           String.sub mli (i1+4) (i1bis-i1-3)
         in
         let cmd = String.sub mli (i3+4) (i4-i3-4) in
         let profile = String.sub mli (i4+3) (i5-i4-3) in
         if args.debug_rdbg then (
           Printf.eprintf 
             "===> %si: i0=%i i1=%i i1bis=%i i2=%i i3=%i i4=%i i5=%i \n%!" 
             file i0 i1 i1bis i2 i3 i4 i5; 
           Printf.eprintf 
             "\tcmd=%s\n\tprofile=%s\n\thelp=%s\n\tcat=%s\n\tfile=%s\n%!"
             cmd profile help cat file; 
           flush stderr
         ); 
         aux ((cmd,(profile,help,cat,file))::acc) i5
       with Invalid_argument _ ->
         if args.debug_rdbg then Printf.eprintf
             "Bad format in comment in %si: i0=%i i1=%i i1bis=%i i2=%i i3=%i i4=%i i5=%i \n%!"
           file i0 i1 i1bis i2 i3 i4 i5;
         aux acc i5)
    with Not_found -> acc

  in
  List.rev(aux [] 0)

let (cmd_list: docu list) = 
  get_cmd_list rdbg_mli

let cmd_list_utils = get_cmd_list rdbg_utils_mli
let cmd_callgraph = get_cmd_list callgraph_mli
let all_cmds = ref (cmd_list @ cmd_list_utils @ cmd_callgraph)
let get_doc c  = try List.assoc c !all_cmds with Not_found -> "","","",""
let doc_prof c = (fun (x,_,_,_) -> x) (get_doc c)
let doc_msg c  = (fun (_, x,_,_) -> x) (get_doc c)
let doc_label c = (fun (_, _, x,_) -> x) (get_doc c)
let doc_file c = (fun (_, _, _, x) -> x) (get_doc c)
let add_doc_entry cmd profile msg cat file =
  if List.mem_assoc cmd !all_cmds then (
    Printf.printf
        "Warning: RdbgMain.add_doc_entry: the '%s' command already exist, and is overwritten\n
%!" cmd ;
    all_cmds := List.remove_assoc cmd !all_cmds
  );
  all_cmds := (cmd,(profile,msg,cat,file))::!all_cmds


let _get_profile str =
  try 
    let i = Str.search_forward (Str.regexp_string ("val "^str)) (fst rdbg_mli) 0 in
    let j = Str.search_forward (Str.regexp_string "\n")  (fst rdbg_mli) i in
      String.sub (fst rdbg_mli) (i+4) (j-i+4)
  with Not_found -> ""


let _print_cmd_list = 
  List.iter 
    (fun (cmd, (profile,_msg, _cat)) -> Printf.printf "\t%12s : %s\n" cmd profile)
    
let print_cmd_list2_string l = 
  List.fold_left  
    (fun acc (cmd, (_profile,msg, _cat, file)) ->
       let msg = try (* let's only keep the first line *)
           let i = Str.search_forward (Str.regexp "\n") msg 0 in
           (String.sub msg 0 i) 
         with Not_found -> msg
       in
       let s = String.length msg in
       let msg = if (s) < 45 then msg else
           (String.sub msg 0 42) ^ "..." 
       in
       let blank = String.make (max 0 (45-s)) ' ' in
       Printf.sprintf "%s\n%16s : %s%s (%s)" acc cmd msg blank file)
    ""
    l
    
let print_cmd_list2 l =
  Printf.printf "%s\n%!" (print_cmd_list2_string l)

let (apropos : string -> unit) =
  fun str -> 
    let l = List.filter 
        (fun (cmd,(_prof,msg,cat,file)) ->
           Str.string_match (Str.regexp (".*"^str^".*")) cmd 0 ||
           Str.string_match (Str.regexp (".*"^str^".*")) msg 0 ||
           Str.string_match (Str.regexp (".*"^str^".*")) cat 0||
           Str.string_match (Str.regexp (".*"^str^".*")) file 0
        ) !all_cmds 
    in
      print_cmd_list2 l
  


let (sort_cmd_by_categories: unit -> (string * docu list) list) =
  fun () ->
  let t = Hashtbl.create 5 in
  List.iter
    (fun (cmd, (p,m,cat,file)) ->
       match Hashtbl.find_opt t cat with
       | None -> Hashtbl.add t cat [(cmd,(p,m,cat,file))]
       | Some l -> Hashtbl.replace t cat ((cmd,(p,m,cat,file))::l)
    )
    !all_cmds;
  Hashtbl.fold (fun cat l acc -> (cat,l)::acc) t []
  

let (help_string:string -> string) =
  fun cmd ->
    if cmd="base" then (
      ("Type 
help \"function\";; 
  to obtain more help on the following functions: \n"^
      print_cmd_list2_string cmd_list)
    ) 
    else if cmd="utils" then (
      print_cmd_list2_string cmd_list_utils
    )
    else if cmd="cat" then (
      let docl = sort_cmd_by_categories () in
      let cat = fst (List.split docl) in
      Printf.sprintf "Available categories are: \n\t - %s " (String.concat "\n\t - " cat)
    )
    else if List.mem_assoc cmd !all_cmds then (
      let prof,help,cat,file = List.assoc cmd !all_cmds in
        Printf.sprintf "%s : %s [%s] (%s)\n%s\n" cmd prof cat file help 
    )
    else
      let docl = sort_cmd_by_categories () in
       match List.assoc_opt cmd docl with
       | None ->
         Printf.sprintf  "Unknown function '%s'. Available functions are: %s" cmd
        (String.concat "," (fst (List.split !all_cmds)))
       | Some cmds -> 
         print_cmd_list2_string (List.rev cmds)
      
let (help:string -> unit) = fun str -> 
  Printf.printf "%s\n%!" (help_string str)
    

let q () = print_string "bye!\n";flush stdout;exit 0
let quit () = q ()


let h = help
let a = apropos

let (man:unit -> unit) =
  fun () -> 
    output_string stdout " rdbg is a Reactive program DeBuGger. 
 - web doc:  http://www-verimag.imag.fr/DIST-TOOLS/SYNCHRONE/rdbg
 - tutorials:http://www-verimag.imag.fr/DIST-TOOLS/vtt/rdbg
 - online doc:
   + help cat     (* List commands categories *)
   + help <a cat> (* List commands related to a given category *)
   + help <a command>   (* Print the documentation of a command/function *)
   + apropos <a string> (* List functions related a string *)
   + apropos            (* List all functions *)
   + h (* Shortcut for help    *)
   + a (* Shortcut for apropos *)
   + l (* List rdbg commands *)
";
  flush stdout
 
let prompt = "(rdbg) "
let get_prompt () = prompt 
(* let set_prompt str = prompt := str *)
                 

