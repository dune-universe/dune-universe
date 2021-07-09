(* Time-stamp: <modified the 28/06/2021 (at 10:38) by Erwan Jahier> *)

open RdbgWrapperArgs

let supported_plugins = ["lutin";"lustre-v6";"sasa"]

(**********************************************************************************)
(* generete a rdbg-session.ml *)

let prolog () =  ((Mypervasives.entete2 "(*" "*)" RdbgVersion.str RdbgVersion.sha) ^
                 (if !lurette then "" else "
#use \"topfind\";; 
#require \"rdbg\";;")^("
open RdbgEvent;;
open RdbgArg;;
open RdbgRun;;
open RdbgMain;;
open RdbgStdLib;;
open Data;;
"))

let add_existence_check oc filename =
  Printf.fprintf oc
    "if not (Sys.file_exists \"%s\") then (
         Printf.printf \"Error: %s is missing!\\n\"; 
         exit 2);;\n"
    filename filename

  
let (tool_to_ocaml_pack_name: string -> string) = function
  | "lutin"   -> "lutin"
  | "lv6" -> "lustre-v6"
  | "lus2lic" -> "lustre-v6"
  | "sasa" -> "sasa"
  | "ocaml" -> "ocaml"
  | _ -> "stdio"

let sasAlgos = ref []
let sasadecl = ref true (* load sasa files only once *)
let dotfile = ref "" (* only one sasa program can be used *)
let gen_plugin_def packs_ref oc nodbg name (str:string) =
  let args = Str.split (Str.regexp "[ \t]+") str in
  let tool = Filename.basename (List.hd args) in
  let make_str, opt_forced, use_array =
    match tool with
    | "sasa"  -> "Sasa.SasaRun.make",[],true
    | "lutin" -> "LutinRun.make",[],true
    | "lus2lic" 
      | "lv6" ->
       if
         List.mem "-2c-exec" args ||
           List.mem "--to-c-execute" args
       then
         "StdioRun.make",[],false
       else
         (* necessary because lv6 -2c generates rif where all i/o are expanded.*)
         "Lv6Run.make",["--expand-io-type"],true
    | "ocaml"  -> "OcamlRun.make",[],false
    | _ -> "StdioRun.make",[],false
  in
  let args = args @ opt_forced in
  let args_str = String.concat ";" (List.map (fun s -> "\""^s^"\"") args) in
  let pn = tool_to_ocaml_pack_name tool in
  if not (List.mem pn !packs_ref || pn = "stdio") then packs_ref:=pn::!packs_ref;
  if (not !lurette && (List.mem pn supported_plugins)) then
    Printf.fprintf oc "#require \"%s\";;\n" pn;

  if !sasadecl && (!sasa_mode || !salut_mode) then (
    sasadecl := false;
    if not !lurette then Printf.fprintf oc "#require \"sasa\";;\n";
    let set_cust_mode =
      (if
         not !sasa_mode ||  (* salut does not have internal daemons *)
           List.fold_left
             (fun acc s ->
               match s with
               | "-custd" | "--custom-daemon" -> true
               | "--synchronous-daemon" | "-sd"
                 | "--central-daemon" | "-cd"
                 | "--locally-central-daemon" |  "-lcd"
                 | "--distributed-daemon"  |  "-dd"
                 | "--greedy-central-daemon" |  "-gcd"
                 | "--greedy-daemon" |  "-gd"
                 -> false (* the last option wins*)
               | _ -> acc
             )
             true
             args
       then
         "let custom_mode, sasa_mode = true, true ;;"
       else (
         "let custom_mode, sasa_mode = false, true ;;"
       ))
    in
    if !sasa_mode && not !salut_mode then missing_vars_at_the_end := true;
    if !sasa_mode then (
      sasAlgos :=
        Mypervasives.run ((String.concat " " args)^ " --list-algos") (fun s -> Some s);
      sasAlgos := List.flatten (List.map Mypervasives.string_to_string_list !sasAlgos)
    );
    let dot_file =
      if !salut_mode then
        (* Guess the name of the dot file *)
        if (Str.string_match (Str.regexp "\\./\\(.*\\.\\)exec") str 0) then
          (Str.matched_group 1 str)^ "dot"
        else
        if (Str.string_match (Str.regexp ".* \\(.*\\.\\)lus") str 0) then
          (Str.matched_group 1 str)^ "dot"
        else
           (Printf.printf
                      "Error: cannot figure out what the name  %s (in \"%s\")%s%s"
                      "of the .dot file could be" str
                      "\nExamples that work: \n\t\"./grid4.exec\" -> grid4.dot"
                      " \n\t\"lv6 grid4.lus -n bla\" -> grid4.dot \n%!";
            exit 2
            )
      else
        match List.find_opt (fun str -> Filename.extension str=".dot") args with
        | Some res -> res
        | None  -> failwith ("No dot files found in: "^(String.concat " " args)) 
    in
    let basename = Filename.chop_extension dot_file in
    let basename = Str.global_replace (Str.regexp "\\.") "" basename in
    if !sasa_mode then (
      add_existence_check oc "state.ml";
      List.iter (fun f -> add_existence_check oc f) !sasAlgos;
      add_existence_check oc "config.ml";
      add_existence_check oc (basename^".ml")
    );
    dotfile := dot_file;
    Printf.fprintf oc "let dotfile = \"%s\";;\n

let topology = Sasacore.Topology.read dotfile;;
let _ = Sasacore.Register.set_topology topology ;;
%s
" dot_file set_cust_mode;
    if !sasa_mode && not !lurette then (
      Printf.fprintf oc "#mod_use \"state.ml\"\n" ;
      List.iter (fun f -> Printf.fprintf oc "#mod_use \"%s\"\n" f) !sasAlgos;
      Printf.fprintf oc "#mod_use \"config.ml\"\n" ;
      Printf.fprintf oc "#mod_use \"%s.ml\"\n" basename
    );
  ); (* end sasa mode *)
  
  if pn = "stdio" then (
    Printf.fprintf oc "\nlet %s = 
let plugin = StdioRun.make \"%s\" in
plugin\n;;\n" name (String.concat " " args);
    flush oc
  ) else if pn = "ocaml" then (
    Printf.fprintf oc "\nlet %s = 
                       let plugin = OcamlRun.make \"%s\" in
                       plugin\n;;\n" name (List.nth args 1);
    flush oc    
  ) else (
    Printf.fprintf oc "\nlet %s = 
  let args = [%s] in
  let aargs = Array.of_list args in
  let plugin = %s %s in
  %s\n;;\n" name args_str make_str
      (if use_array then "aargs" else "(String.concat \" \" args)")
      (if nodbg then "let skip_dbg sl e cont = cont (plugin.step sl) e in
                      { plugin with step_dbg = skip_dbg }"
       else "plugin")

  ;
    flush oc)

let gen_arg_fill oc rp plugin_list =
    Printf.fprintf 
      oc "let _ = args.%s <- [%s];;\n" 
      rp
      (String.concat ";" (List.map (fun n-> "Ocaml("^n^")") plugin_list))
 
let rdbg_tuning () =
  Printf.sprintf " (* Generated by rdbg because not existing *)
#use \"%s\";;
"
    rdbg_cmds_file

let amend_tuning_if_necessary () =
  Printf.printf "Reading %s..\n%!" rdbg_tuning_file;
  let str = Mypervasives.readfile rdbg_tuning_file in
  try 
    ignore (Str.search_forward (Str.regexp "sasa-rdbg-cmds") str 0)
  with Not_found -> 
    Printf.printf "Modifying %s\n%!" rdbg_tuning_file;
    let oc = open_out rdbg_tuning_file in
    Printf.fprintf oc "
%s
#use \"sasa-rdbg-cmds.ml\";;
" str;
    flush oc;
    close_out oc

    
let rdbg_cmds ()=
  "(**********************************************************************)
(* Some commands implemented on top of rdbg libs

CAUTION: This  file is overwritten each  time you launch rdbg.  If you 
         want to tune it,i t is better to modify the my-rdbg-tuning-file *)

open RdbgStdLib
open RdbgMain

 (* In  order to use  emacs, you need  to install the  emacs highlight \
   package available
   in melpa *)
let hl hook (file, line, charb, chare) =
  let  cmd =  Printf.sprintf  \"emacsclient -e  '(save-selected-window \
   (find-file-other-window  \\\"%s\\\")   %s  (goto-char  (point-min)) \
   (forward-line   %i)   (forward-char  %i)   (set-mark-command   nil) \
   (forward-char %i) (hlt-highlight-region) )' \"
 file hook (line-1) charb (max 1 (chare-charb))
  in
  ignore(Sys.command cmd)

let hl_atoms = function
  | []-> ()
  | x::t -> hl \"(hlt-unhighlight-region)\" x; List.iter (hl \"\") t
(*| x::t -> hl \"(hlt-unhighlight-all-prop t)\" x; List.iter (hl \"\") \
   t *)

let _ =
  let cmd = \"emacsclient -e '(hlt-choose-default-face \\\"org-checkbox\\\")'\" in
  ignore(Sys.command cmd)
;;
let emacs = ref false

let emacs_udate e =
  if !emacs then
  try
  match e.sinfo with
  | None -> ()
  | Some si ->
     let si_list =
       List.map
         (fun si -> (si.file, fst si.line, fst si.char, snd si.char))
         (si()).atoms
     in
     hl_atoms si_list
  with
    Match_failure _ -> ()

(**********************************************************************)
(* to implement the undo command *)
let redos = ref [1];;
let store i = redos := i::!redos;;

(**********************************************************************)
(* handy short-cuts *)

let _ = time_travel true;;
let e = ref (RdbgStdLib.run ~call_hooks:false ());;
let pe () = try get_hook \"print_event\" !e with _ -> ();;
let s () = e:=step !e ; emacs_udate !e; store !e.nb;pe();;
let si i = e:=stepi !e i; emacs_udate !e; store !e.nb;pe();;
let n () = e:=next_np !e ; emacs_udate !e; store !e.nb;pe();;
let ni i = e:=nexti_np !e i; emacs_udate !e; store !e.nb;pe();;
let g i = e:=goto !e i; emacs_udate !e; store !e.nb;pe();;
let b () = e:=back !e ; emacs_udate !e; store !e.nb;pe();;
let bi i = e:=backi !e i; emacs_udate !e; store !e.nb;pe();;
let r  () = !e.RdbgEvent.reset();  
   e:=RdbgStdLib.run ~call_hooks:true ();
   emacs_udate !e; 
   redos := [1];;
let c () = e:= continue !e;emacs_udate !e; store !e.nb;;
let cb () = e:= rev !e;emacs_udate !e; store !e.nb;;
let fc predicate =
   e := next_np !e; while not (predicate ()) do e := next_np !e; done;
   get_hook \"print_event\" !e;
   emacs_udate !e;
   store !e.nb;;
let bc predicate =
   e := back !e; while not (predicate ()) do e := back !e; done;
   get_hook \"print_event\" !e;
   emacs_udate !e;
   store !e.nb;;

let sinfo () = match !e.sinfo with  Some si -> si() | None -> failwith \
   \"no source info\";;
let undo () =
  match !redos with
  | _::i::t -> redos := i::t; e:=goto !e i;emacs_udate !e
  | _ -> e:=goto !e 1; emacs_udate !e

let u = undo
let vv str = v str !e;;

let _ =
  add_doc_entry \"s\" \"unit -> unit\" \"Moves forward of one step\"
     \"move\" \"rdbg-cmds.ml\";
  add_doc_entry \"si\" \"int -> unit\" \"Moves forward of n steps\"
     \"move\" \"rdbg-cmds.ml\";
  add_doc_entry \"n\" \"unit -> unit\" \"Moves forward of one event\"
     \"move\" \"rdbg-cmds.ml\";
  add_doc_entry \"ni\" \"int -> unit\" \"Moves forward of i events\"
     \"move\"\"rdbg-cmds.ml\";
  add_doc_entry \"g\" \"int -> unit\" \"Goes to event number i\"
     \"move\" \"rdbg-cmds.ml\";
  add_doc_entry \"b\" \"unit -> unit\" \"Moves backward of one event\"
     \"move\" \"rdbg-cmds.ml\";
  add_doc_entry \"bi\" \"int -> unit\" \"Moves backward of i events\"
     \"move\" \"rdbg-cmds.ml\";
  add_doc_entry \"bc\" \"(unit -> bool) -> unit\"
      \"Moves  backward  until a  predicate  (of  type unit  ->  bool) \
   becomes true
      \" \"move\" \"rdbg-cmds.ml\";
  add_doc_entry \"fc\" \"(unit -> bool) -> unit\"
      \"Moves forward until a predicate (of type unit -> bool) becomes \
   true\"
      \"move\" \"rdbg-cmds.ml\";
  add_doc_entry \"u\" \"unit -> unit\" \"Undo the last move\"
      \"move\" \"rdbg-cmds.ml\";
  add_doc_entry \"vv\" \"string -> Data.v\"
   \"Returns the value of a variable attached to the current event (if \
   available)\"
    \"data\" \"rdbg-cmds.ml\";
  add_doc_entry \"r\" \"unit -> unit\" \"Restarts to the first event\"
    \"main\" \"rdbg-cmds.ml\";
  add_doc_entry  \"l\"  \"unit ->  unit\"  \"Prints  this list  of  L0 \
   commands description\"
    \"main\" \"rdbg-cmds.ml\";
  add_doc_entry \"where\" \"unit -> unit\"
    \"Prints the call stack with source information\"
    \"misc\" \"rdbg-cmds.ml\";
  add_doc_entry \"sinfo\" \"unit -> unit\"
     \"Returns source  information attached  to the current  event (if \
   available)\"
     \"data\" \"rdbg-cmds.ml\";
  add_doc_entry \"c\"  \"unit -> unit\" \"Continues  forward until the \
   next breakpoint\"
     \"move\" \"rdbg-cmds.ml\";
  add_doc_entry \"cb\" \"unit -> unit\" \"Continues backward until the \
   previous breakpoint\"
    \"move\" \"rdbg-cmds.ml\";
  add_doc_entry  \"blist\" \"unit  ->  unit\" \"Lists  of the  current \
   breakpoints\"
     \"move\" \"rdbg-cmds.ml\";

  add_doc_entry \"nm\" \"string -> unit\"
     \"Moves forward until an event which name matches a <string>\"
    \"move\" \"rdbg-cmds.ml\";
  add_doc_entry \"pm\" \"string -> unit\"
    \"Moves backward until a function which name matches a <string>\"
    \"move\" \"rdbg-cmds.ml\";
  add_doc_entry \"exit\"  \"unit -> unit\"  \"Goes to the exit  of the \
   current event\"
    \"main\" \"rdbg-cmds.ml\";


(**********************************************************************)
open Callgraph;;
let cgf () = gen_call_graph_full !e;;
let cg () = (gen_call_graph !e);;
let dcg ()= ignore(display_call_graph ());;
let bt () = print_src !e;;
let finish () = loopforever !e;;
let where () = print_call_stack !e;;

let _ =
  add_doc_entry \"cgf\" \"unit -> unit\"
    \"Generates  the  full call  graph  recursively  (i.e., nodes  are \
   clickable) \"
    \"graph\" \"rdbg-cmds.ml\" ;
  add_doc_entry \"cg\" \"unit -> unit\"
    \"Generates the call graph from the current event\"
    \"graph\" \"rdbg-cmds.ml\";
  add_doc_entry \"dcg\" \"unit -> unit\" \"Displays the generated call \
   graph\"
   \"graph\" \"rdbg-cmds.ml\"
;;

(**********************************************************************)
(* Chronograms *)

let sim2chro () = 
  let cmd = Printf.sprintf \"sim2chrogtk -ecran -in %s > /dev/null &\" args.output in 
  Sys.command cmd

let gnuplot () = 
  let cmd = Printf.sprintf \"gnuplot-rif %s > /dev/null &\" args.output in 
  Sys.command cmd

let _ =
  add_doc_entry \"sim2chro\" \"unit -> unit\" \"Calls sim2chro\" \"misc\" \"rdbg-cmds.ml\" ;
  add_doc_entry \"gnuplot\" \"unit -> unit\"  \"Calls gnuplot-rif \" \"misc\" \"rdbg-cmds.ml\" 
;;

(**********************************************************************)
(* breakpoints *)

let blist () = !breakpoints;;

(**********************************************************************)
(* Go to the next event which name contains a string (useful for
   navigating inside lustre or lutin programs) *)
let nm str = e:= next_match !e str ;;
let pm str = e:=previous_match !e str ;;


(* go to the exit of the current event *)
let exit () =
   if !e.kind <> Exit then e:=next_cond !e
     (fun  ne-> ne.kind  = Exit  && ne.name  = !e.name  && ne.depth  = \
   !e.depth) ;;
(**********************************************************************)
(* Online Level 0 doc *)

let l () = print_string (\"
Here  is  the  list  of  rdbg  Level  0  commands  (i.e.,  defined  in \
   rdbg-cmds.ml) :
- forward moves:
  n: \"^(RdbgMain.doc_msg \"n\")^\"
  ni i: \"^(RdbgMain.doc_msg \"ni\")^\"
  s: \"^(RdbgMain.doc_msg \"s\")^\"
  si i: \"^(RdbgMain.doc_msg \"si\")^\"
  nm <string>: \"^(RdbgMain.doc_msg \"nm\")^\"
  fc pred:\"^(RdbgMain.doc_msg \"fc\")^\"
  exit: \"^(RdbgMain.doc_msg \"exit\")^\"

- backward moves:
  b: \"^(RdbgMain.doc_msg \"b\")^\"
  bi i: \"^(RdbgMain.doc_msg \"bi\")^\"
  g i: \"^(RdbgMain.doc_msg \"g\")^\"
  pm <string>: \"^(RdbgMain.doc_msg \"pm\")^\"

- Call graphs (requires GraphViz dot)
  cg: \"^(RdbgMain.doc_msg \"cg\")^\"
  cgf: \"^(RdbgMain.doc_msg \"cgf\")^\"
  dcg: \"^(RdbgMain.doc_msg \"dcg\")^\"

- Breakpoints:
  break <brpt>: \"^(RdbgMain.doc_msg \"break\")^\"
  c: \"^(RdbgMain.doc_msg \"c\")^\"
  cb: \"^(RdbgMain.doc_msg \"cb\")^\"
  blist: \"^(RdbgMain.doc_msg \"blist\")^\"
  delete: \"^(RdbgMain.doc_msg \"delete\")^\"

- misc:
  where: \"^(RdbgMain.doc_msg \"where\")^\"
  sinfo: \"^(RdbgMain.doc_msg \"sinfo\")^\"
  vv: \"^(RdbgMain.doc_msg \"vv\")^\"

- main:
  l: \"^(RdbgMain.doc_msg \"l\")^\"
  i: \"^(RdbgMain.doc_msg \"i\")^\"
  r: \"^(RdbgMain.doc_msg \"r\")^\"
  u: \"^(RdbgMain.doc_msg \"u\")^\"
  q: \"^(RdbgMain.doc_msg \"q\")^\"
  a: \"^(RdbgMain.doc_msg \"a\")^\"
  h: \"^(RdbgMain.doc_msg \"h\")^\"
\");;
"

let _ =
  if !lurette then () else
  if Sys.file_exists rdbg_tuning_file then () else (
    let oc = open_out rdbg_tuning_file in
    output_string oc (rdbg_tuning());
    flush oc;
    close_out oc);
  let oc = open_out rdbg_cmds_file in
  output_string oc (rdbg_cmds());
  flush oc;
  close_out oc
    
let empty_session () =
  !suts = [] && !envs = [] && !suts_nd = [] && !envs_nd = []
                                                          
let epilog () =
  (Printf.sprintf "
let emacs = ref %b;; 
let _ =
 args.display_gnuplot <- %b;
 args.display_sim2chro <- %b;
 args.rdbg <- true;
 args.step_nb <- %i;
 args.output <- \"%s\";
 args.no_rif <- %b;
 args.overwrite_output <- %b;
 args.stop_on_oracle_error <- not %b;
 args.log <- %b;
 args.reset_cov_file <- %b;
 args.cov_file <- \"lurette.cov\";
 args.stop_on_oracle_error <- true;
 args.luciole_mode <- %b;
 args.sasa_mode <- %b;
 args.salut_mode <- %b;
 args.missing_vars_at_the_end <- %b;
 args.debug_rdbg <- %b;;
"
     !emacs_mode
     !display_gnuplot
     !display_sim2chro
     !test_length !output_file
     !no_rif
     !overwrite_output
     !dont_stop_on_oracle_error
     !log
     !reset_cov_file
     !luciole_mode
     !sasa_mode
     !salut_mode
     !missing_vars_at_the_end
     !drdbg
  )^
  if !lurette then
    Printf.sprintf
      " 
 args.rdbg <- false; (* set on the lurette mode... *)
 try del_all_hooks (); run() (* ...and run! *)
 with RdbgEvent.End _  -> Printf.printf \"bye\\n%!\";exit 0;;\n"
  else if empty_session () then "" else ("
#use \""^rdbg_tuning_file^"\";;
")

(* Fill the session file and returns the list of used plugins  *)
let gen_plugins oc =
  let give_unique_name (i, acc_names, acc_calls) str =  
      let name = Printf.sprintf "plugin_%i" i in
      (i+1, name::acc_names, str::acc_calls)
  in
  let i, sut_names, (sut_calls:string list) = 
    List.fold_left give_unique_name (0, [],[]) !suts
  in
  let i, sut_names_nd, sut_calls_nd = 
    List.fold_left give_unique_name (i, [],[]) !suts_nd
  in
  let i, env_names, env_calls = 
    List.fold_left give_unique_name (i,[],[]) !envs
  in
  let i, env_names_nd, env_calls_nd = 
    List.fold_left give_unique_name (i,[],[]) !envs_nd
  in
  let i, oracle_names, oracle_calls = 
    List.fold_left give_unique_name (i,[],[]) !oracles
  in
  let _, oracle_names_nd, oracle_calls_nd = 
    List.fold_left give_unique_name (i,[],[]) !oracles_nd
  in
  let sut_names = List.rev sut_names in
  let sut_names_nd = List.rev sut_names_nd in
  let env_names = List.rev env_names in
  let env_names_nd = List.rev env_names_nd in
  let oracle_names = List.rev oracle_names in
  let oracle_names_nd = List.rev oracle_names_nd in
  let plugRef = ref [] in
  output_string oc (prolog());
  List.iter2 (gen_plugin_def plugRef oc false) sut_names sut_calls;
  List.iter2 (gen_plugin_def plugRef oc true ) sut_names_nd sut_calls_nd;
  List.iter2 (gen_plugin_def plugRef oc false) env_names env_calls;
  List.iter2 (gen_plugin_def plugRef oc true ) env_names_nd env_calls_nd;
  List.iter2 (gen_plugin_def plugRef oc false) oracle_names oracle_calls;
  List.iter2 (gen_plugin_def plugRef oc true ) oracle_names_nd oracle_calls_nd;
  gen_arg_fill oc "suts" (sut_names@sut_names_nd);
  gen_arg_fill oc "envs" (env_names@env_names_nd);
  gen_arg_fill oc "oracles" (oracle_names@oracle_names_nd);
  output_string oc (epilog());
  flush oc;
  !plugRef

(** [f outc] generates a fresh session file. [outs] is used to output messages.

    It returns 
     - the name of the generated session file
     - the list of necessary plugins that is built out of CL args. 

    As a side effect, it also generates the following files:
    - In rdbg mode: 
      + my-rdbg-tuning.ml (if not existing) 
      + rdbg-cmds.ml 
    - In lurette mode 
      +

    In sasa mode, also generates a [read_dot.ml] file

*)
let file outc =
  let session_file_base = if !lurette then "luretteSession" else "rdbg-session" in
  let i = ref 0 in
  let str = ref "" in
  while (Sys.file_exists (session_file_base^ !str^".ml")) do 
    incr i;
    str := (string_of_int !i);
  done;
  let session_file_base = session_file_base^ !str in
  let session_file_name = session_file_base^".ml" in
  let foc = open_out session_file_name in
  let plugins = gen_plugins foc in
  close_out foc;
  Printf.fprintf outc "  - The file \"%s\" has been generated.\n%!" session_file_name;

  let sasa = !salut_mode || !sasa_mode in
  if sasa then (
    amend_tuning_if_necessary ();
    assert (!dotfile <> "");
    let read_dot = "read_dot.ml" in
    let roc = open_out read_dot in
    Printf.fprintf roc "%s
let dotfile = \"%s\"

let _ =
  let g = Sasacore.Topology.read dotfile in
  Sasacore.Register.set_topology g;

%!"
      (Mypervasives.entete2 "(* " " *)" RdbgVersion.str RdbgVersion.sha)
      !dotfile;
    Printf.fprintf outc "  - The file \"%s\" has been generated.\n%!" read_dot;
    close_out roc
  );
  session_file_base, plugins


let lurette outc =
  let session_file_base, plugins = file outc in
  let plugins =
    List.filter (fun p -> List.mem p supported_plugins) plugins
  in
  let plugins = "rdbg"::plugins in
  let plugins = String.concat "," plugins in
  let read_dot, dot_ml = if !dotfile = "" then "","" else
      "read_dot.ml",
      ((Filename.remove_extension !dotfile)^".ml")
  in
  let sasa = !sasAlgos <> [] in
  let other_ml = if sasa then
      "state.ml " ^(String.concat " " !sasAlgos) ^ " config.ml" else ""
  in
  let ocaml_compiler = if !ocamldebug then "ocamlc -g" else "opt" in
  let cmd =
    Printf.sprintf
      "ocamlfind %s -o %s -package %s -linkpkg %s %s %s %s.ml\n"
      ocaml_compiler session_file_base plugins read_dot other_ml
      dot_ml session_file_base
  in
  let cmd2 = Printf.sprintf "%s ./%s"
      (if !ocamldebug then "ocamldebug" else "") session_file_base
  in
  let msg = Printf.sprintf " 
 In order to re-run the test using do:
      \027[00m%s\027[32m
 If you tune %s.ml, you need to regenerate %s like this:
      \027[00m%s"
      cmd2 session_file_base session_file_base cmd
  in        
  Printf.printf "\027[32m %s \027[00m\n%!" msg ;
  let err_code = Sys.command cmd in
  let err_code = if err_code > 0 then err_code else Sys.command cmd2 in
  flush stdout;
  flush_all();
  exit err_code

