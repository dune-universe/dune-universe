(* Time-stamp: <modified the 04/07/2021 (at 17:33) by Erwan Jahier> *)
(* Mimick the behavior of 'rdbg -lurette', but without the dependency
   on ocaml *)

open RdbgWrapperArgs

let url_doc="http://www-verimag.imag.fr/DIST-TOOLS/SYNCHRONE/lustre-v6/"

let _ =
  lurette := true;
  usage := (Sys.argv.(0) ^
               " is equivalent to 'rdbg -lurette', except that the lustre-v6
and the lutin plugins are statically linked to it, to avoid the need of an 
ocaml compiler. One can not use other language plugins as 'rdbg -lurette'
would allow.
cf also "^url_doc^"

usage: "^ Sys.argv.(0) ^" [<options>] 
     where <options> are:")

             
let get_extension f =
  try
    let bf = Filename.chop_extension f in
    let lf,lbf = String.length f, String.length bf in
    String.sub f (lbf+1) (lf - lbf - 1)
  with _ -> ""

let args= ref []
let ml_files = ref []

let add_args arg = 
  if get_extension arg = "ml" 
  then 
    ml_files := arg::!ml_files 
  else
    args := (arg::!args)

let others = ref []
let (add_other : string -> unit) =
  fun s -> 
    others := s:: !others

let other_args:string =
  mkoptab ();
  Arg.parse_argv Sys.argv !arg_options add_args !usage;
  String.concat " " (List.rev !others)

(**********************************************************************************)

let rec get_dot = function
  | [] -> failwith "No dot file in found"
  | x::t ->
    if Filename.extension x = ".dot" then x else get_dot t

let (gen_reactive_program : string -> RdbgArg.reactive_program) =
  fun str -> 
    let args = Str.split (Str.regexp "[ \t]+") str in
    let tool = Filename.basename (List.hd args) in
    let plugin =
      match tool with
      | "lutin"   -> LutinRun.make (Array.of_list args)
      | "lv6" | "lus2lic" ->
        if
          List.mem "-2c-exec" args ||
          List.mem "--to-c-execute" args
        then
          StdioRun.make str
        else
          Lv6Run.make (Array.of_list (args@["--expand-io-type"]))
      | "ocaml" -> OcamlRun.make (List.nth args 1)
                     
(*  the following code compile (with adding sasa lib in the dune file)
    But there is an issue in practice:
    - Dynlink requires that the lurette binary is built with -linkall (otherwise
     i cannot use  stdlib in my cmxs for instance)
    - But if I use  -linkall, lurette fails to build because of lutin (or camlidl)
     that requires libuid (undefined reference to `IID_IUnknown')
    - sigh

      | "sasa" ->
        let dot = get_dot args in
        let cmxs = (Filename.chop_extension dot) ^ ".cmxs" in
        Dynlink.loadfile cmxs;
        Sasa.SasaRun.make (Array.of_list (args@["-rif"]))  
 *)
      | "sasa" -> StdioRun.make (str ^ " -rif") (* don't work without this option *)
      | _ -> StdioRun.make str 
    in
    Ocaml(plugin)

open RdbgArg    
let _ =        
  let sut_plugins = List.map gen_reactive_program (!suts @ !suts_nd) in
  let env_plugins = List.map gen_reactive_program (!envs @ !envs_nd) in
  let oracle_plugins = List.map gen_reactive_program (!oracles @ !oracles_nd) in

  args.suts <- sut_plugins;
  args.envs <- env_plugins;
  args.oracles <- oracle_plugins;

  args.missing_vars_at_the_end <- !missing_vars_at_the_end;
  args.step_nb <- !test_length;
  args.display_sim2chro <- !display_sim2chro ;
  args.display_gnuplot <- !display_gnuplot ;
  args.verbose <- if !verbose then 1 else 0 ;
  args.output <- !output_file;
  args.luciole_mode <- !luciole_mode;
  
  args.overwrite_output <- !overwrite_output;
  args.stop_on_oracle_error <- not !dont_stop_on_oracle_error;
  args.log <- !log;
  args.cov_file <- !cov_file;
  args.reset_cov_file <- !reset_cov_file;

  args.debug_rdbg <- !drdbg;
  args.rdbg <- false;

  args.cov_file <- "lurette.cov";
  args.stop_on_oracle_error <- not !dont_stop_on_oracle_error;
  ()

(**********************************************************************************)

let _ =
  if args.suts = [] && args.envs = [] then (
    Printf.printf "\027[35mE: Please set at least one sut or one env \027[00m\n\n" ;
    flush stdout;
    help()
  )
  else
  try RdbgRun.lurette_start()
  with
  | RdbgRun.OracleError str ->
     Printf.printf "\027[35m %s \027[00m\n"  str;
     flush stdout
                     
  | Dynlink.Error msg -> (* deadcode *)
     Printf.eprintf "\n*** error in lurette (Dynlink.loadfile %s).\n*** %s.\n%!" 
       (List.fold_left (fun acc x -> acc^" "^x) "" args._others)
       (Dynlink.error_message msg);
     RdbgRun.clean_terminate 2
  | RdbgEvent.End(_i) ->
     RdbgRun.clean_terminate 0
  | pb ->
     Printf.fprintf args.ocr "\n%s\n%!" (Printexc.to_string pb);
     close_out args.ocr;
     close_out args.ecr;
     close_in args.icr;
     Printf.printf "bye\n%!";
     RdbgRun.clean_terminate 2
;;

let _ =
  Printf.printf "lurette: bye\n"; flush stdout; exit 0

