(* Time-stamp: <modified the 30/06/2021 (at 10:22) by Erwan Jahier> *)

let rdbg_tuning_file = "my-rdbg-tuning.ml"
let rdbg_cmds_file = "rdbg-cmds.ml"

let pversion () =
  Printf.fprintf stdout "%s (%s)\n" RdbgVersion.str RdbgVersion.sha;
  flush stdout;
  exit 0

(* exported *)
let verbose = ref false
let lurette = ref false
let ocamldebug = ref false
let suts = ref []
let envs = ref []
let oracles = ref []
let suts_nd = ref []
let envs_nd = ref []
let oracles_nd = ref []
let test_length = ref 100
let missing_vars_at_the_end = ref false
let output_file = (* what is the better default name ?? *)
  ref (Filename.basename (Sys.getcwd()) ^".rif")
let input_file = ref None
let no_rif = ref false
let overwrite_output = ref false
let drdbg = ref false
let go = ref false
let options = ref []
let options_more = ref []
let arg_options = ref []
let emacs_mode = ref false
let display_gnuplot = ref false
let display_sim2chro = ref false
let dont_stop_on_oracle_error = ref false
let log = ref true
let cov_file = ref "lurette.cov"
let reset_cov_file = ref false
let luciole_mode = ref false
let sasa_mode = ref false
let salut_mode = ref false
let cmd = ref ""

let margin = ref 6 
let usage = ref ""
                 
let pspec os (c, ml) = (
  let (m1, oth) = match ml with
	 |	h::t -> (h,t)
	 |	_ -> ("",[])
  in
  let t2 = String.make !margin ' ' in
  let cl = String.length c in
  let t1 = if (cl < !margin ) then
	 String.make (!margin - cl) ' '
  else
	 "\n"^t2
  in
	 Printf.fprintf os "%s%s%s" c t1 m1;
	 List.iter (function x -> Printf.fprintf os "\n%s%s" t2 x) oth ;
	 Printf.fprintf os "\n" ;
)

let help () =
  let l = List.rev !options in
	Printf.fprintf stdout "%s\n\n" !usage;
	List.iter (pspec stdout) l;
	exit 0

let more () =
  let l = List.rev !options_more in
	List.iter (pspec stdout) l;
	exit 0

type opt_cat = Main | More | Lurette

let (mkopt : string list -> ?cat:opt_cat -> ?arg:string -> Arg.spec ->
     string list -> unit) =
  fun ol ?(cat=Main)  ?(arg="") se ml ->
  if cat = Lurette then () (* some args are meaningless from lurette *) 
  else
    let treto o = arg_options := (o, se, ""):: !arg_options in
	 List.iter treto ol ;
    let opt_str = (String.concat ", " ol)^arg in
    if cat = Main  
    then options := (opt_str, ml):: !options
	 else options_more := (opt_str, ml):: !options_more

                      
let (mkoptab : unit -> unit) = 
  fun () ->
    mkopt ["-sut";"--sut"]  ~arg:" <string>"
          (Arg.String (fun str -> (suts :=  str::!suts)))
          ["Add a sut "];
    mkopt ["-sut-nd";"--sut-nd"]  ~arg:" <string>"
          (Arg.String (fun str -> (suts_nd :=  str::!suts_nd)))
          ["Add a sut, but don't debug it"];
    mkopt ["-env";"--env"]  ~arg:" <string>"
          (Arg.String (fun str -> (envs :=  str::!envs)))
          ["Add an env"];
    mkopt ["-env-nd";"--env-nd"]  ~arg:" <string>"
          (Arg.String (fun str -> (envs_nd :=  str::!envs_nd)))
          ["Add an env, but don't debug it"];
    mkopt ["-oracle";"--oracle"]  ~arg:" <string>"
          (Arg.String (fun str -> (oracles :=  str::!oracles)))
          ["Add an oracle"];
    mkopt ["-oracle-nd";"--oracle-nd"]  ~arg:" <string>"
          (Arg.String (fun str -> (oracles_nd :=  str::!oracles_nd)))
          ["Add an oracle, but don't debug it"];
    mkopt ["-lurette";"--lurette"]  ~cat: (if !lurette then Lurette else Main)
          (Arg.Unit (fun _ -> (lurette := true)))
          ["Use the lurette mode (i.e., runs everything without debugging)"];
    mkopt ["-ocd";"--ocamldebug"]  ~cat: More
          (Arg.Unit (fun _ -> (lurette := true; ocamldebug := true)))
          ["Run in lurette mode, but under the control of ocamldebug"];
    mkopt ["-g";"--gnuplot"] ~cat: More
          (Arg.Unit (fun () -> display_gnuplot := true))
          ["in lurette mode only:  Launch gnuplot to visualize the generated rif file" ];
    mkopt ["-s";"--sim2chro"]  ~cat: More 
          (Arg.Unit (fun () -> display_sim2chro := true))
          ["in lurette mode only:  Launch sim2chro to visualize the generated rif file" ];
    mkopt ["-l";"--test-length";"--step-number"] ~arg:" <int>"
          (Arg.Int (fun i -> (test_length := i)))
          ["in lurette mode only: Set test length (step number)"];
    mkopt ["--missing-vars-last"]  ~cat: More 
          (Arg.Unit (fun () -> (missing_vars_at_the_end := true)))
          ["Launch the missing variables process (luciole or stdin) in last (i.e., after env, and sut)"];
    mkopt ["--luciole"]  ~cat: More 
          (Arg.Unit (fun () -> (luciole_mode := true)))
          ["Launch luciole when variables are missing (stdin is used otherwise)"];
    mkopt ["--sasa"] ~cat: More 
          (Arg.Unit (fun () -> (sasa_mode := true)))
          ["Tell rdbg that one RP comes from sasa"];
    mkopt ["--salut"]  ~cat: More 
          (Arg.Unit (fun () -> (salut_mode := true)))
          ["Tell rdbg that one RP comes from salut"];

    mkopt ["-o";"-output";"--output";"-rif";"--rif"] ~arg:" <string>"
          (Arg.String (fun s -> output_file := s))
          ["Set the output file name (currently,  \""^ !output_file ^ "\")" ];
    mkopt ["-in";"--input"]  ~cat: More
          (Arg.String (fun str -> input_file := Some str))
          ["Read rdbg command in a file and exits"];
    mkopt ["-norif";"--no-rif"]  ~cat: More
          (Arg.Unit (fun () -> no_rif := true))
          ["Do not print data in the rif file (useful when there are too many variables)";
           "nb: inhibits  --sim2chro (which is fine when there are too many variables)"];
    mkopt ["-orf";"--overwrite-rif-file"]  ~cat: More
          (Arg.Unit (fun () -> overwrite_output := true))
          ["Overwrite \""^ !output_file
           ^"\" if it exists without trying to invent a new name"];
    mkopt ["-dsoe";"--dont-stop-on-oracle-error"]  ~cat: More 
          (Arg.Unit (fun _ -> dont_stop_on_oracle_error := true))
          ["Do not stop if one oracle is violated"];
    mkopt ["-cv";"--cov-file"]  ~cat: More ~arg:" <string>"
          (Arg.String (fun s -> cov_file := s))
           ["file name coverage info will be put into"];
    mkopt  ["-rcv";"--reset-cov-file"]  ~cat: More
           (Arg.Unit (fun _ -> reset_cov_file := true))
           ["Reset coverage file data"];
    mkopt  ["-no-log";"--no-log"] ~cat: More (Arg.Unit (fun _ -> log := false))
           ["Do no generate a rdbg.log file and output everything on stdout"];
    mkopt ["-e";"-emacs";"--emacs"]  ~cat: (if !lurette then Lurette else Main)
          (Arg.Unit (fun () -> emacs_mode := true))
          ["Use emacs to open appropriate buffers while debugging" ];
    mkopt [ "-v";"-verbose";"--verbose"] ~cat: More
          (Arg.Unit (fun _ -> (verbose := true)))
          ["Echo the commands sent to rdbg-top"];
    mkopt ["--debug"]  ~cat: More
          (Arg.Unit (fun () -> drdbg := true))
          ["set on a verbose mode to (try to) understand what's going on" ];
    mkopt ["--ocaml-cmd";"-cmd"]  ~cat: More
          (Arg.String (fun str -> cmd := !cmd^str^"\n"))
          ["Ocaml code to run before the session starts" ];
    mkopt ["-go"]  ~cat: More
          (Arg.Unit (fun () -> go := true))
          ["just run, without session prompt" ];
    mkopt ["-h";"-help";"--help"]
          (Arg.Unit help)
          ["Display this help message"];
    mkopt ["-m";"-more";"--more"]
          (Arg.Unit more)
          ["Display more options"];
    mkopt ["--ocaml-version"] ~cat: (if !lurette then Lurette else More)
          (Arg.Unit(fun () -> print_string (Sys.ocaml_version); flush stdout; exit 0))
          ["Print the current ocaml version it was compiled with and exit"];
    mkopt ["-version";"--version"]  ~cat: More
          (Arg.Unit(pversion))
          ["Print the current version and exit"]
