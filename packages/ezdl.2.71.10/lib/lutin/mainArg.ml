 
let version =
Printf.sprintf "%s (\"%s\")\n"
	Version.str
	Version.sha


let tool_name = "lutin"
let usage_msg =  "usage: "^tool_name^" [options] <file> | "^tool_name^" -help"

type gen_mode = Simu | GenLuc | Ocaml | Cstubs 

type t = {
  mutable _opts : (string * Arg.spec * string) list; (* classical Arg option tab used by Arg.parse *)
  mutable _user_man  : (string * string list) list; (* ad hoc tab for pretty prtting usage *)
  mutable _hidden_man: (string * string list) list; (* ad hoc tab for pretty prtting usage *) 
  mutable _test_lex : bool;
  mutable _test_parse: bool;
  mutable _test_check: bool;
  mutable _test_expand: bool;
  mutable _test_auto: bool;
  mutable _test_exe : bool;
  mutable _libs : string list;
  mutable _gen_mode : gen_mode ;
  mutable _step_mode : Lucky.step_mode;
  mutable _load_mem : bool;
  mutable _seed : int option;
  mutable _reset_seed : bool;
  mutable _compute_volume : bool;
  mutable _max_steps : int option;
  mutable _show_locals :bool;
  mutable _see_all_options: bool;
  mutable _others : string list;
  mutable _margin : int;
  mutable _infile : string list;
  mutable _main_node: string;
  mutable _outpipe: bool;
  mutable _outfile : string option;
  mutable _riffile : string option;
  mutable _only_outputs: bool;
  mutable _call_gnuplot: bool;
  mutable _run:bool;
  mutable _boot:bool;
  mutable _event_nb:int;
}

let (make_opt : unit -> t) = 
  fun () -> 
  {
   _opts = [];        
   _user_man  = [];   
   _hidden_man  = []; 
   _test_lex = false;
   _test_parse = false;
   _test_check = false;
   _test_expand= false;
   _test_auto = false;
   _test_exe  = true;
   _libs = [];
   _gen_mode  = Simu ;
   _step_mode = Lucky.StepInside;
   _load_mem  = false;
   _seed = None;
   _reset_seed = false;
   _compute_volume = false;
   _max_steps = None;
   _show_locals = false;
   _see_all_options = false;
   _others = [];
   _margin = 12;
   _infile = [];
   _main_node = "";
   _outpipe = false;
   _outfile = None;
   _riffile = None;
   _only_outputs = false;
   _call_gnuplot = false;
   _run=true;
   _boot=false;
   _event_nb=0;
}


let set_libs opt libs =  
    { opt with _libs = libs }

let libs opt = match opt._libs with [] -> None | l -> Some l
let set_precision = Util.change_precision 

(*******************************************************************************)
(* seeds stuff *)
let set_seed opt s = 
  (match s with
  | Some i ->
     Printf.fprintf stderr "The random engine seed is set to %i\n" i;
     Random.init i;
     flush stderr;
  | None -> ());
  opt._seed <- s

let seed_file_name opt =
  Printf.sprintf "%s-%s.seed" (String.concat "-" opt._infile) opt._main_node 

(* for --replay *)
let reset_the_seed_to_last opt =
  let f = seed_file_name opt in
  try
    let ic = open_in f in
    let seed = int_of_string (input_line ic) in
    opt._seed <- Some seed;
    true
  with _ ->
    Printf.eprintf "W: cannot recover the seed in %s\n" f;
    flush stderr;
    false
   
              
let rec seed opt = match opt._seed with
  | Some i -> i
  | None ->
     (* No seed is set: 
         - in -replay mode, we first try to read the seed in the seed file
         - otherwise, we create a random seed and save if into opt, and 
           into a file for -replay *)
     if opt._reset_seed && reset_the_seed_to_last opt then (seed opt) else
       let seed = Random.self_init (); Random.int 1073741823 in
       let seed_file = seed_file_name opt in
       let oc = open_out seed_file in
       Printf.fprintf oc "%d\n%s\n" seed (Util.entete "#" "");
       flush oc;
       close_out oc;
       opt._seed <- Some seed;
       seed


(* Emulate the event number when not run under rdbg so that we are able
   to tell at which event we reached a deadlock
 *)
(* let event_nb = ref 0  *)
let event_incr opt = 
  opt._event_nb <- opt._event_nb + 1;
  ()
    

let (get_event_nb : t -> int) = 
  fun opt -> 
  opt._event_nb

(* all unrecognized options are accumulated *)
let (add_other : t -> string -> unit) =
  fun opt s -> 
    opt._others <- s::opt._others

let infile opt = opt._infile
let test_lex opt = opt._test_lex
let test_parse opt = opt._test_parse
let test_check opt = opt._test_check
let test_expand opt = opt._test_expand
let test_auto opt = opt._test_auto
let main_node opt = opt._main_node
let test_exe opt = opt._test_exe
let max_steps opt = opt._max_steps
let gen_mode opt = opt._gen_mode
let load_mem opt = opt._load_mem
let outfile opt = opt._outfile
let outpipe opt = opt._outpipe
let riffile opt = opt._riffile
let call_gnuplot opt = opt._call_gnuplot
let only_outputs opt = opt._only_outputs
let show_locals opt = opt._show_locals
let set_show_locals opt b = opt._show_locals <- b
let see_all_options opt = opt._see_all_options
let step_mode opt = opt._step_mode
let set_step_mode opt sm = opt._step_mode <- sm
let compute_volume opt = opt._compute_volume
let set_compute_volume opt b = opt._compute_volume <- b
let run opt = opt._run
let boot opt = opt._boot


let pspec os opt (c, ml) = (
  let (m1, oth) = match ml with
	 |	h::t -> (h,t)
	 |	_ -> ("",[])
  in
  let t2 = String.make opt._margin ' ' in
  let cl = String.length c in
  let t1 = if (cl < opt._margin ) then
	 String.make (opt._margin - cl) ' '
  else
	 "\n"^t2
  in
	 Printf.fprintf os "%s%s%s" c t1 m1;
	 List.iter (function x -> Printf.fprintf os "\n%s%s" t2 x) oth ;
	 Printf.fprintf os "\n" ;
)

let usage os opt = (
	let l = List.rev opt._user_man in
	Printf.fprintf os "%s\n\n" usage_msg;
	List.iter (pspec os opt) l
)
let help opt ()= (
	usage stdout opt;
	exit 0
)
let full_usage os opt = (
	Printf.fprintf os "%s\n" usage_msg;
	let l = List.rev (opt._hidden_man) in
	List.iter (pspec os opt) l
)
let full_help opt ()= (
	full_usage stdout opt;
	exit 0
)

let unexpected s opt = (
	prerr_string ("unexpected argument \""^s^"\"");
	prerr_newline ();
	usage stderr opt;
	exit 1
)
let file_notfound f opt = (
	prerr_string ("File not found: \""^f^"\"");
	prerr_newline ();
	usage stderr opt;
	exit 1
)

let debug_options_list =    
  [
    "CkType";
    "Expand";
    "LutExe";
    "Run";
    "LutProg";
    "LucProg";
    "AutoGen"; "AutoExplore"; "Guard"]

let (mkopt : t -> string list -> ?hide:bool -> ?arg:string -> Arg.spec ->
             string list -> unit) =
  fun opt ol ?(hide=false) ?(arg="") se ml ->
    let treto o = opt._opts <- (o, se, "")::opt._opts in
	   List.iter treto ol ;
	   let col1 = (String.concat ", " ol)^arg in
	     if hide 
        then opt._hidden_man <- (col1, ml)::opt._hidden_man
	     else opt._user_man   <- (col1, ml)::opt._user_man
          (*
	         let tabs = String.make (col - (String.length o) - (String.length arg)) ' ' in
	       (* (o, se, arg^tabs^m) *)
	         (o, se, arg^"\n     "^m)
          *)

(*** USER OPTIONS TAB **)
let (mkoptab : t -> unit) = 
  fun opt ->  (                 
    mkopt opt ["-n";"-m";"-node";"-main"] ~arg:" <string>"
      (Arg.String(function s -> 
        Luc2c.option.Luc2c.main_node <- s; opt._main_node <- s))
      ["Set the main node"]
    ;
    mkopt opt ~hide:true
      ["-check"]
      (Arg.Unit(function _ -> opt._run<-false))
      ["check the program is correct and exit"]
    ;
    mkopt opt
      ["--version";"-version"]
      (Arg.Unit(function _ -> print_string version; flush stdout;exit 0))
      ["Print the current version and exit"]
    ;
    mkopt opt ~hide:true
      ["--ocaml-version"]
      (Arg.Unit(fun () -> print_string (Sys.ocaml_version); flush stdout; exit 0))
      ["Print the current ocaml version it was compiled with and exit"]
    ;
    (* verbose *)
    mkopt opt
      ["-v"]
      (Arg.Unit(function _ -> Verbose.on () ))
      ["Set the verbose level to 1"]
    ;
    mkopt opt
      ["-vl"]
      ~arg:" <int>"
      (Arg.Int(function i -> Verbose.set i))
      ["Set the verbose level"]
    ;
    (* ---- COMPILE OPTIONS: broken...*)
(*     mkopt opt *)
(*       ["-luc"] *)
(*       (Arg.Unit(function _ -> opt._gen_mode <- GenLuc ; ())) *)
(*       ["Generate a lucky program"] *)
(*     ; *)
(*     mkopt opt (* XXX Merge it with -luc !!! *) *)
(*       ["-o"] *)
(*       ~arg:" <string>" *)
(*       (Arg.String(function s -> opt._outfile <- Some s)) *)
(*       ["with -luc: write to specified file (default is infile_main.luc)"] *)
(*     ; *)
(*     mkopt opt *)
(*       ["-os"] *)
(*       (Arg.Unit(function s -> opt._outpipe <- true)) *)
(*       ["write to stdout"] *)
(*     ; *)
    mkopt opt
      ["-gnuplot";"-gp"]
      (Arg.Unit(function _s -> 
        opt._call_gnuplot <- true; 
        (match opt._riffile with
          | None -> opt._riffile <- Some "_lutin.rif" 
          | Some _ -> ())
       ))
      ["call gnuplot-rif to display data (type 'a' in the gnuplot window to refresh it)."]
    ;
      mkopt opt
      ["-rif";"-quiet";"-q";"-only-outputs"]
      (Arg.Unit(function _s -> opt._only_outputs <- true))
      ["display only outputs on stdout (i.e., behave as a rif input file)"]
    ;
    mkopt opt
      ["-o"]
      ~arg:" <string>"
      (Arg.String(function s ->
                           opt._outfile <- Some s;
                           (*let news = if not (Sys.file_exists s)  *)
                           (*   then s else *)
                           (*     let rec find_free_name b i = *)
                           (*       let f = Printf.sprintf "%s-%d.rif" b i in *)
                           (*       if Sys.file_exists f then *)
                           (*        find_free_name b (i+1) *)
                           (*       else *)
                           (* f *)
                           (*     in *)
                           (* find_free_name (Filename.chop_extension s) 1 *)
                           (* in *)
                           if  (Filename.check_suffix s ".rif") then (
          opt._riffile <- Some s;
          Luc2c.option.Luc2c.rif <- Some s
       )))
      ["output file name"]
    ;

    mkopt opt
      ["-L"; "-lib"]
      ~arg:" <string>"
      (Arg.String(function s -> opt._libs <- s::opt._libs))
      ["Add a dynamic library where external code will be searched in"]
    ;
    (* ---- SIMU/SOLVER OPTIONS *)
    (* simu *)

    mkopt opt
      ["--replay";"-r"]
      (Arg.Unit(fun () -> opt._reset_seed <- true))
      ["Use the last generated seed to replay the last run"]
    ;
      mkopt opt
      ["-seed"]
      ~arg:" <int>"
      (Arg.Int(function i -> opt._seed <- Some i ; ()))
      ["Set a seed for the pseudo-random generator (wins over --replay)"]
    ;
    mkopt opt
      ["-boot"]
      (Arg.Unit(function _ -> opt._boot<-true))
      ["Perform ther first step without reading inputs"]
    ;
    mkopt opt
      ["--max-steps";"-l"]
      ~arg:" <int>"
      (Arg.Int(function i -> opt._max_steps <- Some i ; ()))
      ["Set a maximum number of simulation steps to perform"]
    ;
    mkopt opt
      ["--step-inside"; "-si"]
      (Arg.Unit(function _ -> opt._step_mode <- Lucky.StepInside))
      ["Draw inside the convex hull of solutions (default)"]
    ;
    mkopt opt
      ["--step-edges"; "-se"]
      (Arg.Unit(function _ -> opt._step_mode <- Lucky.StepEdges))
      (* "draw inside the convex hull of solutions, but a little bit more at edges and vertices" *)
      ["Draw a little bit more at edges and vertices"]
    ;
    mkopt opt
      ["--step-vertices"; "-sv"]
      (Arg.Unit(function _ -> opt._step_mode <- Lucky.StepVertices))
      ["Draw among the vertices of the convex hull of solutions"]
    ;
    mkopt opt ~hide:true
      ["-fair"; "--compute-poly-volume"]
      (Arg.Unit (fun () -> opt._compute_volume <- true; ))
      [ "[Currently not supported anymore; if you need it, please ask] ";
        "Compute polyhedron volume before drawing";
        "More fair, but more expensive"
      ];
    mkopt opt
      ["-precision";"-p"]
      (Arg.Int(fun i -> Util.change_precision i))
      ~arg:" <int>"
      [ "Set the precision used for converting float to rational (default: "^
          (string_of_int !Util.precision)^")"
      ]; 
    mkopt opt
      ["-locals";"-loc"]
      (Arg.Unit (fun () -> opt._show_locals <- true))
      [ "Show local variables in the generated data."
      ];

    mkopt opt
      ["--ocaml";"-ocaml"]
      (Arg.Unit(function _s -> opt._gen_mode <- Ocaml))
      ["Generate ocaml glue code that makes it possible to call the lutin interpreter ";
       "from ocaml with the current set of arguments."]
    ;

    (* ---- luc2c OPTIONS *)
    mkopt opt ~hide:true
      ["--2c-4c"]
      (Arg.Unit(fun () -> 
        opt._gen_mode <- Cstubs;
        Luc2c.option.Luc2c.gen_mode <- Luc2c.Nop))
      ["Generate C code to be called from C "];

    mkopt opt ~hide:true
      ["--2c-4c-socks"]
      ~arg:" <string> (the machine IP)"
      (Arg.String(function str ->  
        opt._gen_mode <- Cstubs;
        Luc2c.option.Luc2c.use_sockets <- true;
        Luc2c.option.Luc2c.sock_addr <- str
       ))
      ["The same as --2c-4c, but using sockets."]
    ;
    mkopt opt ~hide:true
      ["---sock-port";"-port"]
      ~arg:" <int> "
      (Arg.Int(function i -> Luc2c.option.Luc2c.sock_port <- i))
      ["to set the port number; meaningful only if --2c-4c-socks is used."]
    ;

    mkopt opt ~hide:true
      ["--2c-4lustre"]
      ~arg:" <string>"
      (Arg.String(fun str -> 
        opt._gen_mode <- Cstubs;
        Luc2c.option.Luc2c.gen_mode <- Luc2c.Lustre;
        Luc2c.option.Luc2c.calling_module_name <- str))
      ["Generate C code to be called from Lustre V4 "];

    mkopt opt ~hide:true
      ["--2c-4scade"]
      ~arg:" <string>"
      (Arg.String(fun str -> 
        opt._gen_mode <- Cstubs;
        Luc2c.option.Luc2c.gen_mode <- Luc2c.Scade;
        Luc2c.option.Luc2c.calling_module_name <- str))
      ["Generate C code to be called from Scade "];

    mkopt opt  ~hide:true
      ["-luciole"; "--2c-4luciole"]
      (Arg.Unit(fun _ -> 
        opt._gen_mode <- Cstubs;
        Luc2c.option.Luc2c.gen_mode <- Luc2c.Luciole))
      ["not working anymore: use luciole-rif instead."];

    mkopt opt  ~hide:true
      ["--2c-4alice"]
      ~arg:" <string>"
      (Arg.String(fun str -> 
        opt._gen_mode <- Cstubs;
        Luc2c.option.Luc2c.gen_mode <- Luc2c.Alices;
        Luc2c.option.Luc2c.calling_module_name <- str))
      ["Generate C and C++ code to be called from Alice"];

    mkopt opt ~hide:true
      ["--2c-output-dir"]
      ~arg:" <string>"
      (Arg.String(fun str -> Luc2c.option.Luc2c.output_dir <- str))
      ["Set the directory where C files are generated"];

    mkopt opt 
      ["-h";"-help"; "--help"]
      (Arg.Unit (help opt))
      ["Display this message"]
    ;

    (* to show Hidden opt *)
    mkopt opt
      ["-more"]
      (* (Arg.Unit(fun _ -> opt._see_all_options <- true)) *)
      (Arg.Unit (full_help opt))
      ["Show hidden options (for dev purposes)"];
    (* HIDDEN *)


    (* test lexical *)
    mkopt opt ~hide:true
      ["-tlex"]
      (Arg.Unit(function _ -> opt._gen_mode <- GenLuc ; opt._test_lex <- true ; ()))
      ["Test the lexical analysis"]
    ;
    (* test syntaxique *)
    mkopt opt ~hide:true
      ["-tparse"]
      (Arg.Unit(function _ -> opt._gen_mode <- GenLuc ; opt._test_parse <- true ; ()))
      ["Test the syntactic analysis"]
    ;
    (* test type-checking *)
    mkopt opt ~hide:true
      ["-tcheck"]
      (Arg.Unit(function _ -> opt._gen_mode <- GenLuc ; opt._test_check <- true ; ()))
      ["Test the type/binding checking"]
    ;
    (* test expansion *)
    mkopt opt ~hide:true
      ["-texpand"]
      (Arg.Unit(function _ -> opt._gen_mode <- GenLuc ; opt._test_expand <- true ; ()))
      ["Test the expansion"]
    ;
    (* test autogen *)
    mkopt opt ~hide:true
      ["-tauto"]
      (Arg.Unit(function _ -> opt._gen_mode <- GenLuc ; opt._test_auto <- true ; ()))
      ["Test the automaton generation"]
    ;
    (* use ths old algo (ie, not the LutExe based simu *)
    mkopt opt ~hide:true
      ["-old"]
      (Arg.Unit(function _ -> opt._gen_mode <- Simu; opt._test_exe <- false ; ()))
      ["Simulation with an old algo."]
    ;

    (* paranoid/debug *)
    mkopt opt ~hide:true
      ["-paranoid"]
      (Arg.Unit(function _ -> Utils.set_paranoid ()))
      ["Set on the paranoid mode (slower)"]
    ;

    mkopt opt ~hide:true
      ["-prof"]
      (Arg.Unit(function _ -> Utils.set_prof ()))
      ["Set on the profile mode"]
    ;
    (* misc degub flag *)
    mkopt opt ~hide:true
      ["-debug"; "-dbg"]	

      (Arg.Symbol (debug_options_list, 
                   (function s -> let x = Verbose.get_flag s in Verbose.set_flag x)))
      
      [ "<dbg_flag>"; 
        "Possible dbg_flag are: " ^(String.concat ", " debug_options_list) ]
  )


let first_line b = (
	try (
		let f = String.index b '\n' in
		String.sub b 0 f
	) with Not_found -> b
)


let current = ref 0;;

(* La ``méthode'' principale *)
let parse argv = (
  let opt = make_opt() in
  let save_current = !current in
  try (
    mkoptab opt;
	 Arg.parse_argv ~current:current argv opt._opts (add_other opt) usage_msg;
    (List.iter 
       (fun f -> 
          if f="" then () else
          if (String.sub f 0 1 = "-") then
            unexpected f opt
          else if not (Sys.file_exists f) then
            file_notfound f opt
          else ()
       ) 
       opt._others);
    opt._infile <- (List.rev opt._others);
    current := save_current;
    opt
  ) with
  (* only 1rst line is interesting ! *)
  | Arg.Bad  msg -> Printf.fprintf stderr "%s\n" (first_line msg);
    usage stderr opt; exit 2; 
  | Arg.Help msg ->
    Printf.fprintf stderr "%s\n" (first_line msg);
    help opt ();
)

