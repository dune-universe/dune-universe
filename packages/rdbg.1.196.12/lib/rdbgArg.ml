(* Time-stamp: <modified the 07/07/2021 (at 16:08) by Erwan Jahier> *)

let cmxs_or_cma = 
  if Dynlink.is_native then "cmxs" else "cma"

let usage_msg = ("usage: " ^Sys.argv.(0) ^ " [<option>] ["^cmxs_or_cma^" file]
use --help to see the available options.
" )
let print_usage () = Printf.printf "%s\n" usage_msg; flush stdout

type verbose_level = int
type program_kind = SUT | Env | Oracle 

type reactive_program = 
  | Stdio of string
  | StdioInit of string 
  | Sock of string * int
  | SockInit of string * int
  | Ocaml of RdbgPlugin.t 

let program_kind_of_string = function
  | "sut" -> SUT
  | "oracle" -> Oracle
  | "env" -> Env
  | _s -> assert false

let program_kind_to_string = function
  | SUT -> "sut"
  | Oracle ->  "oracle"
  | Env -> "env"


let reactive_program_to_string = function
  | Stdio(cmd) -> cmd
  | StdioInit(cmd) -> "init:"^cmd
  | Sock(addr,port) -> Printf.sprintf "%s:%i" addr port
  | SockInit(addr,port) -> Printf.sprintf "init:%s:%i" addr port
  | Ocaml(plugin) -> plugin.RdbgPlugin.id

type t = {

  mutable _args : (string * Arg.spec * string) list; (* classical Arg option tab used by Arg.parse *)
  mutable _user_man  : (string * string list) list; 
  mutable _hidden_man: (string * string list) list; 

  mutable suts : reactive_program list ;
  mutable envs : reactive_program list ;
  mutable oracles : reactive_program list ;

  mutable missing_vars_at_the_end : bool;
  mutable step_nb : int;

  mutable luciole_mode : bool;
  mutable sasa_mode : bool;
  mutable salut_mode : bool;
  mutable delay_env_outputs : bool;  (* clutch for comon *)

  mutable display_sim2chro : bool;
  mutable display_gnuplot : bool;
  mutable precision : int ;
  mutable verbose : verbose_level ;

(* RIF output control *)
  mutable display_local_var : bool ;
  mutable show_step : bool ;

  mutable output : string ;
  mutable no_rif : bool ;
  mutable overwrite_output : bool;

  mutable prompt : string option ;

  mutable go : bool ; (* obselete ? *)
  mutable log : bool;
  mutable tmp_dir : string;
  mutable tmp_dir_provided : string option;

(* to call rdbg via a socket  *)
  mutable socket_inet_addr : string option; (* if None, we use stdin/stdout *)
  mutable socket_port : int option;
  mutable socket_err_port : int option;

  mutable debug_rdbg : bool;

  mutable rdbg : bool; (* if false, well, basically, it's lurette! *)
  
  mutable cov_file : string;
  mutable reset_cov_file : bool;
  mutable stop_on_oracle_error : bool;

(* 
   I am using references for that in order to be able to replace them
   by sockets if necessary (i.e., once the sockets are connected) *)
  mutable icr : in_channel;
  mutable ocr : out_channel;
  mutable ecr : out_channel;

  (* unknown args*)
  mutable _others : string list;
  mutable _margin : int;

}


let (make_args : unit -> t) = 
  fun () -> 
    {
      _args = [];        
      _user_man  = [];   
      _hidden_man  = []; 
      suts = [];
      oracles= [];
      envs = [];
      missing_vars_at_the_end = false;
      step_nb = 100;
      luciole_mode = false;
      salut_mode = false;
      sasa_mode = false;
      delay_env_outputs = false;
      show_step = false ;
      display_local_var = false ;
      display_sim2chro = false ;
      display_gnuplot = false ;
      precision = 2;
      verbose = 0 ;
      output = "rdbg.rif" ;
      no_rif = false;
      overwrite_output = false;
      prompt = None ;

      tmp_dir = ".";
      tmp_dir_provided = None;

      go = false ;

      log = false;
      socket_inet_addr = None;
      socket_port = None;
      socket_err_port = None;
      debug_rdbg = false;
      rdbg = false;

      cov_file = "lurette.cov";
      reset_cov_file = false;
      stop_on_oracle_error = false;
      
      ocr =  stdout;
      icr =  stdin;
      ecr =  stderr;

      _others = [];
      _margin = 12;
    }
let (args : t) = make_args ()



let pspec os  (c, ml) = (
  let (m1, oth) = match ml with
	 |	h::t -> (h,t)
	 |	_ -> ("",[])
  in
  let t2 = String.make args._margin ' ' in
  let cl = String.length c in
  let t1 = if (cl < args._margin ) then
	 String.make (args._margin - cl) ' '
  else
	 "\n"^t2
  in
	 Printf.fprintf os "%s%s%s" c t1 m1;
	 List.iter (function x -> Printf.fprintf os "\n%s%s" t2 x) oth ;
	 Printf.fprintf os "\n" ;
)

let options oc = (
	let l = List.rev args._user_man in
(*    let str = Arg.usage_string args._args usage_msg in *)
(* 	output_string oc  str; flush oc; *)
	List.iter (pspec oc) l
)
let more_options oc = (
	let l = List.rev (args._hidden_man) in
	List.iter (pspec oc) l
)

let myexit i = 
  if args.rdbg then failwith "error in rdbg" else exit i

let unexpected s = (
	prerr_string ("unexpected argument \""^s^"\"");
	prerr_newline ();
	myexit 1
)
let file_notfound f = (
	prerr_string ("File not found: \""^f^"\"");
	prerr_newline ();
	myexit 1
)

let (parse_stdio_string : string -> reactive_program) =
  fun str -> 
    (*     try *)
    let l = (Str.split (Str.regexp ":") str) in
    let rp = 
      match l with
        | [cmd] -> Stdio(cmd)
        | ["init"; cmd] -> StdioInit(cmd)
        | _ -> failwith ("*** Error: in --*-stdio arguments: \""^str^"\"\n")
    in
    rp

let my_int_of_string port =
  try int_of_string port
  with _ -> failwith ("*** Error: in --*-socket arguments: \""^port^
                        "\" should be an int\n")
let (parse_sock_string : string -> reactive_program) =
  fun str -> 
    (*     try *)
    let l = (Str.split (Str.regexp ":") str) in
    let rp = 
      match l with
        | [        addr;port] -> Sock(addr,my_int_of_string port)
        | ["init"; addr;port] -> SockInit(addr,my_int_of_string port)
        | _ -> failwith ("*** Error: in --*-socket arguments: \""^str^"\"\n")
    in
    rp

(************************************************************************)
let (mkopt : t -> string list -> ?hide:bool -> ?arg:string -> Arg.spec -> string list -> unit) =
  fun opt ol ?(hide=false) ?(arg="") se ml ->
    let treto o = opt._args <- (o, se, "")::opt._args in
	   List.iter treto ol ;
	   let col1 = (String.concat ", " ol)^arg in
	     if hide 
        then opt._hidden_man <- (col1, ml)::opt._hidden_man
	     else opt._user_man   <- (col1, ml)::opt._user_man


(*** User Options Tab **)
let (mkoptab : t -> unit) = 
  fun opt ->  
  let nl = "\n"^(String.make args._margin ' ') in
    (                 
    mkopt opt  ["--sut-stdio"] ~arg:" \"{init:}sys call\" "
      (Arg.String(fun s -> args.suts <- args.suts@[parse_stdio_string s]))
      ["the sut read/writes its I/O on stdin/stdout in RIF." ^ nl ^
          " if 'init' is present, the process first" ^nl ^
          " read values to initialiase its I/O values sequence."
      ];

    mkopt opt  ["--env-stdio"] ~arg:" \"{init:}sys call\" "
      (Arg.String(fun s -> args.envs <- args.envs@[parse_stdio_string s]))
      ["ditto for env"];

    mkopt opt  ["--oracle-stdio"] ~arg:" \"{init:}sys call\" "
      (Arg.String(fun s -> args.oracles <- args.oracles@[parse_stdio_string s]))
      ["ditto for oracle"];

    mkopt opt  ["--sut-socket"] ~arg:" \"{init:}sockadr:port\" "
      (Arg.String(fun s -> args.suts <- args.suts@[parse_sock_string s]))
      ["the sut read/writes its I/O on a socket in RIF." ^ nl ^
          " if 'init' is present, the process first" ^nl ^
          " read values to initialiase its I/O values sequence."
      ];

    mkopt opt  ["--env-socket"] ~arg:" \"{init:}sockadr:port\" "
      (Arg.String(fun s -> args.envs <- args.envs@[parse_sock_string s]))
      ["ditto for env"];

    mkopt opt  ["--oracle-socket"] ~arg:" \"{init:}sockadr:port\" "
      (Arg.String(fun s -> args.oracles <- args.oracles@[parse_sock_string s]))
      ["ditto for oracle"];

    mkopt opt ["--test-length";"-l"] ~arg:" <int>"
      (Arg.Int (fun i -> args.step_nb <- i))
      ["Number of steps to be done (" ^ (string_of_int args.step_nb) ^ " by default).\n"];

    mkopt opt ["--output";"-o"] ~arg:" <string>"
      (Arg.String (fun s -> args.output <- s))
      ["Set the output file name (currently,  \"" ^ args.output ^ "\")."];

    mkopt opt  ~hide:true
      ["--precision";"-p"] ~arg:" <int>" (Arg.Int (fun i -> args.precision <- i))
      ["number of digit after the dot used for floating points."];

    mkopt opt ["--lurette";"-lurette"] (Arg.Unit (fun () -> args.rdbg <- false))
      ["Remove debugging stuff and thus behaves as lurette"];

    mkopt opt ~hide:true ["--debug-me"]
      (Arg.Unit (fun () -> args.debug_rdbg <- true))
      ["debug rdbg mode (to debug rdbg)\n"];

    mkopt opt ~hide:true ["--no-rif";"-norif"]
      (Arg.Unit (fun () -> args.no_rif <- true; args.display_sim2chro <- false))
      ["Do not print data in the rif file (useful when there are too many variables)";
       "nb: inhibits  --sim2chro (which is fine when there are too many variables)"
      ];

    mkopt opt ~hide:true ["--overwrite-rif-file";"-orf"]
      (Arg.Unit (fun () -> args.overwrite_output <- true))
      ["Overwrite \""^args.output^"\" if it exists without trying to invent a new name"];

    (*
    mkopt opt  ~hide:true ["--socket-inet-addr"] ~arg:" <string>"
      (Arg.String (fun i -> args.socket_inet_addr <- Some i))
      ["Set the socket address"];

    mkopt opt  ~hide:true ["--socket-io-port"] ~arg:" <int>"
      (Arg.Int (fun i -> args.socket_port <- Some i))
      ["Set the socket io port"];

    mkopt opt  ~hide:true ["--socket-err-port"] ~arg:" <int>"
      (Arg.Int (fun i -> args.socket_err_port <- Some i))
      ["Set the socket error port"];
     *)
    mkopt opt ~hide:true ["--stop-on-oracle-error"]
      (Arg.Unit (fun _ -> args.stop_on_oracle_error <- true))
      ["Stop if one oracle is violated"];

    mkopt opt ~hide:true ["--delay-env-outputs"]
      (Arg.Unit (fun _ -> args.delay_env_outputs <- true))
      ["Delay the outputs of the environements before transmitting them to the oracles"];

    mkopt opt  ~hide:true ["--log";"-log"] (Arg.Unit (fun _ -> args.log <- true))
      ["Redirect stdout to a log file (rdbg_stdout.log)"];

    mkopt opt ["--gnuplot";"-gp"]
      (Arg.Unit (fun () -> args.display_gnuplot <- true))
      ["Call gnuplot to display data"];

    mkopt opt ["--sim2chro"]
      (Arg.Unit (fun () -> args.display_sim2chro <- true))
      ["Call sim2chro to display data"];

    mkopt opt ["--luciole"]
      (Arg.Unit (fun () -> args.luciole_mode <- true))
      ["Use luciole (instead of stdin/stdout) when some inputs are missing"];

    mkopt opt ~hide:true ["--salut"]
      (Arg.Unit (fun () -> args.salut_mode <- true))
      ["Tell rdbg that one RP comes from salut"];

    mkopt opt ~hide:true ["--sasa"]
      (Arg.Unit (fun () -> args.sasa_mode <- true))
      ["Tell rdbg that one RP comes from sasa"];

    mkopt opt ~hide:true ["--ocaml-version"]
      (Arg.Unit (fun _ -> (print_string (Sys.ocaml_version) ; flush stdout; exit 0)))
      ["Display the version ocaml version lurette was compiled with and exit."];

    mkopt opt ["--version";"-version";"-v"]
      (Arg.Unit (fun _ -> (print_string (RdbgVersion.str^"-"^RdbgVersion.sha) ; exit 0)))
      ["Display the version and exit"];

    mkopt opt ~hide:true ["--cov-file"] ~arg:" <string>"
      (Arg.String (fun s -> args.cov_file <- s))
      ["file name coverage info will be put into"];

    mkopt opt ~hide:true ["--reset-cov-file"]
      (Arg.Unit (fun _ -> args.reset_cov_file <- true))
      ["Reset coverage file data"];

    mkopt opt  ["--verbose";"-vl"] ~arg:" <int>"
      (Arg.Int (fun i -> args.verbose <- i))   ["Set the verbose level"];

    mkopt opt ["--help";"-help"; "-h"] (Arg.Unit (fun _ -> print_usage();options stdout; exit 0))
      ["Display main options"];

    mkopt opt ["--more";"-m"] (Arg.Unit (fun () -> more_options stdout; exit 0)) 
      ["Display more options"]
  )


(* all unrecognized options are accumulated *)
let (add_other : t -> string -> unit) =
  fun args s -> 
    args._others <- s::args._others

let current = ref 0;;
let first_line b = (
	try (
		let f = String.index b '\n' in
		String.sub b 0 f
	) with Not_found -> b
)


let parse argv = (
  let save_current = !current in
  try (
    mkoptab args;
	 Arg.parse_argv ~current:current argv args._args (add_other args) usage_msg;
    (List.iter 
       (fun f -> 
         if (String.sub f 0 1 = "-") then
           unexpected f 
         else if not (Sys.file_exists f) then
           file_notfound f
         else ()
       ) 
       args._others);
    current := save_current;
  ) with
	   (* only 1rst line is interesting ! *)
	 | Arg.Bad  msg -> 
      Printf.fprintf stderr "*** Error when calling '%s': %s\n%s\n" (Sys.argv.(0))
        (first_line msg) usage_msg; exit 2; 
	 | Arg.Help msg -> 
      Printf.fprintf stdout "%s\n%s\n" msg usage_msg; 
      options stdout; exit 0
)

 
