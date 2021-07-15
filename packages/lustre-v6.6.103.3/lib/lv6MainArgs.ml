(* Time-stamp: <modified the 13/07/2021 (at 11:22) by Erwan Jahier> *)
(*
Le manager d'argument adapté de celui de lutin, plus joli
N.B. solution un peu batarde : les options sont stockées, comme avant, dans Global,
du coup, le fait de rendre un type "t" est une koketerie !
*)

open Arg

let tool_name = Lv6version.tool
let usage_msg =  "usage: "^tool_name^" [options] <file> | "^tool_name^" -help"

type enum_mode = AsInt | AsBool (* finishme *) | AsConst | AsEnum 
type io_transmit_mode = Stack | Heap | HeapStack
type schedul_mode = Simple | Sort | Reorder

type t = {
  mutable opts : (string * Arg.spec * string) list; (* classical Arg option tab used by Arg.parse *)
  mutable user_man  : (string * string list) list; (* ad hoc tab for pretty prtting usage *)
  mutable hidden_man: (string * string list) list; (* ad hoc tab for pretty prtting usage *) 
  mutable dev_man: (string * string list) list; (* ad hoc tab for pretty prtting usage *) 
  mutable others: string list;
  mutable margin : int;
  mutable outfile :  string;
  mutable infiles :  string list;
  mutable main_node :  string;
  mutable compile_all_items : bool;
  mutable run_unit_test :  bool;
  mutable print_interface :  bool;
  mutable inline_iterator :  bool;
  mutable expand_nodes :  bool;
  mutable expand_node_call :  string list;
  mutable expand_arrays :  bool;
  mutable expand_io_type :  bool;
  mutable optim_ite : bool;
  mutable oc :  out_channel;
  mutable tlex :  bool;
  mutable exec :  bool;
  mutable gen_c:  bool;
  mutable rif  :  bool;
  mutable gen_ocaml :  bool;
  mutable launch_cc :  bool;
  mutable launch_exec :  bool;
  mutable precision : int option;
  mutable gen_lic : bool;
  mutable keep_aliases : bool;
  
}
(* Those are really too boring to be functionnal (used in all over the places) *)
type global_opt = {
  mutable dir : string;
  mutable gen_c_inline_predef :  bool;
  mutable lv4 :  bool;
  mutable kcg :  bool;
  mutable ec :  bool;
  mutable gen_autotest :  bool;
  mutable expand_enums :  enum_mode;
  mutable one_op_per_equation :  bool;
  mutable when_on_ident :  bool;
  mutable no_when_not :  bool;
  mutable no_prefix :  bool;
  mutable nonreg_test :  bool;
  mutable current_file :  string;
  mutable line_num : int;
  mutable line_start_pos : int;
  mutable soc2c_no_switch : bool;
  mutable soc2c_one_file : bool;
  mutable soc2c_inline_loops : bool;
  mutable soc2c_global_ctx : bool;
  mutable soc2c_dro : bool;
  mutable gen_wcet :  bool;
  mutable io_transmit_mode : io_transmit_mode;
  mutable schedul_mode : schedul_mode;
}
let (global_opt:global_opt) =
  {
    dir = "";
    gen_c_inline_predef = true;
    gen_autotest =  false;
    lv4 =  false;
    kcg =  false;
    ec =  false;
    one_op_per_equation =  true;
    when_on_ident =  false;
    no_when_not = false;
    no_prefix =  false;
    nonreg_test =  false;
    line_num =  1;
    line_start_pos = 0;
    current_file =  "";
    expand_enums =  AsEnum;
    soc2c_no_switch = false;
    soc2c_one_file = true;
    soc2c_inline_loops = false;
    soc2c_global_ctx = false;
    soc2c_dro = false;
    gen_wcet =  false;
    io_transmit_mode = Stack;
    schedul_mode = Simple;
  }
let (make_opt : unit -> t) = 
  fun () -> 
    {
      opts = [];        
      user_man  = [];   
      hidden_man  = []; 
      dev_man  = []; 
      others = [];
      margin = 12;
      outfile =  "";
      infiles =  [];
      main_node =  "";
      compile_all_items =  true;
      run_unit_test =  false;
      print_interface =  false;
      inline_iterator =  false;
      expand_nodes =  false;
      expand_node_call =  [];
      expand_arrays =  false;
      expand_io_type = false;
      optim_ite = false;
      oc =  stdout;(* the output channel *)
      tlex =  false;
      exec =  false;
      gen_c =  false;
      rif  = false;
      gen_ocaml =  false;
      precision = None;
      launch_cc = false;
      launch_exec = false;
      keep_aliases = false;
      gen_lic = false;
    }


(** flag 'paranoid' utile pour forcer (via le mecanisme Lv6Verbose.exe)
   des vérifs de trucs douteux ...
*)
let paranoid = (Lv6Verbose.get_flag "paranoid")

let (lexbuf_of_file_name : string -> Lexing.lexbuf) =
fun file -> 
  let inchannel = 
    Lv6Verbose.print_string ~level:1 
(*       ("Opening file " ^ (Filename.concat (Sys.getcwd ()) file) ^ "\n"); *)
      ("Opening file " ^ (file) ^ "\n");
    open_in file 
  in
    global_opt.line_num <- 1;
    global_opt.line_start_pos <- 0;
    global_opt.current_file <- file;
    Lexing.from_channel inchannel 

(* all unrecognized options are accumulated *)
let (add_other : t -> string -> unit) =
  fun opt s -> 
    opt.others <- s::opt.others

let pspec os opt (c, ml) = (
  let (m1, oth) = match ml with
	 |	h::t -> (h,t)
	 |	_ -> ("",[])
  in
  let t2 = String.make opt.margin ' ' in
  let cl = String.length c in
  let t1 = if (cl < opt.margin ) then
	 String.make (opt.margin - cl) ' '
  else
	 "\n"^t2
  in
	 Printf.fprintf os "%s%s%s" c t1 m1;
	 List.iter (function x -> Printf.fprintf os "\n%s%s" t2 x) oth ;
	 Printf.fprintf os "\n" ;
)

let usage os opt = (
	let l = List.rev opt.user_man in
	Printf.fprintf os "%s\n\n" usage_msg;
	List.iter (pspec os opt) l
)
let help opt ()= (
	usage stdout opt;
	exit 0
)
let full_usage os opt = (
	Printf.fprintf os "%s\n" usage_msg;
(* 	let l = List.rev opt.user_man in *)
(* 	List.iter (pspec os opt) l; *)
	let l = List.rev (opt.hidden_man) in
	List.iter (pspec os opt) l
)

let dev_usage os opt = (
	Printf.fprintf os "%s\n" usage_msg;
	let l = List.rev (opt.dev_man) in
	List.iter (pspec os opt) l
)


let full_help opt ()= (
	full_usage stdout opt;
	exit 0
)
let dev_help opt ()= (
	dev_usage stdout opt;
	exit 0
)

let unexpected s opt = (
	prerr_string ("Error: unexpected argument \""^s^"\"");
	prerr_newline ();
	usage stderr opt;
	exit 1
)
let file_notfound f opt = (
	prerr_string ("Error: file not found: \""^f^"\"");
	prerr_newline ();
	usage stderr opt;
	exit 1
)

type doc_level = Basic | Advanced | Dev

let (mkopt : t -> string list -> ?doc_level:doc_level -> ?arg:string -> Arg.spec -> 
     string list -> unit) =
  fun opt ol ?(doc_level=Basic) ?(arg="") se ml ->
    let treto o = opt.opts <- (o, se, "")::opt.opts in
	   List.iter treto ol ;
	   let col1 = (String.concat ", " ol)^arg in
      match doc_level with
        | Basic -> opt.user_man   <- (col1, ml)::opt.user_man
        | Advanced -> opt.hidden_man <- (col1, ml)::opt.hidden_man
        | Dev -> opt.dev_man   <- (col1, ml)::opt.dev_man
(*
  let tabs = String.make (col - (String.length o) - (String.length arg)) ' ' in
(* (o, se, arg^tabs^m) *)
  (o, se, arg^"\n     "^m)
*)
          
(* utils *)
let set_v4_options opt =
  global_opt.lv4 <- true;
  (match global_opt.expand_enums with
  | AsEnum -> global_opt.expand_enums <- AsConst; (* only override the default *)
  | AsInt | AsConst | AsBool -> ());
  opt.inline_iterator <- true;
  global_opt.when_on_ident <- true;
  opt.expand_arrays <- true;
  opt.expand_nodes <- true (* because expand_arrays is true *)


let set_ec_options opt =
  (match global_opt.expand_enums with
  | AsEnum -> global_opt.expand_enums <- AsConst; (* only override the default *)
  | AsInt | AsConst | AsBool -> ());
  set_v4_options opt;
  global_opt.ec <- true;
  global_opt.no_when_not <- true;
  global_opt.no_prefix <- true;
  opt.expand_nodes <- true;
  ()

let set_c_options opt =
  opt.gen_c <- true; 
  (match global_opt.expand_enums with
   | AsEnum -> global_opt.expand_enums <- AsInt;
   (* only override the default in this case *)
   | AsInt | AsConst | AsBool -> ());
  ()
    
(*** USER OPTIONS TAB **)
let mkoptab (opt:t) : unit = (
    mkopt opt
      ["-n";"-node"]  ~arg:" <string>"
      (Arg.String(function x -> 
         opt.main_node <- x;
         opt.compile_all_items <- false))
      ["Set the main node (all items are compiled if unset)"]
    ;
    mkopt opt  ["-o";"--output-file"]  ~arg:" <string>"
      (Arg.String(function x -> opt.outfile <- x))
      ["Set the output file name"]
    ;
    mkopt opt  ["-dir";"--directory"]  ~arg:" <string>"
      (Arg.String(function x ->
                    if not (Sys.file_exists x) then Unix.mkdir x 0o744;
                    global_opt.dir <- x))
      ["Set the directory into which output files are generated"]
    ;
    mkopt opt
      ["-exec"]
      (Arg.Unit (fun _ ->
        opt.exec <- true;
        global_opt.expand_enums <- AsInt;
      ))
      ["Interpret the program using RIF conventions for I/O (force -eei)"]
    ;
    mkopt opt
      ["-2c";"--to-c"]
      (Arg.Unit (fun _ -> set_c_options opt))
      ["Generate C code"]
    ;
    mkopt opt ~doc_level:Basic
      ["-cc"; "--compile-generated-c"]
      (Arg.Unit (fun () -> set_c_options opt; opt.launch_cc <- true))
      ["Try to compile the generated C files (force -2c)"]
    ;
    mkopt opt
      ["-2c-exec";"--to-c-execute"]
      (Arg.Unit (fun () -> set_c_options opt; opt.launch_cc <- true; opt.launch_exec <- true))
      ["Try to execute the generated C files (force -cc)."]
    ;
    mkopt opt
      ["-rif"]
      (Arg.Unit(function () -> opt.rif <- true))
      ["Behave as a rif input file (meaningless without -exec)"]
    ;
    mkopt opt ~doc_level:Advanced
      ["-ocaml"]
      (Arg.Unit(function () -> opt.gen_ocaml <- true))
      ["Generate ocaml glue code that makes it possible to call the lv6 interpreter ";
       "from ocaml with the current set of arguments (with Lv6Run.make)"]
    ;

    mkopt opt ~doc_level:Dev
      ["-knc"; "--keep-nested-calls"]
      (Arg.Unit (fun _ -> global_opt.one_op_per_equation <- false))
      ["Keep nested calls (inhibited by -en). By default, only one node ";
       "per equation is generated (don't work with -exec or -2c)"]
    ;
    mkopt opt ~doc_level:Advanced
      ["--when-on-ident"]
      (Arg.Unit (fun _ -> global_opt.when_on_ident <- true))
      ["Invent ident names so that when only operates on idents (to be able ";
       "to translate enums into ec/v4)"]
    ;
    mkopt opt ~doc_level:Advanced
      ["--no-when-not"]
      (Arg.Unit (fun _ -> global_opt.no_when_not <- true))
      ["Remove 'when not' statements (for ec) "]
    ;
    mkopt opt ~doc_level:Advanced
      ["-ei"; "--expand-iterators"]
      (Arg.Unit (fun _ -> opt.inline_iterator <- true))
      ["Expand array iterators (i.e., generate iterator-free code)"]
    ; 
    mkopt opt ~doc_level:Advanced
      ["-ee"; "--expand-enums"]
      (Arg.Unit (fun _ -> global_opt.expand_enums <- AsConst))
      ["Translate enums using extern types and consts"]
    ;
    mkopt opt ~doc_level:Advanced
      ["-eei"; "--expand-enums-as-int"]
      (Arg.Unit (fun _ -> global_opt.expand_enums <- AsInt))
      ["Translate enums into integers (to be kind with data plotters)"]
    ;
    mkopt opt ~doc_level:Dev
      ["-eeb"; "--expand-enums-as-bool"]
      (Arg.Unit (fun _ -> global_opt.expand_enums <- AsBool))
      ["Translate enums into boolean arrays (to be kind with model-checkers)"]
    ;
    mkopt opt ~doc_level:Advanced
      ["-esa"; "--expand-structs-and-arrays"]
      (Arg.Unit (fun _ ->
         opt.expand_arrays <- true;
         opt.expand_nodes <- true;
         opt.inline_iterator <- true))
      ["Expand structures and arrays (force '-ei' and '-en')"]
    ;
    mkopt opt ~doc_level:Advanced
      ["-en"; "--expand-nodes"]
      (Arg.Unit (fun _ -> opt.expand_nodes <- true; opt.inline_iterator <- true (* an iterator is a kind of node *)))
      ["Expand all node calls in the main node"]
    ;
    mkopt opt
      ["-et"; "--expand-io-type"]
      (Arg.Unit (fun _ -> opt.expand_io_type <- true))
      ["Expand structured types of the main node (impact the rif output only).";
       "Necessary to use lurette and rdbg in presence of lutin (that only ";
       "knows about basic the types int/bool/real)" ]
    ;
    mkopt opt ~doc_level:Advanced
      ["-enc"; "--expand-node-call"]
      ~arg:" <string> "
      (Arg.String (fun str ->
         opt.expand_node_call <- str::opt.expand_node_call
      ))
      ["Expand the call of the specified node (can be used for several nodes)"]
    ;
    mkopt opt ~doc_level:Advanced
      ["-oite"; "--optimize-ite"]
      (Arg.Unit (fun _ -> opt.optim_ite <- true))
      ["Transform if/then/else into merge when possible"]
    ;
    mkopt opt ~doc_level:Advanced
      ["-lv4"; "--lustre-v4"]
      (Arg.Unit (fun _ -> set_v4_options opt))
      ["deprecated: generate Lustre V4 code (force '-ei -ee -esa')"]
    ;
    mkopt opt ~doc_level:Dev
      ["-kcg"; "--generate-scade-lustre"]
      (Arg.Unit (fun _ -> 
	     (* opt.expand_arrays <- true; for problem of "#"; XXX remove me ! *) 
	     global_opt.kcg <- true
       ))
      ["Generate Lustre code that is compatible with the lustre of scade"]
    ;
    mkopt opt
      ["-ec"; "--expanded-code"]
      (Arg.Unit (fun _ -> set_ec_options opt))
      ["Generate ec programs (force '--expand-nodes --no-when-not --expand-enums --lustre-v4 --no-prefix')"]
    ;
    mkopt opt ~doc_level:Advanced
      ["-np"; "--no-prefix"]
      (Arg.Unit (fun () -> global_opt.no_prefix <- true))
      ["Do not prefix variable names by their module \n\t(beware: variable names may clash with this option)"]
    ;
    mkopt opt  ~doc_level:Advanced
      ["-2cdil";"--to-c-dont-inline-predef"]
      (Arg.Unit (fun _ ->
        global_opt.gen_c_inline_predef <- false))
      ["Do not inline predef calls when generating C code"]
    ;
    mkopt opt ~doc_level:Advanced
      ["-2cil";"--2c-inline-loop"]
      (Arg.Unit (fun () -> global_opt.soc2c_inline_loops <- true; set_c_options opt))
      ["Inline loops (that come from array iterators)"]
    ;
    mkopt opt ~doc_level:Advanced
      ["-2csf";"--2c-several-files"]
      (Arg.Unit (fun () -> global_opt.soc2c_one_file <- false; set_c_options opt))
      ["Generate one .c and one .h file per node"]
    ;
    mkopt opt ~doc_level:Advanced
      ["-2cgc";"--2c-global-ctx"]
      (Arg.Unit (fun () -> global_opt.soc2c_global_ctx <- true; set_c_options opt))
      ["Node context allocated as global variable (no \"new_ctx\" method)"]
    ;
    mkopt opt ~doc_level:Advanced
      ["-dro";"--2c-dro"]
      (Arg.Unit (fun () -> global_opt.soc2c_dro <- true; set_c_options opt;
                           global_opt.io_transmit_mode <- Heap))
      ["Generate a .dro file (for luciole)"]
    ;
    mkopt opt ~doc_level:Advanced
      ["-lic";"--gen-lic"]
      (Arg.Unit (fun () -> opt.gen_lic<-true))
      ["Generate lic"]
      ;
      mkopt opt
      ["-version"; "--version"]
      (Arg.Unit(function _ -> Printf.fprintf stdout "%s\n" Lv6version.str; exit 0))
      ["Print the current version and exit"]
    ;
    (* verbose *)
    mkopt opt
      ["-v"; "--verbose"]
      (Arg.Unit(function _ -> Lv6Verbose.on () ))
      ["Set the verbose level to 1"]
    ;
    mkopt opt
      ["-vl"]
      ~arg:" <int>"
      (Arg.Int(function i -> Lv6Verbose.set i))
      ["Set the verbose level"]
    ;
    mkopt opt
      ["-h";"-help";"--help"]
      (Arg.Unit (help opt))
      ["Display this message"]
    ;
    (* to show Hidden opt *)
    mkopt opt
      ["-more";"--advanced-options"]
      (* (Arg.Unit(fun _ -> opt.see_all_options <- true)) *)
      (Arg.Unit (full_help opt))
      ["Show more options"]
    ;
    (* to show Hidden opt *)
    mkopt opt ~doc_level:Basic
      ["-dev";"--dev-options"]
      (* (Arg.Unit(fun _ -> opt.see_all_options <- true)) *)
      (Arg.Unit (dev_help opt))
      ["Show experimental/internal options"];
    (* HIDDEN *)

    (* test lexical *)
    mkopt opt ~doc_level:Dev
      ["-tlex"; "--test-lexer"]
      (Arg.Unit (fun () -> opt.tlex <- true))
      ["Test the lexical analysis"]
    ;
    (* test syntaxique
    mkopt opt ~hide:true
      ["-tparse"]
      (Arg.Unit(function _ -> opt.gen_mode <- GenLuc ; opt.test_parse <- true ; ()))
      ["Test the syntactic analysis"]
    ;
    *)

    mkopt opt ~doc_level:Advanced
      ["-2cw7";"--2c-wcet"]
      (Arg.Unit (fun () ->
                 set_c_options opt;
                 global_opt.gen_wcet <- true;
                 global_opt.soc2c_no_switch <-true;
                 global_opt.soc2c_global_ctx <- true))
      ["Generate a main file for computing the wcet (force -2c -2cgc)"]
    ;

    mkopt opt ~doc_level:Dev
      ["-2cs";"--2c-stack"]
      (Arg.Unit (fun () -> set_c_options opt; global_opt.io_transmit_mode <- Stack))
      ["Transmit Soc I/O as params of the step functions (force -2c)"]
    ;
    mkopt opt ~doc_level:Dev
      ["-2ch";"--2c-heap"]
      (Arg.Unit (fun () -> set_c_options opt; global_opt.io_transmit_mode <- Heap))
      ["Transmit Soc I/O via a ctx structure in the heap (force -2c)"]
    ;
    mkopt opt ~doc_level:Dev
      ["-2chs";"--2c-heap-and-stack"]
      (Arg.Unit (fun () -> set_c_options opt; global_opt.io_transmit_mode <- HeapStack))
      ["Transmit soc memoryless I/O via the stack, and the heap otherwise (force -2c)"]
    ;

    mkopt opt ~doc_level:Dev
      ["--schedule-simple"]
      (Arg.Unit (fun () -> global_opt.schedul_mode <- Simple))
      ["No re-ordering after topological sort"]
    ;
    mkopt opt ~doc_level:Dev
      ["--schedule-sort"]
      (Arg.Unit (fun () -> global_opt.schedul_mode <- Sort))
      ["Sort wrt guard before after topological sort"]
    ;
    mkopt opt ~doc_level:Dev
      ["--schedule-reorder"]
      (Arg.Unit (fun () -> global_opt.schedul_mode <- Reorder))
      ["Re-order Soc.gao after scheduling to increase the clock factorisation"]
    ;

    mkopt opt ~doc_level:Advanced
      ["-2cns";"--2c-no-switch"]
      (Arg.Unit (fun () -> global_opt.soc2c_no_switch <-true))
      ["Use if-then-else instead of switches when generating C codes"]
    ;
    mkopt opt ~doc_level:Dev
      ["-interface"]
      (Arg.Unit (fun () -> opt.print_interface<-true))
      ["Print the node interface"]
    ;
    mkopt opt ~doc_level:Dev
      ["--keep-aliases"]
      (Arg.Unit (fun () -> opt.keep_aliases<-true))
      ["Do not perform aliases elimination"]
    ;
    mkopt opt ~doc_level:Dev
      ["-unit"]
      (Arg.Unit (fun () -> opt.run_unit_test<-true))
      ["Run embedded unit tests"]
    ;
    mkopt opt ~doc_level:Advanced
      ["--precision"]
      (Arg.Int (fun i -> opt.precision <- Some i))
      ["Number of digits after ther dot used to print floats in -exec mode"]
    ;
    mkopt opt ~doc_level:Dev
      ["--nonreg-test"]
      (Arg.Unit (fun () -> global_opt.nonreg_test <- true))
      ["Avoid printing full path error msgs to ease non-reg test decision"]
    ;
    mkopt opt ~doc_level:Dev
      ["--gen-autotest"]
      (Arg.Unit (fun () -> global_opt.gen_autotest <- true))
      ["Generate a Lutin Stimulator and a Lustre oracle to compare the ";
       "result of 2 Lustre compilers"]
    ;
    (* misc debug flag *)
    mkopt opt ~doc_level:Advanced
      ["-dbg"; "--debug"]	
      (Arg.Symbol
         ( Lv6Verbose.flag_list (),
           fun s -> let f = Lv6Verbose.get_flag s in Lv6Verbose.set_flag f))
      [ "<dbg_flag>"; 
        "Possible dbg_flag are: " ^(String.concat ", " (Lv6Verbose.flag_list())) ]
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
	 Arg.parse_argv ~current:current argv opt.opts (add_other opt) usage_msg;
    (List.iter 
       (fun f -> 
          if (String.sub f 0 1 = "-") then
            unexpected f opt
          else if not (Sys.file_exists f) then
            file_notfound f opt
          else ()
       ) 
       opt.others
    );
    opt.infiles <- (List.rev opt.others);
    if opt.main_node = "" &&
       (opt.gen_c || opt.exec  || opt.gen_lic )
    then (
      Printf.fprintf stderr "No node set: use -n to set a node\n";
      exit 2);
    current := save_current;
    opt
  ) with
  (* only 1rst line is interesting ! *)
  | Bad msg -> Printf.fprintf stderr "%s\n" (first_line msg); usage stderr opt; exit 2; 
  | Help _msg -> help opt ();
)
