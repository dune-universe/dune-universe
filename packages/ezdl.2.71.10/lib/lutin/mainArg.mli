(** LUTIN2 : arguments de la commande

*)

type t

(* La ``méthode'' principale *)
val parse : string array -> t
val make_opt : unit -> t

val usage : out_channel -> t -> unit
val full_usage : out_channel -> t -> unit

(* Paramètres statiques *)
val version : string
val tool_name : string
val usage_msg : string

(* Accès aux arguments *)
val infile : t -> string list
val test_lex : t -> bool
val test_parse : t -> bool
val test_check : t -> bool
val test_expand : t -> bool
val test_auto : t -> bool
val main_node : t -> string

val test_exe : t -> bool
val run : t -> bool
val boot : t -> bool
(* Simulation *)
val max_steps : t -> int option

type gen_mode = 
    Simu    (* Simulate the Lutin file *)
  | GenLuc  (* Generate a lucky file *)
  | Ocaml   (* Generate ocaml stubs file *)
  | Cstubs  (* Generate C stubs files *)
val gen_mode : t -> gen_mode

val load_mem : t -> bool

val seed : t -> int
val set_seed : t -> int option -> unit

val event_incr : t -> unit
val get_event_nb : t -> int

(* n.b. if "Some l", l is certainly non-empty ! *)
val libs : t -> string list option
val set_libs : t -> string list -> t

(* Output *)
val outfile : t -> string option
val outpipe : t -> bool

(* Rif *)
val riffile : t -> string option
val call_gnuplot : t -> bool

val only_outputs : t -> bool

val show_locals : t -> bool
val set_show_locals : t -> bool -> unit

val see_all_options : t -> bool

(* Options du solver *)
val step_mode : t -> Lucky.step_mode
val set_step_mode : t -> Lucky.step_mode -> unit

val compute_volume : t -> bool
val set_compute_volume : t -> bool -> unit

val set_precision : int -> unit


