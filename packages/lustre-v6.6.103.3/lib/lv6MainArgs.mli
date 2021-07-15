(* Time-stamp: <modified the 13/07/2021 (at 11:44) by Erwan Jahier> *)

type enum_mode = 
    AsInt  (* translate enums into int (for rif-friendlyness *)
  | AsBool (* translate enums into bool arrays; not working yet *)
  | AsConst  (* translate enums into abstract const *)
  | AsEnum (* do nothing *)


(* koketeri, vu qu'on continu à ranger concetement
   les options dans des var. globales ! (cf Global
*)

type io_transmit_mode =
  | Stack (* All I/O are are passed as arguments of the step functions *)
  | Heap  (* All I/O are in a ctx structure; ctx of memoryless soc are global *)
  | HeapStack  (* I/O of memoryful soc are in a ctx structure; memoryless soc uses step arg *)
(* *)

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
  mutable gen_c :  bool;
  mutable rif :  bool;
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
val paranoid : Lv6Verbose.flag 

val global_opt:global_opt

(* La ``méthode'' principale *)
val parse : string array -> t

val usage : out_channel -> t -> unit
val full_usage : out_channel -> t -> unit

val lexbuf_of_file_name : string -> Lexing.lexbuf
