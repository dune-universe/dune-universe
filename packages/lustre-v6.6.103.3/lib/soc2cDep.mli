(* Time-stamp: <modified the 26/06/2017 (at 16:39) by Erwan Jahier> *)


(** Choose between the various C code generators (heap-based, Stack
    based, etc).

    the "Dep" in the module name means "Depends", but I'm not happy
    with this name...  *)


(* ditto *)
val gen_assign_var_expr : Soc.t -> Soc.var_expr -> Soc.var_expr -> string

(* Generates the C step name from the soc key and the soc step name *)
val step_name : Soc.key -> string -> string

(* returns the step declaration (to put in the .h) and step header
    (to put in the .c) 
   For instance , something like:
   "void step(ctx_type, int, int, int* );",  
   "void step(ctx_type ctx, int x, int y,int* z){",  
*)
val get_step_prototype : Soc.step_method -> Soc.t -> string * string * string


val string_of_var_expr: Soc.t -> Soc.var_expr -> string


(* [ctx_var vk id] *)
val ctx_var : Soc2cUtil.var_kind -> Soc.t -> Lv6Id.t -> string
  
(*  [gen_step_call soc called_soc vel_out vel_in ctx sname step_arg]
 Generates the C code that performs the call to a step method of
   [called_soc] from [soc].
   - [vel_out] and [vel_in] are the called_soc O/I arguments
   - [ctx] is the C name of the context of the called_soc
   - [sname] is the C name of the step to call
   - [step_arg] is a string holding the arg of the step (empty for proc call)
*)

val gen_step_call : Soc.t -> Soc.t -> Soc.var_expr list -> Soc.var_expr list -> 
     string -> string -> string -> string
(* should this soc be inlined? (depends on Lv6MainArgs.global_opt) *)
val inlined_soc : Soc.key -> bool

(** Returns the C code corresponding a soc key  *)
val get_predef_op: Soc.key -> string

(** Returns the C code implementing an iterator (map, fill, red) *)
val get_iterator : Soc.t -> string ->  Soc.t ->  int -> string


(** Returns the C code implementing a condact *)
val get_condact : Soc.t -> Soc.t -> Soc.var_expr list -> string 

(** Returns the C code implementing a boolred *)
val get_boolred : Soc.t -> int -> int -> int -> string 

val typedef_of_soc : Soc.t -> string


