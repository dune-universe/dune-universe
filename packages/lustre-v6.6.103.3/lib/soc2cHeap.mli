(* Time-stamp: <modified the 26/06/2017 (at 16:41) by Erwan Jahier> *)

(** Gathers all entities (functions, types) that implement the
    heap-based C generator.  *)

(* [gen_assign t vi vo] generated the C code that assign vo to vi,
   using memcpy or = depending on the type t *)
val gen_assign : Data.t  -> string -> string -> string

(* ditto *)
val gen_assign_var_expr : Soc.t -> Soc.var_expr -> Soc.var_expr -> string

(* Generates the C step name from the soc key and the soc step name *)
val step_name : Soc.key -> string -> string

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

val typedef_of_soc : Soc.t -> string
