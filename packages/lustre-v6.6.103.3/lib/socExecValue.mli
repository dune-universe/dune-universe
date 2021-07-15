(* Time-stamp: <modified the 25/11/2016 (at 17:03) by Erwan Jahier> *)

(** Manipulating data in the Soc interpreter  *)

type path = Soc.ident list
type subst = (path * Data.v)

(* Not really a good name anymore. Memory ? *)
type substs 


(* update the memory. The path correspond to a path in the call tree,
   except the last ident that is the variable name we want to
   associate a value to.

 [sadd ct x v] updates updates ct by associating x to v in ct 
*)
val sadd : substs -> path -> Data.v -> substs

(* [sadd_partial ct ve path v]  updates ct by associating ve::path  to v in ct ;  

   nb : It is a more general version of sadd that does not only work on
   var but on var_expr. This means that it can update an array element,
   or a struct field, whereas sadd only updates variable.
*)

val sadd_partial : substs  -> Soc.var_expr -> path -> Data.v -> substs


type ctx = { 
  cpath:Soc.ident list;
  s:substs;
}

(* Performs a recursive traversal of the top-level soc to init memories. *)
val create_ctx : Soc.tbl -> Soc.t -> ctx


val get_val : Soc.ident -> ctx -> Data.v
val get_value : ctx -> Soc.var_expr -> Data.v
val get_enum : Soc.ident -> ctx -> Soc.ident

  (* Returns all the variables accessible from a ctx (i.e., a node) *)
val get_vals : ctx -> Data.subst list

(* Pretty-printers *)
val string_of_substs :substs -> string

(* RIF I/O *)
val dump_substs : substs -> unit
val read_enum : Soc.ident list -> Soc.ident
val read_value : Soc.var -> Data.v

(* if 
   args = [Var("x",Int); Const("3.14",Real) ] 
   pars = [("a",Int); ("b",Real)]
   s = [ "x",I(42) ; ... ]

then I want to output the follwing substitution :

   s = [ "a",I(42) ; "b",R(3.14) ]

nb : args and pars order matters

*)
val substitute_args_and_params : Soc.var_expr list -> Soc.var list -> ctx -> substs
val substitute_params_and_args : Soc.var list -> Soc.var_expr list -> ctx -> substs

(* Returns the top-level variable substitutions in a RIF format *)
val filter_top_subst     : substs -> Data.subst list
val substs_to_data_subst : substs -> Data.subst list



  
