(* Time-stamp: <modified the 01/06/2021 (at 16:40) by Erwan Jahier> *)

type var = Data.ident * Data.t

(** Source info related to the current control point. Its content
   depends on the plugin it comes from. *)
type src_info_atom = { 
  str  : string ; 
  file : string ; 
  line : int * int ; (* line nb begin/end in file *)
  char : int * int ; (* char nb begin/end in file *)
  stack: src_info_atom option; 
}

type src_info = {
  expr : Expr.t ;    (* holds info about the current control point *)
  atoms: src_info_atom list; 
  more : (unit -> Expr.t) option;  (* more (costly) info *)
  in_subst  : (var * var) list; (* relate input  args and parameters *)
  out_subst : (var * var) list; (* relate output args and parameters *)
}

(* a.k.a. ports in the Prolog Byrd's box model *)
type kind = Ltop | Call | Exit 
  | MicroStep of string (* holds info related to the sub-lang micro-step *)

type t = { 
  nb    : int;
  step  : int;
  depth : int;
  data  : Data.subst list;  
  next  : unit -> t;
  terminate: unit -> unit;
  reset: unit -> unit;
  save_state: int -> unit;    (* necessary for backward time-travelling with *) 
  restore_state: int -> unit; (* reactive programs that perform side-effects *)
  kind : kind;
  name : string;
  lang : string; 
  inputs  : var list; 
  outputs : var list;
  locals  : var list;
  sinfo : (unit -> src_info) option
} 

(** raised by next  when there is no next event. Holds the event number.*)
exception End of int 
exception Error of string * t 

(**/**)
                 
let set_nb e i = { e with nb = i }

let incr_event_nb ctx = { ctx with nb = ctx.nb+1 }
let decr_event_depth ctx = { ctx with depth = ctx.depth-1 }
let incr_event_depth ctx = { ctx with depth = ctx.depth+1 }
                             
(****************************************************************)
(* printers (for toplevel) *)
let (print_expr : Format.formatter -> Expr.t -> unit) =
  fun ff e ->
  Format.fprintf ff "%s" (Expr.to_string e);
  ()

