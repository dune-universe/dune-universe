(** Forwarding Tables.
  
    A forwarding table is an alternative representation of a relation 
    R ⊆ \{0,1\}^n x \{0,1\}^n. For an IDD on [2n] variables, a row in a forwarding 
    table is a pair [(P,A)] where [P] is a pattern on which the input string 
    is matched and [A] is a set of modifications which is applied to an input 
    that matches [P]. Specifically for a given input [p], the first pattern [P] 
    that [p] matches yields a set of ouputs from the action [A] of [P] such that
    this output set is exactly \{p':(p,p') ∈ R\}
*)

open Idds
open Base

(** {2 Type} *)

(** The type of a forwarding table. *)
type t

(** {2 Constructor} *)

(** [to_table idd] is the table [t] such that [eval t env] iff [Idd.eval idd env]
    for all [env] *)
val to_table : Idd.t -> t

(** {2 Semantics} *)

(** [eval tbl env] evaluates the forwarding table [tbl] in environment [env] *)
val eval : t -> (Var.t -> bool) -> bool

(** {2 Generic operations on Forwarding Tables} *)

(** [to_string ?var_name tbl] converts [tbl] to a string using [var_name] to map
    variables to string. [var_name] defaults to [Var.to_string] *)
val to_string : ?var_name:(Var.t -> string) -> t -> string

(** [to_expr tbl ~interp_test ~interp_act] is the KAT expression representing 
    [tbl] where [interp_act] ([interp_test]) specifies how to map actions 
    (patterns) in the forwarding table to (Boolean) expressions *)
val to_expr : t -> interp_test:((Var.t * bool) list -> 'test Kat.Ast.bexp)
  -> interp_act:((Var.t * bool) list -> ('act, 'test) Kat.Ast.exp) -> ('act, 'test) Kat.Ast.exp

(** [render tbl] renders [tbl] in HTML and opens the file using [var_name] to 
    map variables to string. [var_name] defaults to [Var.to_string] *)
val render : ?var_name:(Var.t -> string) -> t -> unit