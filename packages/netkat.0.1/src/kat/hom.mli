(** Operations on ASTs often take the form of {i homomorphisms}. Examples
    include syntax transformations, compilers, or interpreters. This module 
    provides tools for conveniently defining such homomorphisms,
    eliminating boilerplate code.
*)

(** {1 Algebraic signatures} *)

(** Boolean algebra on ['b].  *)
type 'b ba = {
  ctrue : 'b;
  cfalse : 'b;
  conj : 'b -> 'b -> 'b;
  disj : 'b -> 'b -> 'b;
  neg : 'b -> 'b;
}

(** Kleene Algebra with Tests on ['k] and ['b].  *)
type ('k, 'b) kat = {
  ba : 'b ba;
  assrt : 'b -> 'k;
  union : 'k -> 'k -> 'k;
  seq : 'k -> 'k -> 'k;
  star : 'k -> 'k;
}


(** {1 Interpretations of (aka homomorphisms on) Boolean and KAT expressions}

Any interpretation ['test -> 'b] of primitive tests as elements in ['b]
also induces an interpretation ['test bexp -> 'b] of Boolean expressions
as elements in ['b], provided ['b] carries a Boolean algebra structure.

More formally, for each Boolean algebra [ba: 'b ba] on ['b], any
interpretation of primitive tests [map_test : 'test -> 'b] extends
uniquely to a homomorphism from the initial algebra ['test bexp] to ['b];
the homomomorphism is given by 
[map_bexp ~ba ~map_test : 'test bexp -> 'b].

The sitatuion for a KAT [kat: ('k,'b) kat] on ['k] and ['b] is similar. Given
+ an interpretation [map_test : 'test -> 'b] of primitive tests as elements in ['b]; and
+ an interpretation [map_act : 'act -> 'k] of actions as elements in ['k];

the unique homomorphism from KAT expressions [('act,'test) exp] to ['k] is
given by [map_exp ~kat ~map_test ~map_act: ('act,'test) exp -> 'k].
*)

val map_bexp :
  ba:'b ba -> map_test:('test -> 'b)
  -> 'test Ast.bexp -> 'b

val map_exp :
  kat:('k,'b) kat -> map_test:('test -> 'b) -> map_act:('act -> 'k) 
  -> ('act, 'test) Ast.exp -> 'k