(** Generated by coq-of-ocaml *)
Require Import OCaml.OCaml.

Local Set Primitive Projections.
Local Open Scope string_scope.
Local Open Scope Z_scope.
Local Open Scope type_scope.
Import ListNotations.

Module M.
  Definition b : bool := false.
  
  Definition n : int := 12.
End M.

Definition n : int := Z.add M.n 2.
