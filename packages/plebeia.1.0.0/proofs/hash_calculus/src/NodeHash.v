Require Import String.
Require Import Premise.

Parameter t : Set.
Parameter zero : t.

Parameter of_value : value -> t.
Parameter to_string : t -> string.
Parameter of_string : string -> t.

Parameter set_last_2bits : nat -> t -> t.

Parameter h : nat -> string -> t.

Fixpoint compute node :=
  match node with
  | Bud None => zero
  | Bud (Some n) =>
    set_last_2bits 3 (h 2 (to_string (compute n)))
  | Leaf v => h 0 v
  | Internal n1 n2 =>
    set_last_2bits 0 (h 1 (to_string (compute n1) ^^ to_string (compute n2)))
  | Extender seg n => of_string (to_string (compute n) ++ seg)
  end.
