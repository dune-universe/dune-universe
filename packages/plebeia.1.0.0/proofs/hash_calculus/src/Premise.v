Require Import String.

Definition segment : Set := string.

Definition value := string.

Inductive node : Set :=
| Bud : option node -> node
| Leaf : value -> node
| Internal : node -> node -> node
| Extender : segment -> node -> node.

Fixpoint node_ind' (P : node -> Prop)
         (Case1 : P (Bud None))
         (Case2 : forall n : node, P n -> P (Bud (Some n)))
         (Case3 : forall v : value, P (Leaf v))
         (Case4 : forall n1, P n1 -> forall n2, P n2 -> P (Internal n1 n2))
         (Case5 : forall (seg : segment) (n : node), P n -> P (Extender seg n))
         (n : node) : P n :=
  match n with
  | Bud None => Case1
  | Bud (Some n) => Case2 n (node_ind' P Case1 Case2 Case3 Case4 Case5 n)
  | Leaf v => Case3 v
  | Internal n1 n2 => Case4 n1 (node_ind' P Case1 Case2 Case3 Case4 Case5 n1)
                            n2 (node_ind' P Case1 Case2 Case3 Case4 Case5 n2)
  | Extender seg n => Case5 seg n (node_ind' P Case1 Case2 Case3 Case4 Case5 n)
  end.

Parameter merge : string -> string -> string.

Infix "^^" := merge (at level 80).

Axiom merge_injective : forall a b a' b',
    (a ^^ b) = (a' ^^ b') -> (a,b) = (a',b').

Lemma string_app_length : forall s1 s2,
    String.length (s1 ++ s2) = String.length s1 + String.length s2.
Proof.
  induction s1.
  - reflexivity.
  - simpl. intros s2. now rewrite IHs1.
Qed.

Lemma string_append_injective_with_same_length : forall (x y x' y': string),
    length x = length x' ->
    (x ++ y)%string = (x' ++ y')%string -> (x, y) = (x', y').
Proof.
  induction x.
  - now destruct x'; [simpl; intros; subst| discriminate].
  - destruct x'; [ discriminate|].
    simpl. intros y' eq H.
    injection eq. injection H. intros Htl Hhd eq'.
    injection (IHx y x' y' eq' Htl). intros eqy eqx.
    now subst.
Qed.
