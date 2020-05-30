Require Import String Omega.
Require Import Premise NodeHash.

Axiom h_not_eq_zero : forall n s, h n s <> zero.

Axiom set_last_2bits_not_zero : forall x n s,
    n <> 0 -> set_last_2bits x (h n s) <> zero.

Axiom set_last_2bits_h_injective : forall x1 x2 n1 n2 s1 s2,
    set_last_2bits x1 (h n1 s1) = set_last_2bits x2 (h n2 s2) -> (x1, h n1 s1) = (x2, h n2 s2).

Axiom h_injective : forall n1 n2 s1 s2,
    h n1 s1 = h n2 s2 -> (n1, s1) = (n2, s2).

Axiom to_string_injective : forall t1 t2,
    to_string t1 = to_string t2 -> t1 = t2.

Parameter head : t -> nat.
Axiom head_h : forall n s, head (h n s) = n.
Axiom head_set_last_2bits : forall x t, head (set_last_2bits x t) = head t.
Axiom head_zero : head zero = 0.

Lemma not_eq_h_set_last_2bits : forall x n1 n2 s1 s2,
    n1 <> n2 -> set_last_2bits x (h n1 s1) <> h n2 s2.
Proof.
  intros x n1 n2 s1 s2 neq. intro eq. destruct neq.
  apply (f_equal head) in eq. rewrite head_set_last_2bits in eq.
  now do 2 rewrite head_h in eq.
Qed.

Parameter length : t -> nat.
Notation the_fixed_length := 28.
Axiom length_zero : length zero = the_fixed_length.

Axiom of_string_to_string : forall t, of_string (to_string t) = t.
Axiom to_string_of_string : forall s, String.length s >= the_fixed_length ->
                                      to_string (of_string s) = s.
Axiom length_of_string : forall s, length (of_string s) = String.length s.
Lemma length_to_string : forall t, String.length (to_string t) = length t.
Proof.
  intros t. rewrite <- (length_of_string (to_string t)).
  now rewrite of_string_to_string.
Qed.

Lemma of_string_injective : forall s1 s2,
    String.length s1 >= the_fixed_length -> String.length s2 >= the_fixed_length ->
    of_string s1 = of_string s2 -> s1 = s2.
Proof.
  intros s1 s2 len1 len2 eq. apply (f_equal to_string) in eq.
  now rewrite (to_string_of_string s1), (to_string_of_string s2) in eq.
Qed.

Axiom length_h : forall i t, length (h i t) = the_fixed_length.
Axiom length_set_last_2bits : forall x t, length (set_last_2bits x t) = length t.

Axiom length_min : forall t, length t >= the_fixed_length.

Axiom length_merge : forall s1 s2,
    String.length (s1 ^^ s2) > String.length s1 + String.length s2.

Lemma not_eq_merge_zero : forall t seg,
    seg <> ""%string ->
    zero <> of_string (to_string t ^^ seg).
Proof.
  intros t seg neq. intro eq.
  apply (f_equal length) in eq.
  rewrite length_zero, length_of_string in eq.
  set (length_merge (to_string t) seg).
  rewrite (length_to_string t) in g.
  set (length_min t).
  omega.
Qed.

Lemma not_eq_append_zero : forall t seg,
    seg <> ""%string ->
    zero <> of_string (to_string t ++ seg).
Proof.
  intros t seg neq. intro eq.
  apply (f_equal length) in eq.
  rewrite length_zero, length_of_string in eq.
  rewrite string_app_length in eq.
  rewrite length_to_string in eq. set (length_min t).
  now destruct seg; [|simpl in eq; omega].
Qed.

Lemma not_eq_h_merge: forall seg i t1 t2,
    seg <> ""%string ->
    h i t1 <> (of_string (to_string t2 ^^ seg)).
Proof.
  intros seg i t1 t2 neq. intro eq.
  apply (f_equal length) in eq.
  rewrite length_h, length_of_string in eq.
  set (length_merge (to_string t2) seg).
  rewrite <- eq in g. rewrite length_to_string in g.
  set (length_min t2).
  omega.
Qed.

Lemma not_eq_h_append: forall seg i t1 t2,
    seg <> ""%string ->
    h i t1 <> (of_string (to_string t2 ++ seg)).
Proof.
  intros seg i t1 t2 neq. intro eq.
  apply (f_equal length) in eq.
  rewrite length_h, length_of_string in eq.
  rewrite string_app_length, length_to_string in eq.
  set (length_min t2).
  now destruct seg; [| simpl in eq; omega].
Qed.

Lemma not_eq_set_last_2bits_h_merge: forall seg x i t1 t2,
    seg <> ""%string ->
    set_last_2bits x (h i t1) <> (of_string (to_string t2 ^^ seg)).
Proof.
  intros seg x i t1 t2 neq. intro eq.
  apply (f_equal length) in eq.
  rewrite length_set_last_2bits, length_h, length_of_string in eq.
  set (length_merge (to_string t2) seg).
  set (length_min t2).
  rewrite length_to_string in g. omega.
Qed.

Lemma not_eq_set_last_2bits_h_append: forall seg x i t1 t2,
    seg <> ""%string ->
    set_last_2bits x (h i t1) <> (of_string (to_string t2 ++ seg)).
Proof.
  intros seg x i t1 t2 neq. intro eq.
  apply (f_equal length) in eq.
  rewrite length_set_last_2bits, length_h, length_of_string in eq.
  rewrite string_app_length, length_to_string in eq.
  set (length_min t2).
  now destruct seg; [| simpl in eq; omega].
Qed.

Inductive Is_Extender : node -> Prop :=
| IE : forall seg n, Is_Extender (Extender seg n).
Hint Constructors Is_Extender.

Inductive Invariant_node : node -> Prop :=
| InvBudNone : Invariant_node (Bud None)
| InvBudSome : forall n, Invariant_node n -> Invariant_node (Bud (Some n))
| InvLeaf : forall v, Invariant_node (Leaf v)
| InvInt : forall n1 n2, Invariant_node n1 -> Invariant_node n2 ->
                         Invariant_node (Internal n1 n2)
| InvExt : forall seg n, Invariant_node n -> seg <> ""%string -> ~ Is_Extender n ->
    Invariant_node (Extender seg n).

Lemma length_hash_of_child_of_extender : forall seg n,
    Invariant_node (Extender seg n) ->
    length (compute n) = the_fixed_length.
Proof.
  intros seg n Inv.
  destruct n as [[n' |] | | |]; simpl.
  - now rewrite length_set_last_2bits, length_h.
  - exact length_zero.
  - now apply length_h.
  - now rewrite length_set_last_2bits, length_h.
  - inversion Inv. now destruct H3.
Qed.

Theorem hash_CR : forall n1 n2,
    Invariant_node n1 -> Invariant_node n2 ->
    compute n1 = compute n2 -> n1 = n2.
Proof.
  induction n1 using node_ind'; intros n2 Inv1 Inv2.
  - (* n1 = Bud None *)
    destruct n2 as [[n' |] | | |]; simpl; intro hash_eq.
    + edestruct set_last_2bits_not_zero with (n := 2); [now auto|]. now rewrite <- hash_eq.
    + reflexivity.
    + edestruct h_not_eq_zero. now rewrite <- hash_eq.
    + edestruct set_last_2bits_not_zero with (n := 1); [now auto|]. now rewrite <- hash_eq.
    + (* Use: _ ++ seg <> zero *)
      now edestruct not_eq_append_zero; [inversion Inv2|exact hash_eq].
  - (* n1 = Bud (Some n) *)
    destruct n2 as [[n' |] | | |]; simpl; intro hash_eq.
    + do 2 f_equal. apply IHn1; [now inversion Inv1 | now inversion Inv2 |].
      apply set_last_2bits_h_injective in hash_eq. injection hash_eq. intro h_eq.
      injection (h_injective _ _ _ _ h_eq).
      apply h_injective in h_eq.
      now apply to_string_injective.
    + edestruct set_last_2bits_not_zero with (n := 2); [now auto|]. exact hash_eq.
    + now apply not_eq_h_set_last_2bits in hash_eq.
    + now injection (set_last_2bits_h_injective _ _ _ _ _ _ hash_eq).
    + (* n2 = Extender seg n2
         Use: (set_last_ (h ...)) <> _ ++ seg
       *)
      now edestruct not_eq_set_last_2bits_h_append; [inversion Inv2| exact hash_eq].
  - (* n1 = Leaf v *)
    destruct n2 as [[n' |] | | |]; simpl; intro hash_eq.
    + apply eq_sym in hash_eq.
      now apply not_eq_h_set_last_2bits in hash_eq.
    + edestruct h_not_eq_zero. exact hash_eq.
    + injection (h_injective _ _ _ _ hash_eq). intro eq. now rewrite eq.
    + apply eq_sym in hash_eq.
      now apply not_eq_h_set_last_2bits in hash_eq.
    + (* n2 = Extender seg n2
         Use : (h ... <> _ ++ seg)
       *)
      now edestruct (not_eq_h_append s 0 v); [inversion Inv2| exact hash_eq].
  - (* n1 = Internal n1_1 n2_2 *)
    destruct n2 as [[n' |] | | |]; simpl; intro hash_eq.
    + now injection (set_last_2bits_h_injective _ _ _ _ _ _ hash_eq).
    + apply (f_equal head) in hash_eq.
      now rewrite head_set_last_2bits, head_h, head_zero in hash_eq.
    + apply (f_equal head) in hash_eq.
      rewrite head_set_last_2bits in hash_eq.
      now do 2 rewrite head_h in hash_eq.
    + (* n2 = Internal n2_1 n2_2 *)
      injection (set_last_2bits_h_injective _ _ _ _ _ _ hash_eq). intro h_eq.
      injection (h_injective _ _ _ _ h_eq). intro eq.
      (* Use: injectivitiy of merge and inductive hypothesises *)
      injection (merge_injective _ _ _ _ eq). intros eq1 eq2.
      apply to_string_injective in eq1.
      apply to_string_injective in eq2.
      rewrite (IHn1_1 n2_1); [| now inversion Inv1| now inversion Inv2| assumption].
      rewrite (IHn1_2 n2_2); [| now inversion Inv1| now inversion Inv2| assumption].
      reflexivity.
    + (* n2 = Extender seg n2
         Use: (set_last_ (h ...)) <> _ ++ seg
       *)
      now edestruct not_eq_set_last_2bits_h_append; [inversion Inv2| exact hash_eq].
  - (* n1 = Extender seg n *)
    destruct n2 as [[n' |] | | |]; simpl; intro hash_eq.
    + (* n2 = Bud (Some n')
         Use: (set_last_ (h ...)) <> _ ++ seg
       *)
      apply eq_sym in hash_eq.
      now edestruct not_eq_set_last_2bits_h_append; [inversion Inv1|exact hash_eq].
    + (* n2 = Bud None
         Use: _ ++ seg <> zero
       *)
      apply eq_sym in hash_eq.
      now edestruct (not_eq_append_zero); [inversion Inv1| exact hash_eq].
    + (* n2 = Leaf v
         Use: _ ++ seg <> h _ _
       *)
      apply eq_sym in hash_eq.
      now edestruct not_eq_h_append; [inversion Inv1 | exact hash_eq].
    + (* n2 = Internal n2_1 n2_2
         Use: _ ++ seg <> set_last_ (h _ _)
       *)
      apply eq_sym in hash_eq.
      now edestruct not_eq_set_last_2bits_h_append; [inversion Inv1|exact hash_eq].
    + (* n2 = Extender seg' n2
         Use: injectivity of of_string and merge and the inductive hypothesis
       *)
      apply of_string_injective in hash_eq.
      *
        {eapply (string_append_injective_with_same_length) in hash_eq.
         - injection hash_eq. intros eq1 eq2.
           rewrite eq1. apply to_string_injective in eq2.
           inversion Inv1. inversion Inv2. now rewrite (IHn1 n2).
         - do 2 rewrite length_to_string.
           erewrite length_hash_of_child_of_extender; [|now apply Inv1].
           erewrite length_hash_of_child_of_extender; [|now apply Inv2].
           reflexivity.
        }
      * (* length of merge _ _ *)
        rewrite string_app_length, length_to_string.
        set (length_min (compute n1)). omega.
      * (* length of merge _ _ *)
        rewrite string_app_length, length_to_string.
        set (length_min (compute n2)). omega.
Qed.
