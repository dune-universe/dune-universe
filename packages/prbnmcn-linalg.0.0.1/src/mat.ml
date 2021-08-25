open Intf

module Make_internal
    (Repr : Basic_intf.Lang.Empty)
    (Monad : Basic_intf.Codegen_monad with type 'a m = 'a Repr.m)
    (S : Tensor with type 'a m = 'a Repr.m and type 'a k = 'a Monad.t)
    (B : Basic_intf.Lang.Bool with type 'a m = 'a Repr.m)
    (R : Basic_intf.Lang.Ring with type 'a m = 'a Repr.m)
    (R_storage : Basic_intf.Lang.Storage
                   with type 'a m = 'a Repr.m
                    and type elt = R.t)
    (I_ring : Basic_intf.Lang.Ring with type 'a m = 'a Repr.m and type t = S.pos)
    (E : Basic_intf.Lang.Exn with type 'a m = 'a Repr.m)
    (M : Basic_intf.Lang.Sequencing with type 'a m = 'a Repr.m)
    (P : Basic_intf.Lang.Product with type 'a m = 'a Repr.m) =
struct
  type 'a m = 'a Repr.m

  type base_index = S.pos

  (* An matrix index is a pair (c, r) of a column index c and a row index r. *)

  type 'a shape = 'a S.t

  let index ~c ~r = P.prod c r

  (* Could we pass this as argument to the matrix functor? *)
  (* module V = Vec.Make_internal (Repr) (B) (R) (R_storage) (S) (E) (M) (Monad) *)

  include (
    Vec.Make_internal (Repr) (Monad) (S) (B) (R) (R_storage) (E) (M) :
        module type of
          Vec.Make_internal (Repr) (Monad) (S) (B) (R) (R_storage) (E) (M)
        with type 'a shape := 'a shape
         and type 'a m := 'a m)

  open Monad.Infix

  let cols (Vec (s, _)) = S.fst s

  let rows (Vec (s, _)) = S.snd s

  let identity size =
    let shape = S.tensor size size in
    Vec
      ( shape,
        fun i ->
          B.dispatch (S.pos_equal size (P.fst i) (P.snd i)) @@ function
          | true -> R.one
          | false -> R.zero )

  let diagonal (Vec (idim, v)) =
    let shape = S.tensor idim idim in
    Vec
      ( shape,
        fun i ->
          B.dispatch (S.pos_equal idim (P.fst i) (P.snd i)) @@ function
          | true ->
              let open M in
              let* i' = P.fst i in
              v i'
          | false -> R.zero )

  let unsafe_col (Vec (s, v)) c = Vec (S.snd s, fun r -> v (P.prod c r))

  let col (Vec (s, v)) c =
    let*! _ =
      B.dispatch (S.mem (S.fst s) c) @@ function
      | false -> E.raise_ Intf.Out_of_bounds
      | true -> M.unit
    in
    Monad.return (Vec (S.snd s, fun r -> v (P.prod c r)))

  let of_col (Vec (s, v)) =
    let shape = S.tensor S.scalar s in
    let m index =
      let open M in
      let* r = P.snd index in
      v r
    in
    Vec (shape, m)

  let unsafe_row (Vec (s, v)) r = Vec (S.fst s, fun c -> v (P.prod c r))

  let row (Vec (s, v)) r =
    let*! _ =
      B.dispatch (S.mem (S.snd s) r) @@ function
      | false -> E.raise_ Intf.Out_of_bounds
      | true -> M.unit
    in
    Monad.return (Vec (S.fst s, fun c -> v (P.prod c r)))

  let of_row (Vec (s, v)) =
    let shape = S.tensor s S.scalar in
    let m index =
      let open M in
      let* r = P.fst index in
      v r
    in
    Vec (shape, m)

  let swap_rows (Vec (s, m)) r1 r2 =
    let rows_shape = S.snd s in
    let*! _ =
      B.dispatch B.(S.mem rows_shape r1 && S.mem rows_shape r2) @@ function
      | false -> E.raise_ Intf.Out_of_bounds
      | true -> M.unit
    in
    Monad.return
      (Vec
         ( s,
           fun i ->
             let open M in
             let* ri = P.snd i in
             B.dispatch (S.pos_equal rows_shape ri r1) @@ function
             | true -> m (P.prod (P.fst i) r2)
             | false -> (
                 B.dispatch (S.pos_equal rows_shape ri r2) @@ function
                 | true -> m (P.prod (P.fst i) r1)
                 | false -> m i) ))

  let swap_cols (Vec (s, m)) c1 c2 =
    let cols_shape = S.fst s in
    let*! _ =
      B.dispatch B.(S.mem cols_shape c1 && S.mem cols_shape c2) @@ function
      | false -> E.raise_ Intf.Out_of_bounds
      | true -> M.unit
    in
    Monad.return
      (Vec
         ( s,
           fun i ->
             let open M in
             let* ci = P.fst i in
             B.dispatch (S.pos_equal cols_shape ci c1) @@ function
             | true -> m (P.prod c2 (P.snd i))
             | false -> (
                 B.dispatch (S.pos_equal cols_shape ci c2) @@ function
                 | true -> m (P.prod c1 (P.snd i))
                 | false -> m i) ))

  let concat_horiz (Vec (s1, m1)) (Vec (s2, m2)) =
    let cols1 = S.fst s1 in
    let* shape = S.concat s1 s2 (S.Path.left ()) in
    let*! cols1_dim = S.dim cols1 S.Path.empty in
    let f index =
      let open M in
      let* c = P.fst index in
      let* r = P.snd index in
      B.dispatch (S.mem cols1 c) @@ function
      | true -> m1 index
      | false ->
          let* index = P.prod I_ring.(sub c cols1_dim) r in
          m2 index
    in
    Monad.return (Vec (shape, f))

  let concat_vert (Vec (s1, m1)) (Vec (s2, m2)) =
    let rows1 = S.snd s1 in
    let* shape = S.concat s1 s2 (S.Path.right ()) in
    let*! rows1_dim = S.dim rows1 S.Path.empty in
    let f index =
      let open M in
      let* c = P.fst index in
      let* r = P.snd index in
      B.dispatch (S.mem rows1 r) @@ function
      | true -> m1 index
      | false ->
          let* index = P.prod c I_ring.(sub r rows1_dim) in
          m2 index
    in
    Monad.return (Vec (shape, f))

  let mm (Vec (l_shape, _) as lhs) (Vec (r_shape, _) as rhs) =
    let*! _ =
      B.dispatch S.(equal (fst l_shape) (snd r_shape)) @@ function
      | false -> E.raise_ Intf.Dimensions_mismatch
      | true -> M.unit
    in
    let shape = S.tensor (S.fst r_shape) (S.snd l_shape) in
    Monad.return
      (Vec
         ( shape,
           fun i ->
             let open M in
             let* r = P.snd i in
             let* c = P.fst i in
             let row = unsafe_row lhs r in
             let col = unsafe_col rhs c in
             unsafe_dot (idim row) row col ))
  end
  [@@inline]

module Make : functor
  (Repr : Basic_intf.Lang.Empty)
  (Monad : Basic_intf.Codegen_monad with type 'a m = 'a Repr.m)
  (S : Tensor with type 'a m = 'a Repr.m and type 'a k = 'a Monad.t)
  (B : Basic_intf.Lang.Bool with type 'a m = 'a Repr.m)
  (R : Basic_intf.Lang.Ring with type 'a m = 'a Repr.m)
  (R_storage : Basic_intf.Lang.Storage
                 with type 'a m = 'a Repr.m
                  and type elt = R.t)
  (I_ring : Basic_intf.Lang.Ring with type 'a m = 'a Repr.m and type t = S.pos)
  (E : Basic_intf.Lang.Exn with type 'a m = 'a Repr.m)
  (M : Basic_intf.Lang.Sequencing with type 'a m = 'a Repr.m)
  (P : Basic_intf.Lang.Product with type 'a m = 'a Repr.m)
  ->
  Intf.Mat
    with type 'a k = 'a Monad.t
     and type 'a m = 'a Repr.m
     and type base_index = I_ring.t
     and type 'a shape = 'a S.t
     and type ('a, 'b) morphism = ('a, 'b) S.Morphism.t
     and type elt = R.t =
  Make_internal

(* Matrices backed by arrays *)

(* Instantiate some typical schemes *)
module Array_backed_column_major
    (Repr : Basic_intf.Lang.Empty)
    (Monad : Basic_intf.Codegen_monad with type 'a m = 'a Repr.m)
    (S : Intf.Tensor with type 'a m = 'a Repr.m)
    (B : Basic_intf.Lang.Bool with type 'a m = 'a Repr.m)
    (R : Basic_intf.Lang.Ring with type 'a m = 'a Repr.m and type t = S.pos)
    (R_ord : Basic_intf.Lang.Infix_order
               with type 'a m = 'a Repr.m
                and type t = R.t)
    (A : Basic_intf.Lang.Array with type index = R.t and type 'a m = 'a Repr.m)
    (E : Basic_intf.Lang.Exn with type 'a m = 'a Repr.m)
    (P : Basic_intf.Lang.Product with type 'a m = 'a Repr.m)
    (M : Basic_intf.Lang.Sequencing with type 'a m = 'a Repr.m) =
struct
  type index = S.pos * S.pos

  open Monad.Infix

  let of_array shape (a : A.t Repr.m) =
    let rows = S.snd shape in
    let num_rows = S.dim rows S.Path.empty in
    let*! numel = S.numel shape in
    let*! l = A.length a in
    let*! _ =
      B.dispatch R_ord.(l = numel) @@ function
      | false -> E.raise_ Intf.Dimensions_mismatch
      | true -> M.unit
    in
    let vec =
      Vec
        ( shape,
          fun i ->
            let l = R.(add (P.snd i) (mul num_rows (P.fst i))) in
            A.unsafe_get a l )
    in
    let ovec =
      OVec
        ( shape,
          fun i v ->
            let l = R.(add (P.snd i) (mul num_rows (P.fst i))) in
            A.unsafe_set a l v )
    in
    Monad.return (vec, ovec)

  let in_of_array shape a =
    let rows = S.snd shape in
    let num_rows = S.dim rows S.Path.empty in
    let*! numel = S.numel shape in
    let*! l = A.length a in
    let*! _ =
      B.dispatch R_ord.(l = numel) @@ function
      | false -> E.raise_ Intf.Dimensions_mismatch
      | true -> M.unit
    in
    let vec =
      Vec
        ( shape,
          fun i ->
            let l = R.(add (P.snd i) (mul num_rows (P.fst i))) in
            A.unsafe_get a l )
    in
    Monad.return vec

  let out_of_array shape a =
    let rows = S.snd shape in
    let num_rows = S.dim rows S.Path.empty in
    let*! numel = S.numel shape in
    let*! l = A.length a in
    let*! _ =
      B.dispatch R_ord.(l = numel) @@ function
      | false -> E.raise_ Intf.Dimensions_mismatch
      | true -> M.unit
    in
    let ovec =
      OVec
        ( shape,
          fun i v ->
            let l = R.(add (P.snd i) (mul num_rows (P.fst i))) in
            A.unsafe_set a l v )
    in
    Monad.return ovec
end
[@@inline]

(**/**)

module BL = Basic_impl.Lang

(**/**)

module Make_native
    (R : Basic_intf.Lang.Ring with type 'a m = 'a)
    (R_storage : Basic_intf.Lang.Storage with type 'a m = 'a and type elt = R.t) =
  Make (BL.Empty) (BL.Codegen) (Tensor.Int) (BL.Bool) (R) (R_storage) (BL.Int)
    (BL.Exn)
    (BL.Sequencing)
    (BL.Product)
module Float = Make_native (BL.Float) (BL.Float_storage)
module Rational = Make_native (BL.Rational) (BL.Make_storage (BL.Rational))
