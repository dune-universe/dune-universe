open Intf

(* Generic operations *)
module Generic = struct
  let vec n f = Vec (n, f)

  let ovec n f = OVec (n, f)

  let map f (Vec (n, v)) = Vec (n, fun x -> f (v x))

  let mapi f (Vec (n, v)) = Vec (n, fun x -> f x (v x))
end

module Make_internal
    (Repr : Basic_intf.Lang.Empty)
    (Monad : Basic_intf.Codegen_monad with type 'a m = 'a Repr.m)
    (S : Basic_intf.Lang.Shape with type 'a m = 'a Repr.m)
    (B : Basic_intf.Lang.Bool with type 'a m = 'a Repr.m)
    (R : Basic_intf.Lang.Ring with type 'a m = 'a Repr.m)
    (R_storage : Basic_intf.Lang.Storage
                   with type 'a m = 'a Repr.m
                    and type elt = R.t)
    (E : Basic_intf.Lang.Exn with type 'a m = 'a Repr.m)
    (M : Basic_intf.Lang.Sequencing with type 'a m = 'a Repr.m) =
struct
  type 'a k = 'a Monad.t

  type 'a m = 'a Repr.m

  type 'a shape = 'a S.t

  type ('a, 'b) morphism = ('a, 'b) S.Morphism.t

  type elt = R.t

  type 'a t = ('a shape, 'a m, elt m) vec

  type 'a out = ('a shape, 'a m, elt m, unit m) ovec

  open Monad.Infix

  let idim (Vec (s, _)) = s

  let odim (OVec (s, _)) = s

  let make s f = Generic.vec s f

  let pullback m (Vec (s, v)) =
    let u = S.Morphism.underlying m in
    let d = S.Morphism.domain m in
    let*! _ =
      B.dispatch (S.equal s (S.Morphism.range m)) @@ function
      | false -> E.raise_ Dimensions_mismatch
      | true -> M.unit
    in
    Monad.return (Vec (d, fun x -> v (u x)))

  let get (Vec (s, v)) i =
    let*! _ =
      B.dispatch (S.mem s i) (function
          | false -> E.raise_ Out_of_bounds
          | true -> M.unit)
    in
    Monad.return (v i)

  let unsafe_get (Vec (_, v)) i = v i

  let set (OVec (s, v)) i e =
    let*! _ =
      B.dispatch (S.mem s i) (function
          | false -> E.raise_ Out_of_bounds
          | true -> M.unit)
    in
    Monad.return (v i e)

  let map f (Vec (s, v)) = Vec (s, fun x -> f (v x))

  let mapi f (Vec (s, v)) = Vec (s, fun x -> f x (v x))

  let unsafe_map2 f s (Vec (_, v1)) (Vec (_, v2)) =
    Vec
      ( s,
        fun i ->
          let open M in
          let* v1i = v1 i in
          let* v2i = v2 i in
          f v1i v2i )
    [@@inline]

  let map2 f vec1 vec2 =
    let s1 = idim vec1 in
    let s2 = idim vec2 in
    let*! _ =
      B.dispatch (S.equal s1 s2) @@ function
      | false -> E.raise_ Dimensions_mismatch
      | true -> M.unit
    in
    Monad.return (unsafe_map2 f s1 vec1 vec2)

  let map2i f (Vec (s1, v1)) (Vec (s2, v2)) =
    let*! _ =
      B.dispatch S.(equal s1 s2) @@ function
      | false -> E.raise_ Dimensions_mismatch
      | true -> M.unit
    in
    Monad.return
      (Vec
         ( s1,
           fun i ->
             let open M in
             let* v1i = v1 i in
             let* v2i = v2 i in
             f i v1i v2i ))

  let assign :
      ('i shape, 'i m, 'a M.m, unit M.m) ovec ->
      ('i shape, 'i m, 'a M.m) vec ->
      ('i shape, 'i m, unit M.m) vec k =
   fun (OVec (s1, fo)) (Vec (s2, f)) ->
    let*! _ =
      B.dispatch (S.equal s1 s2) @@ function
      | false -> E.raise_ Dimensions_mismatch
      | true -> M.unit
    in
    Monad.return
      (Vec
         ( s1,
           fun i ->
             let vi = f i in
             fo i vi ))

  let zero s = Vec (s, fun _ -> R.zero)

  let one s = Vec (s, fun _ -> R.one)

  let const s r = Vec (s, fun _ -> r)

  let basis s i r =
    let*! _ =
      B.dispatch (S.mem s i) @@ function
      | false -> E.raise_ Out_of_bounds
      | true -> M.unit
    in
    Monad.return
      (Vec
         ( s,
           fun j ->
             B.dispatch (S.pos_equal s j i) @@ function
             | true -> r
             | false -> R.zero ))

  let add (v1 : 'a t) (v2 : 'a t) = map2 R.add v1 v2

  let sub (v1 : 'a t) (v2 : 'a t) =
    map2 (fun r1 r2 -> R.add r1 (R.neg r2)) v1 v2

  let unsafe_mul n (v1 : 'a t) (v2 : 'a t) = unsafe_map2 R.mul n v1 v2

  let mul (v1 : 'a t) (v2 : 'a t) = map2 R.mul v1 v2

  let neg (Vec (n, f)) =
    Vec
      ( n,
        fun i ->
          M.(
            let* vi = f i in
            R.neg vi) )

  let smul r (Vec (n, f)) =
    Vec
      ( n,
        fun i ->
          M.(
            let* vi = f i in
            R.mul r vi) )

  let swap i1 i2 (Vec (s, f)) =
    let*! _ =
      B.dispatch B.(S.mem s i1 && S.mem s i2) @@ function
      | false -> E.raise_ Out_of_bounds
      | true -> M.unit
    in
    let vecfun (i : 'a m) =
      B.dispatch (S.pos_equal s i i1) @@ function
      | true -> f i2
      | false -> (
          B.dispatch (S.pos_equal s i i2) @@ function
          | true -> f i1
          | false -> f i)
    in
    Monad.return (Vec (s, vecfun))

  let reduce_generic :
      type acc elt index.
      acc S.storage ->
      (acc m -> elt m -> acc m) ->
      acc m ->
      (index shape, index m, elt m) vec ->
      acc m =
    fun (type acc) storage f (init : acc m) (Vec (s, v)) ->
     S.fold
       storage
       (fun i acc ->
         let open M in
         let* vi = v i in
         f acc vi)
       s
       init

  let reduce f init vec = reduce_generic (module R_storage) f init vec

  let iter (Vec (s, v)) = S.iter v s

  let ( := ) vout vin =
    let* result = assign vout vin in
    Monad.return (iter result)

  let add_ vout v1 v2 =
    let* res = add v1 v2 in
    vout := res

  let sub_ vout v1 v2 =
    let* res = sub v1 v2 in
    vout := res

  let mul_ vout v1 v2 =
    let* res = mul v1 v2 in
    vout := res

  let dot v1 v2 =
    let* res = mul v1 v2 in
    Monad.return (reduce R.add R.zero res)

  let unsafe_dot n v1 v2 =
    let vec = unsafe_mul n v1 v2 in
    reduce R.add R.zero vec

  module Infix = struct
    let ( ~! ) = Monad.return

    let ( + ) x y =
      let* x = x in
      let* y = y in
      add x y

    let ( - ) x y =
      let* x = x in
      let* y = y in
      sub x y

    let ( * ) x y =
      let* x = x in
      let* y = y in
      mul x y

    let ( ~- ) vec = Monad.lift1 neg vec

    let ( %* ) x y =
      let* y = y in
      Monad.return (smul x y)

    let ( <*> ) x y =
      let* x = x in
      let* y = y in
      dot x y

    let ( .%{} ) x y =
      let* x = x in
      get x y

    let ( .%{}<- ) x y z = set x y z

    let ( := ) x y =
      let* y = y in
      x := y
  end
end
[@@inline]

module Make : functor
  (Repr : Basic_intf.Lang.Empty)
  (Monad : Basic_intf.Codegen_monad with type 'a m = 'a Repr.m)
  (S : Basic_intf.Lang.Shape with type 'a m = 'a Repr.m)
  (B : Basic_intf.Lang.Bool with type 'a m = 'a Repr.m)
  (R : Basic_intf.Lang.Ring with type 'a m = 'a Repr.m)
  (R_storage : Basic_intf.Lang.Storage
                 with type 'a m = 'a Repr.m
                  and type elt = R.t)
  (E : Basic_intf.Lang.Exn with type 'a m = 'a Repr.m)
  (M : Basic_intf.Lang.Sequencing with type 'a m = 'a Repr.m)
  ->
  Intf.Vec
    with type 'a k = 'a Monad.t
     and type 'a m = 'a Repr.m
     and type 'a shape = 'a S.t
     and type ('a, 'b) morphism = ('a, 'b) S.Morphism.t
     and type elt = R.t =
  Make_internal

module Array_backed
    (Repr : Basic_intf.Lang.Empty)
    (Monad : Basic_intf.Codegen_monad with type 'a m = 'a Repr.m)
    (S : Intf.Tensor with type 'a m = 'a Repr.m)
    (A : Basic_intf.Lang.Array with type index = S.pos and type 'a m = 'a Repr.m) =
struct
  let of_array (a : A.t Repr.m) =
    let n = A.length a in
    let shape = S.rank_one n in
    let vec = Vec (shape, fun i -> A.unsafe_get a i) in
    let ovec = OVec (shape, fun i v -> A.unsafe_set a i v) in
    (vec, ovec)

  let in_of_array (a : A.t Repr.m) =
    let n = A.length a in
    let shape = S.rank_one n in
    Vec (shape, fun i -> A.unsafe_get a i)

  let out_of_array (a : A.t Repr.m) =
    let n = A.length a in
    let shape = S.rank_one n in
    OVec (shape, fun i v -> A.unsafe_set a i v)
end
[@@inline]

(* Instantiate some typical schemes *)

module BL = Basic_impl.Lang
module Make_native
    (R : Basic_intf.Lang.Ring with type 'a m = 'a)
    (R_storage : Basic_intf.Lang.Storage with type 'a m = 'a and type elt = R.t) =
  Make_internal (BL.Empty) (BL.Codegen) (Tensor.Int) (BL.Bool) (R) (R_storage)
    (BL.Exn)
    (BL.Sequencing)
module Float = Make_native (BL.Float) (BL.Float_storage)
module Rational = Make_native (BL.Rational) (BL.Make_storage (BL.Rational))
