open Linalg
module T = Proptest

module Make_vec_tests
    (N : T.Generic.Name)
    (Ring : Basic_intf.Lang.Ring with type 'a m = 'a)
    (S : Basic_intf.Lang.Shape with type 'a m = 'a)
    (V : Intf.Vec
           with type 'a k = 'a
            and type 'a m = 'a
            and type elt = Ring.t
            and type 'a shape = 'a S.t)
    (L : sig
      type index

      val length : index V.shape
    end)
    (V_gen : T.Generic.Generator with type t = L.index V.t)
    (K : T.Generic.Generator with type t = L.index)
    (Ring_gen : T.Generic.Generator with type t = V.elt) =
struct
  (* Construct a Q-module based on Linalg.Vec *)
  module V_mod = struct
    module R = Ring

    type t = L.index V.t

    let zero = V.const L.length R.zero

    let one = V.const L.length R.one

    let add = V.add

    let neg = V.neg

    let smul = V.smul

    let mul = V.mul
  end

  module Module_properties = T.Algebra.Module (N) (Ring_gen) (V_gen) (V_mod)
  module Monoid_properties = T.Algebra.Monoid (N) (V_gen) (V_mod)

  let reduce = V.reduce

  (* let reduce :
   *     type acc elt.
   *     (acc -> elt -> acc) -> acc -> (I.t, elt) Linalg.Intf.vec -> acc =
   *   fun (type acc) f (init : acc) (Linalg.Intf.Vec (n, v)) ->
   *    let length = n in
   *    let module Storage = Basic_impl.Lang.Make_storage (struct
   *      type t = acc
   *    end) in
   *    I.fold
   *      (module Storage)
   *      (fun i acc ->
   *        let vi = v i in
   *        f acc vi)
   *      length
   *      init *)

  let n s = Printf.sprintf "%s: %s" N.name s

  let () =
    T.Generic.add_test
      (n "swap involutive")
      Crowbar.(
        map [V_gen.gen; K.gen; K.gen] @@ fun vec i1 i2 ->
        V_gen.(V.swap i1 i2 (V.swap i1 i2 vec) = vec))

  let () =
    T.Generic.add_test
      (n "reduce invariant by swap")
      Crowbar.(
        map [V_gen.gen; K.gen; K.gen] @@ fun vec i1 i2 ->
        Ring_gen.(
          reduce Ring.add Ring.zero (V.swap i1 i2 vec)
          = reduce Ring.add Ring.zero vec))

  let () =
    T.Generic.add_test
      (n "reduce R.add commutes with V.add")
      Crowbar.(
        map [V_gen.gen] (fun v ->
            Ring_gen.(
              reduce Ring.add Ring.zero (V.add v v)
              = Ring.add
                  (reduce Ring.add Ring.zero v)
                  (reduce Ring.add Ring.zero v))))

  let () =
    T.Generic.add_test
      (n "basis is zero except at one index")
      Crowbar.(
        map [K.gen; K.gen] (fun i j ->
            if Ring_gen.(V.get (V.basis L.length i Ring.one) j = Ring.one) then
              K.(i = j)
            else true))

  let () =
    T.Generic.add_test
      (n "dot (basis i) (basis j) <> 0 iff i = j")
      Crowbar.(
        map [K.gen; K.gen] (fun i j ->
            let v1 = V.basis L.length i Ring.one in
            let v2 = V.basis L.length j Ring.one in
            if Ring_gen.(V.dot v1 v2 = Ring.one) then K.(i = j) else true))
end

module Make_array_backed_tests
    (N : T.Generic.Name)
    (Ring : Basic_intf.Lang.Ring with type 'a m = 'a)
    (S : Intf.Tensor with type 'a k = 'a and type 'a m = 'a and type pos = int)
    (V : Intf.Vec
           with type 'a k = 'a
            and type 'a m = 'a
            and type elt = Ring.t
            and type 'a shape = 'a S.t)
    (A : Basic_intf.Lang.Array
           with type 'a m = 'a
            and type index = int
            and type elt = Ring.t)
    (L : sig
      type index = int

      val length : int S.t
    end)
    (K : T.Generic.Generator with type t = int)
    (Ring_gen : T.Generic.Generator with type t = V.elt) =
struct
  module BL = Basic_impl.Lang

  module Array_helpers =
    T.Array.Helpers (BL.Empty) (BL.Sequencing) (A)
      (struct
        let length = S.dim L.length S.Path.empty
      end)
      (K)
      (Ring_gen)

  module Array_backed = Vec.Array_backed (BL.Empty) (BL.Codegen) (S) (A)

  let reduce :
      type acc elt.
      (acc -> elt -> acc) -> acc -> ('i S.t, 'i, elt) Intf.vec -> acc =
    fun (type acc) f (init : acc) (Intf.Vec (s, v)) ->
     let module Storage = BL.Make_storage (struct
       type t = acc
     end) in
     S.fold
       (module Storage)
       (fun i acc ->
         let open BL.Sequencing in
         let* vi = v i in
         f acc vi)
       s
       init

  module V_gen = struct
    type t = int V.t

    let gen = Crowbar.map [Array_helpers.array_gen] Array_backed.in_of_array

    let ( = ) (v1 : 'a V.t) (v2 : 'a V.t) =
      reduce ( && ) true (V.map2 Ring_gen.( = ) v1 v2)
  end

  include Make_vec_tests (N) (Ring) (S) (V) (L) (V_gen) (K) (Ring_gen)
end

module Q_array = Basic_impl.Lang.Make_array (Basic_impl.Lang.Rational)

type t = Q_array.t

module N = struct
  let name = "Linalg.Vec"
end

module Q_tests =
  Make_array_backed_tests (N) (Basic_impl.Lang.Rational) (Tensor.Int)
    (Vec.Rational)
    (Q_array)
    (struct
      type index = int

      let length = Tensor.Int.rank_one 10
    end)
    (struct
      type t = int

      let gen = Crowbar.range 10

      let ( = ) = Int.equal
    end)
    (T.Generators.Q)
