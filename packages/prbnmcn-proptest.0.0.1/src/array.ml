(** Generic functors for building property-based tests on array-like structures *)

open Basic_structures
open Generic

module Helpers
    (Repr : Basic_intf.Lang.Empty)
    (M : Basic_intf.Lang.Sequencing with type 'a m = 'a Repr.m)
    (A : Basic_intf.Lang.Array with type 'a m = 'a Repr.m) (L : sig
      val length : A.index Repr.m
    end)
    (K : Generator with type t = A.index Repr.m)
    (V : Generator with type t = A.elt Repr.m) =
struct
  type array = A.t

  type key = A.index Repr.m

  type value = A.elt Repr.m

  type actions = Set of key * value * actions | Nil

  let actions_gen : actions Crowbar.gen =
    let open Crowbar in
    fix (fun actions_gen ->
        choose
          [ map [K.gen; V.gen; actions_gen] (fun k v a -> Set (k, v, a));
            const Nil ])

  let rec interpret actions a =
    let open M in
    match actions with
    | Set (k, v, next) -> seq (A.set a k v) (fun () -> interpret next a)
    | Nil -> M.unit

  let array_gen =
    Crowbar.map [actions_gen; V.gen] (fun actions v0 ->
        M.(
          let* array = A.make L.length v0 in
          let* _ = interpret actions array in
          array))
end

module Array_theory
    (N : Name)
    (Repr : Basic_intf.Lang.Empty)
    (M : Basic_intf.Lang.Sequencing with type 'a m = 'a Repr.m)
    (A : Basic_intf.Lang.Array with type 'a m = 'a Repr.m) (L : sig
      val length : A.index Repr.m
    end)
    (K : Generator with type t = A.index Repr.m)
    (V : Generator with type t = A.elt Repr.m) =
struct
  include Helpers (Repr) (M) (A) (L) (K) (V)

  let () =
    Crowbar.add_test
      ~name:(sf "%s: set k v x; get k x = v" N.name)
      [array_gen; K.gen; V.gen]
      (fun array k v ->
        let open M in
        let v' = seq (A.set array k v) (fun () -> A.get array k) in
        Crowbar.check @@ V.(v = v'))

  let () =
    Crowbar.add_test
      ~name:(sf "%s: forall k != k', set k v x; set k' v' x; get k = v" N.name)
      [array_gen; K.gen; K.gen; V.gen; V.gen]
      (fun array k1 k2 v v' ->
        let open M in
        Crowbar.guard K.(not (k1 = k2)) ;
        let r =
          seq (A.set array k1 v) @@ fun () ->
          seq (A.set array k2 v') @@ fun () -> A.get array k1
        in
        Crowbar.check @@ V.(v = r))
end
