open! Core_kernel
open! Import

let empty =
  [%accessor
    Accessor.variant
      ~match_:(fun dq -> if Fdeque.is_empty dq then First () else Second dq)
      ~construct:(fun () -> Fdeque.empty)]
;;

let front =
  [%accessor
    Accessor.variant
      ~match_:(fun dq ->
        match Fdeque.dequeue_front dq with
        | None -> Second dq
        | Some (a, dq) -> First (a, dq))
      ~construct:(fun (a, dq) -> Fdeque.enqueue_front dq a)]
;;

let back =
  [%accessor
    Accessor.variant
      ~match_:(fun dq ->
        match Fdeque.dequeue_back dq with
        | None -> Second dq
        | Some (a, dq) -> First (dq, a))
      ~construct:(fun (dq, a) -> Fdeque.enqueue_back dq a)]
;;

let first = [%accessor front @> Accessor.Tuple2.fst]
let last = [%accessor back @> Accessor.Tuple2.snd]
let reversed = [%accessor Accessor.isomorphism ~get:Fdeque.rev ~construct:Fdeque.rev]
let list = [%accessor Accessor.isomorphism ~get:Fdeque.to_list ~construct:Fdeque.of_list]
let each = [%accessor list @> Accessor.List.each]
let eachi = [%accessor list @> Accessor.List.eachi]

include Accessor.Of_monad (struct
    include Fdeque

    let apply = `Define_using_bind
  end)
