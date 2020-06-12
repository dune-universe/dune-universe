open! Core_kernel
open! Import

let empty =
  [%accessor
    Accessor.variant
      ~match_:(fun dq -> if Fqueue.is_empty dq then First () else Second dq)
      ~construct:(fun () -> Fqueue.empty)]
;;

let list = [%accessor Accessor.isomorphism ~get:Fqueue.to_list ~construct:Fqueue.of_list]
let each = [%accessor list @> Accessor.List.each]
let eachi = [%accessor list @> Accessor.List.eachi]

include Accessor.Of_monad (struct
    include Fqueue

    let apply = `Define_using_bind
  end)
