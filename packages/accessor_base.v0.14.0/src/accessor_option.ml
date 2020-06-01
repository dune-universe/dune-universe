open! Base
open! Import

type 'a t = 'a Option.t =
  | None
  | Some of 'a
[@@deriving accessors]

let default default ~is_default =
  Accessor.isomorphism
    ~get:(function
      | None -> default
      | Some a -> a)
    ~construct:(fun b -> if is_default b then None else Some b)
;;

include Accessor.Of_monad (struct
    include Option

    let apply = `Define_using_bind
  end)
