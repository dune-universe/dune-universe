open! Base
open! Import

type ('f, 's) t = ('f, 's) Either.t =
  | First of 'f
  | Second of 's
[@@deriving accessors]

let swapped = [%accessor Accessor.isomorphism ~get:Either.swap ~construct:Either.swap]

let assocl = function
  | First a -> First (First a)
  | Second (First a) -> First (Second a)
  | Second (Second a) -> Second a

and assocr = function
  | First (First a) -> First a
  | First (Second a) -> Second (First a)
  | Second a -> Second (Second a)
;;

let assocl = [%accessor Accessor.isomorphism ~get:assocl ~construct:assocr]
and assocr = [%accessor Accessor.isomorphism ~get:assocr ~construct:assocl]

module Index = struct
  type t =
    | First
    | Second
  [@@deriving accessors, compare, hash, sexp_of]
end

let tuple =
  [%accessor
    Accessor.isomorphism
      ~get:(function
        | First a -> Index.First, a
        | Second a -> Second, a)
      ~construct:(function
        | Index.First, a -> First a
        | Second, a -> Second a)]
;;

let each = [%accessor tuple @> Accessor_tuple2.snd]
let eachi = [%accessor tuple @> Accessor_tuple2.sndi]

module First = Accessor.Of_monad2 (struct
    include Either.First

    let apply = `Custom apply
  end)

module Second = Accessor.Of_monad2 (struct
    include Either.Second

    let apply = `Custom apply
  end)
