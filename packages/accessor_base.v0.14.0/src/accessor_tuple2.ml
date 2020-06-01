open! Base
open! Import

let fst = [%accessor Accessor.field ~get:fst ~set:(fun (_, c) b -> b, c)]

let fsti =
  [%accessor Accessor.fieldi ~get:(fun (a, b) -> b, a) ~set:(fun (_, c) b -> b, c)]
;;

let snd = [%accessor Accessor.field ~get:snd ~set:(fun (c, _) b -> c, b)]
let sndi = [%accessor Accessor.fieldi ~get:Fn.id ~set:(fun (c, _) b -> c, b)]

let swap =
  let swap (a, b) = b, a in
  [%accessor Accessor.isomorphism ~get:swap ~construct:swap]
;;

let assocl (a, (b, c)) = (a, b), c
and assocr ((a, b), c) = a, (b, c)

let assocl = [%accessor Accessor.isomorphism ~get:assocl ~construct:assocr]
and assocr = [%accessor Accessor.isomorphism ~get:assocr ~construct:assocl]

let each =
  [%accessor
    Accessor.nonempty (fun (a, b) ->
      let open Accessor.Nonempty.Let_syntax in
      let%map_open a = access a
      and b = access b in
      a, b)]
;;

module Fst = Accessor.Of_functor2 (struct
    type ('fst, 'snd) t = 'fst * 'snd

    let map (a, b) ~f = f a, b
  end)

module Snd = Accessor.Of_functor2 (struct
    type ('snd, 'fst) t = 'fst * 'snd

    let map (a, b) ~f = a, f b
  end)
