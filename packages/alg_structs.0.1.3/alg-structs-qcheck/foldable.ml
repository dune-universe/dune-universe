open QCheck
open Utils

module Alg = Alg_structs

module type S = sig
  include Alg.Foldable.S
  val name : string
  val arbitrary : 'a arbitrary -> 'a t arbitrary
end

let test (module F : S) =
  let module Law = Alg.Foldable.Law(F) in
  let fold_right_law (Fun (_, f), x, t) =
    Law.fold_right f x t
  in
  [ test_law "Foldable" F.name "fold_right for int"
      (triple int_int_fun int (F.arbitrary int))
      fold_right_law
  ]

let tests ts = make_tests test ts
