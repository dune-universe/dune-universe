open QCheck
open Utils

module Alg = Alg_structs

module type S = sig
  include Alg.Semigroup.S
  val name : string
  val arbitrary : t arbitrary
end

let test (module S : S) =
  let module Law = Alg.Semigroup.Law(S)
  in
  let three = triple S.arbitrary S.arbitrary S.arbitrary
  in
  [ test_law
      "Semigroup" S.name "Associativity - [(x * (y * z)) = ((x * y) * z)]"
      three
      (fun (x, y, z) -> (Law.associativity x y z))
  ]

let tests ts = make_tests test ts
