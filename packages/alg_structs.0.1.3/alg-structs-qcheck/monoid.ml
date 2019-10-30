open QCheck
open Utils

module Alg = Alg_structs

module type S = sig
  include Alg.Monoid.S
  val name : string
  val arbitrary : t arbitrary
end

let test (module M : S) =
  let module Law = Alg.Monoid.Law(M)
  in
  Semigroup.test (module M) @
  [
    test_law
      "Monoid" M.name "Right Cancellation - [(x * unit) = x]"
      M.arbitrary
      Law.unit_right_cancelation;

    test_law
      "Monoid" M.name "Left Cancellation - [(unit * x) = x]"
      M.arbitrary
      Law.unit_left_cancelation;

    test_law
      "Monoid" M.name "Equivalence of mconcat and right fold"
      (list M.arbitrary)
      Law.mconcat_is_a_fold_right;
  ]

let tests ts = make_tests test ts
