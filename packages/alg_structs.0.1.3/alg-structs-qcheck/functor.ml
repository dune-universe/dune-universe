open QCheck
open Utils

module Alg = Alg_structs

module type S = sig
  include Alg.Functor.S
  val name : string
  val arbitrary : 'a arbitrary -> 'a t arbitrary
end

let test (module F : S) =
  let module Law = Alg.Functor.Law(F) in
  let comp_law (Fun (_, f), Fun (_, g), x) =
    Law.composition f g x
  in
  [
    test_law
      "Functor" F.name "Identity - [map id = id] for int"
      (F.arbitrary int)
      Law.identity;

    test_law
      "Functor" F.name "Identity - [map id = id] for int list"
      (F.arbitrary (list int))
      Law.identity;

    test_law
      "Functor" F.name "Composition - [map (f . g) = (map f) . (map g)] for int"
      (triple int_fun int_fun (F.arbitrary int))
      comp_law;

    test_law "Functor" F.name "Composition - [map (f . g) = (map f) . (map g)] for int list"
      (triple int_list_fun int_list_fun (F.arbitrary (list int)))
      comp_law;
  ]

let tests ts = make_tests test ts
