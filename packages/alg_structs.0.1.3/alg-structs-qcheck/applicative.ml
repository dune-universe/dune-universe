open QCheck
open Utils

module Alg = Alg_structs

module type S = sig
  include Alg.Applicative.S
  val name : string
  val arbitrary : 'a arbitrary -> 'a t arbitrary
end

let test (module A : S) =
  let module Law = Alg.Applicative.Law(A) in
  let comp_law (Fun (_, f), Fun (_, g), x) = Law.composition (A.return f) (A.return g) x in
  let homo_law (Fun (_, f), x) = Law.homomorphism f x in
  let inter_law (Fun (_, f), x) = Law.interchange (A.return f) x
  in
  [ test_law
      "Applicative" A.name "Identity - [return id <*> x = x] for ints"
      (A.arbitrary int)
      Law.identity;

    test_law
      "Applicative" A.name "Identity - [return id <*> x = x] for int lists"
      (A.arbitrary (list int))
      Law.identity;

    test_law
      "Applicative" A.name "Composition - [(return (.) <*> u <*> v <*> w) = (u <*> (v <*> w))] for int"
      (triple int_fun int_fun (A.arbitrary int))
      comp_law;

    test_law
      "Applicative" A.name "Composition - [(return (.) <*> u <*> v <*> w) = (u <*> (v <*> w))] for int list"
      (triple int_list_fun int_list_fun (A.arbitrary (list int)))
      comp_law;

    test_law
      "Applicative" A.name "Homomorphism - [return f <*> return x = return (f x)] for int"
      (pair int_fun int)
      homo_law;

    test_law
      "Applicative" A.name "Homomorphism - [return f <*> return x = return (f x)] for int list"
      (pair int_list_fun (list int))
      homo_law;

    test_law
      "Applicative" A.name "Interchange - [(u <*> return y) = (return (fun f -> f y) <*> u)] for int"
      (pair int_fun int)
      inter_law;

    test_law
      "Applicative" A.name "Interchange - [(u <*> return y) = (return (fun f -> f y) <*> u)] for int list"
      (pair int_list_fun (list int))
      inter_law;
  ]

let tests ts = make_tests test ts
