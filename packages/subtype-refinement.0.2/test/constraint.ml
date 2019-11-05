open OUnit2
open Subtype_refinement

module Natural = Refine (struct
  type t = int

  let where value = value >= 0
end)

module Zero = Singleton (struct
  type t = int

  let value = 0
end)

module NonZero = (val refine ((!=) 0) : Subtype with type super = int)

let __natural_downcast _ =
  assert_equal (Natural.downcast 1) (Natural.downcast 1)

let __failed_natural_downcast _ =
  let negative      = -1 in
  let failure       = Natural.FailedDownCast negative in
  let procedure ( ) = Natural.downcast negative in
  assert_raises failure procedure

let __zero_downcast _ =
  assert_equal (Zero.downcast 0) (Zero.downcast 0)

let __failed_zero_downcast _ =
  let nonzero       = 1 in
  let failure       = Zero.FailedDownCast nonzero in
  let procedure ( ) = Zero.downcast nonzero in
  assert_raises failure procedure

let __upcast _ =
  let natural = Natural.downcast 0 in
  let zero    = Zero.downcast 0 in
  assert_equal (natural :> int) (zero :> int)

let (/) m n = m / (NonZero.upcast n)

let __safe_division_succeeds _ =
  let n = 10 / (NonZero.downcast 2) in
  assert_equal n 5

let __safe_division_fails _ =
  let failure       = NonZero.FailedDownCast 0 in
  let procedure ( ) = 10 / (NonZero.downcast 0) in
  assert_raises failure procedure

let suite = "constraint-suite" >::: [
  "natural-downcast"        >:: __natural_downcast;
  "zero-downcast"           >:: __zero_downcast;
  "failed-natural-downcast" >:: __failed_natural_downcast;
  "failed-zero-downcast"    >:: __failed_zero_downcast;
  "upcast"                  >:: __upcast;
  "safe-division-succeeds"  >:: __safe_division_succeeds;
  "safe-division-fails"     >:: __safe_division_fails
]

let _ =
  run_test_tt_main suite

(* END *)
