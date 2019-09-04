module SelfA = struct
  include Foundations.Int

  include Traits.Ringoid.Exponentiate.Make0(struct
    include Foundations.Int

    let exponentiate_negative_exponent ~exponentiate:_ _ n =
      Exception.invalid_argument "Int.exponentiate: Negative exponent: %i" n
  end)
end

module SelfB = struct
  module O = struct
    include SelfA.O

    let ( ** ) = SelfA.exponentiate
  end

  include (SelfA: module type of SelfA[@remove_aliases] with module O := O)
end

include SelfB

module Examples = struct
  let repr = [
    (-3, "-3");
    (-0, "0");
    (0, "0");
    (1, "1");
    (15, "15");
  ]

  let to_string = repr

  let of_string = [
    ("0", 0);
    ("1", 1);
    ("-1", -1);
    ("1_000", 1000);
  ]

  let equal = [
    [0];
    [1];
    [2];
  ]

  let different = [
    (0, 1);
    (1, -1);
  ]

  let ordered = [
    [-10; -5; -1; 0; 1; 2; 5];
  ]

  let add_substract = [
    (4, 3, 7);
    (4, -2, 2);
    (5, -7, -2);
  ]

  let negate = [
    (4, -4);
    (-7, 7);
  ]

  let multiply = [
    (4, 3, 12);
    (4, -3, -12);
    (-4, -3, 12);
  ]

  let divide = [
    (5, 2, 2);
    (4, 2, 2);
    (4, 3, 1);
    (4, 4, 1);
    (4, 5, 0);
  ]

  let exponentiate = [
    (3, 3, 27);
    (2, 7, 128);
  ]

  let succ = [
    (1, 2);
    (42, 43);
    (-121, -120);
  ]
end

module Tests = struct
  open Testing

  let test = "Int" >:: [
    (let module T = Concepts.Integer.Tests.Make0(SelfB)(Examples) in T.test);
    (let module T = Traits.Parsable.Tests.Make0(SelfB)(Examples) in T.test);
    "exponentiate 2 (-4)" >: (lazy (expect_exception ~expected:(Exception.InvalidArgument "Int.exponentiate: Negative exponent: -4") (lazy (exponentiate 2 (-4)))));
  ]
end
