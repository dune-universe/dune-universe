include Foundations.Bool

module Tests = struct
  open Testing

  module Examples = struct
    let repr = [
      (false, "false");
      (true, "true");
    ]

    let to_string = repr

    let of_string = [
      ("false", false);
      ("true", true);
    ]

    let equal = [
      [true];
      [false];
    ]

    let different = [
      (true, false);
    ]

    let ordered = [
      [false; true];
    ]
  end

  let test = "Bool" >:: [
    (let module T = Concepts.Able.Tests.Make0(Foundations.Bool)(Examples) in T.test);
    (let module T = Traits.Displayable.Tests.Make0(Foundations.Bool)(Examples) in T.test);
    (let module T = Traits.Parsable.Tests.Make0(Foundations.Bool)(Examples) in T.test);
    "not true" >: (lazy (check_false (not true)));
    "not false" >: (lazy (check_true (not false)));

    "and_ true true" >: (lazy (check_true (and_ true true)));
    "and_ false true" >: (lazy (check_false (and_ false true)));
    "and_ true false" >: (lazy (check_false (and_ true false)));
    "and_ false false" >: (lazy (check_false (and_ false false)));
    "true && true" >: (lazy (check_true (true && true)));
    "true && false" >: (lazy (check_false (true && false)));
    "false && whatever" >:(lazy (check_false (false && (Exception.failure "Don't call me")))); (*BISECT-IGNORE*)

    "or_ true true" >: (lazy (check_true (or_ true true)));
    "or_ false true" >: (lazy (check_true (or_ false true)));
    "or_ true false" >: (lazy (check_true (or_ true false)));
    "or_ false false" >: (lazy (check_false (or_ false false)));
    "true || whatever" >:(lazy (check_true (true || (Exception.failure "Don't call me")))); (*BISECT-IGNORE*)
    "false || true" >: (lazy (check_true (false || true)));
    "false || false" >: (lazy (check_false (false || false)));

    "xor true true" >: (lazy (check_false (xor true true)));
    "xor false true" >: (lazy (check_true (xor false true)));
    "xor true false" >: (lazy (check_true (xor true false)));
    "xor false false" >: (lazy (check_false (xor false false)));
  ]
end
