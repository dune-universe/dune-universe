module OCSI = OCamlStandard.Int64

module Self = StandardInt.Make(struct
  include OCSI

  let name = "Int64"
  let repr_suffix = "L"

#if OCAML_VERSION < (4, 3, 0)
  let equal = Equate.Poly.equal
#endif
end)

include Self

module Examples = struct
  let of_string = [
    ("43", 43L);
    ("-12", -12L);
  ]

  let repr = [
    (-3L, "-3L");
    (-0L, "0L");
    (0L, "0L");
    (1L, "1L");
    (15L, "15L");
  ]

  let to_string = [
    (-3L, "-3");
    (-0L, "0");
    (0L, "0");
    (1L, "1");
    (15L, "15");
  ]

  let equal = [
    [0L];
    [1L];
    [2L];
  ]

  let different = [
    (0L, 1L);
    (1L, -1L);
  ]

  let ordered = [
    [-10L; -5L; -1L; 0L; 1L; 2L; 5L];
  ]

  let add_substract = [
    (4L, 3L, 7L);
    (4L, -2L, 2L);
    (5L, -7L, -2L);
  ]

  let negate = [
    (4L, -4L);
    (-7L, 7L);
  ]

  let multiply = [
    (4L, 3L, 12L);
    (4L, -3L, -12L);
    (-4L, -3L, 12L);
  ]

  let divide = [
    (5L, 2L, 2L);
    (4L, 2L, 2L);
    (4L, 3L, 1L);
    (4L, 4L, 1L);
    (4L, 5L, 0L);
  ]

  let exponentiate = [
    (3L, 3, 27L);
    (2L, 7, 128L);
  ]

  let succ = [
    (1L, 2L);
    (42L, 43L);
    (-121L, -120L);
  ]
end

module Tests = struct
  open Testing

  let test = "Int64" >:: [
    (let module T = Concepts.Integer.Tests.Make0(Self)(Examples) in T.test);
    (let module T = Traits.Parsable.Tests.Make0(Self)(Examples) in T.test);
    "exponentiate 2L (-4)" >: (lazy (expect_exception ~expected:(Exception.InvalidArgument "Int64.exponentiate: Negative exponent: -4") (lazy (exponentiate 2L (-4)))));
  ]
end
