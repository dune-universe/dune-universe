module OCSI = OCamlStandard.Nativeint

module Self = StandardInt.Make(struct
  include OCSI

  let name = "NativeInt"
  let repr_suffix = "n"

#if OCAML_VERSION < (4, 3, 0)
  let equal = Equate.Poly.equal
#endif
end)

include Self

module Examples = struct
  let of_string = [
    ("43", 43n);
    ("-12", -12n);
  ]

  let repr = [
    (-3n, "-3n");
    (-0n, "0n");
    (0n, "0n");
    (1n, "1n");
    (15n, "15n");
  ]

  let to_string = [
    (-3n, "-3");
    (-0n, "0");
    (0n, "0");
    (1n, "1");
    (15n, "15");
  ]

  let equal = [
    [0n];
    [1n];
    [2n];
  ]

  let different = [
    (0n, 1n);
    (1n, -1n);
  ]

  let ordered = [
    [-10n; -5n; -1n; 0n; 1n; 2n; 5n];
  ]

  let add_substract = [
    (4n, 3n, 7n);
    (4n, -2n, 2n);
    (5n, -7n, -2n);
  ]

  let negate = [
    (4n, -4n);
    (-7n, 7n);
  ]

  let multiply = [
    (4n, 3n, 12n);
    (4n, -3n, -12n);
    (-4n, -3n, 12n);
  ]

  let divide = [
    (5n, 2n, 2n);
    (4n, 2n, 2n);
    (4n, 3n, 1n);
    (4n, 4n, 1n);
    (4n, 5n, 0n);
  ]

  let exponentiate = [
    (3n, 3, 27n);
    (2n, 7, 128n);
  ]

  let succ = [
    (1n, 2n);
    (42n, 43n);
    (-121n, -120n);
  ]
end

module Tests = struct
  open Testing

  let test = "NativeInt" >:: [
    (let module T = Concepts.Integer.Tests.Make0(Self)(Examples) in T.test);
    (let module T = Traits.Parsable.Tests.Make0(Self)(Examples) in T.test);
    "exponentiate 2n (-4)" >: (lazy (expect_exception ~expected:(Exception.InvalidArgument "NativeInt.exponentiate: Negative exponent: -4") (lazy (exponentiate 2n (-4)))));
  ]
end
