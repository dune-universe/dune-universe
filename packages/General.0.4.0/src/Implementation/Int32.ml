module OCSI = OCamlStandard.Int32

module Self = StandardInt.Make(struct
  include OCSI

  let name = "Int32"
  let repr_suffix = "l"

#if OCAML_VERSION < (4, 3, 0)
  let equal = Equate.Poly.equal
#endif
end)

include Self

module Examples = struct
  let of_string = [
    ("43", 43l);
    ("-12", -12l);
  ]

  let repr = [
    (-3l, "-3l");
    (-0l, "0l");
    (0l, "0l");
    (1l, "1l");
    (15l, "15l");
  ]

  let to_string = [
    (-3l, "-3");
    (-0l, "0");
    (0l, "0");
    (1l, "1");
    (15l, "15");
  ]

  let equal = [
    [0l];
    [1l];
    [2l];
  ]

  let different = [
    (0l, 1l);
    (1l, -1l);
  ]

  let ordered = [
    [-10l; -5l; -1l; 0l; 1l; 2l; 5l];
  ]

  let add_substract = [
    (4l, 3l, 7l);
    (4l, -2l, 2l);
    (5l, -7l, -2l);
  ]

  let negate = [
    (4l, -4l);
    (-7l, 7l);
  ]

  let multiply = [
    (4l, 3l, 12l);
    (4l, -3l, -12l);
    (-4l, -3l, 12l);
  ]

  let divide = [
    (5l, 2l, 2l);
    (4l, 2l, 2l);
    (4l, 3l, 1l);
    (4l, 4l, 1l);
    (4l, 5l, 0l);
  ]

  let exponentiate = [
    (3l, 3, 27l);
    (2l, 7, 128l);
  ]

  let succ = [
    (1l, 2l);
    (42l, 43l);
    (-121l, -120l);
  ]
end

module Tests = struct
  open Testing

  let test = "Int32" >:: [
    (let module T = Concepts.Integer.Tests.Make0(Self)(Examples) in T.test);
    (let module T = Traits.Parsable.Tests.Make0(Self)(Examples) in T.test);
    "exponentiate 2l (-4)" >: (lazy (expect_exception ~expected:(Exception.InvalidArgument "Int32.exponentiate: Negative exponent: -4") (lazy (exponentiate 2l (-4)))));
  ]
end
