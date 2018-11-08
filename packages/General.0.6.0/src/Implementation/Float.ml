module SelfA = struct
  include Foundations.Float

  include Traits.Ringoid.Exponentiate.Make0(struct
    include Foundations.Float

    let exponentiate_negative_exponent ~exponentiate x n =
      exponentiate (divide 1. x) (-n)
  end)

  include Traits.Comparable.Between.Make0(Foundations.Float)
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
    (-3., "-3.");
    (-0., "-0.");
    (0., "0.");
    (1., "1.");
    (15., "15.");
  ]

  let to_string = repr

  let of_string = [
    ("0", 0.);
    ("1", 1.);
    ("1.0", 1.);
    ("-1", -1.);
    ("1_000", 1000.);
  ]

  let equal = [
    [0.];
    [1.];
    [2.];
    [infinity];
    [negative_infinity];
  ]

  let different = [
    (0., 1.);
    (1., -1.);
    (not_a_number, infinity);
    (not_a_number, negative_infinity);
    (not_a_number, 1.);
    (not_a_number, 0.);
    (not_a_number, not_a_number);
  ]

  let ordered = [
    [-10.; -5.; -1.; -0.2; 0.; 0.7; 1.; 2.; 5.];
  ]

  let add_substract = [
    (4., 3., 7.);
    (4., -2., 2.);
    (5., -7., -2.);
  ]

  let negate = [
    (4., -4.);
    (-7., 7.);
  ]

  let multiply = [
    (4., 3., 12.);
    (4., -3., -12.);
    (-4., -3., 12.);
  ]

  let divide = [
    (5., 2., 2.5);
    (4., 2., 2.);
    (1., 4., 0.25);
    (4., 4., 1.);
    (4., 5., 0.8);
  ]

  let exponentiate = [
    (3., 3, 27.);
    (2., 7, 128.);
    (0.5, 4, 0.0625);
    (2., -4, 0.0625);
  ]
end

module ClassExamples = struct
  open SelfB.Class

  let repr = [
    (Normal, "Normal");
    (SubNormal, "SubNormal");
    (Zero, "Zero");
    (Infinite, "Infinite");
    (NotANumber, "NotANumber");
  ]

  let equal = [
    [Normal];
    [SubNormal];
    [Zero];
    [Infinite];
    [NotANumber];
  ]

  let different = [
    (Normal, SubNormal);
  ]

  let ordered = [
    [Normal; SubNormal; Zero; Infinite; NotANumber];
  ]
end

module Tests = struct
  open Testing

  let test = "Float" >:: [
    (let module T = Concepts.RealNumber.Tests.Make0(SelfB)(Examples) in T.test);
    (let module T = Traits.Parsable.Tests.Make0(SelfB)(Examples) in T.test);
    "ceil" >:: (
      let make x expected =
        ~: "%f" x (lazy (check_float_exact ~expected (ceil x)))
      in
      [
        make (-1.) (-1.);
        make (-0.99) 0.;
        make (-0.1) 0.;
        make 0. 0.;
        make 0.01 1.;
        make 0.99 1.;
        make 1. 1.;
      ]
    );
    "Class" >:: [
      (let module T = Traits.Comparable.Tests.Make0(SelfB.Class)(ClassExamples) in T.test);
      (let module T = Traits.Equatable.Tests.Make0(SelfB.Class)(ClassExamples) in T.test);
      (let module T = Traits.Representable.Tests.Make0(SelfB.Class)(ClassExamples) in T.test);
      "of_float" >:: SelfB.Class.(
        let check = check ~repr ~equal in
        [
          "Normal" >: (lazy (check ~expected:Normal (of_float 1.)));
          "SubNormal" >: (lazy (check ~expected:SubNormal (of_float (1. /. greatest))));
          "Zero" >: (lazy (check ~expected:Zero (of_float 0.)));
          "Zero-" >: (lazy (check ~expected:Zero (of_float (-0.))));
          "Infinite+" >: (lazy (check ~expected:Infinite (of_float (1. /. 0.))));
          "Infinite+" >: (lazy (check ~expected:Infinite (of_float infinity)));
          "Infinite-" >: (lazy (check ~expected:Infinite (of_float (-1. /. 0.))));
          "Infinite-" >: (lazy (check ~expected:Infinite (of_float negative_infinity)));
          "NotANumber" >: (lazy (check ~expected:NotANumber (of_float (0. /. 0.))));
          "NotANumber" >: (lazy (check ~expected:NotANumber (of_float not_a_number)));
          "NotANumber-" >: (lazy (check ~expected:NotANumber (of_float (-0. /. 0.))));
        ]
      );
    ];
  ]
end
