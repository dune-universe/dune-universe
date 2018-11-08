module Self = Foundations.IntRange

include Self

module Tests = struct
  open Testing

  (* @feature Comparable: compare ranges as equal if to_list produces the same integers in the same order, not if they have the same start, stop and step. Poly.equal wouldn't work. *)

  module Examples = struct
    let repr = [
      (make 0, "[]");
      (make (-5), "[]");
      (make 5, "[0 to 4 step 1]");
      (make ~step:(-3) (-15), "[0 down to -12 step -3]");
      (make ~start:3 ~step:4 25, "[3 to 23 step 4]");
    ]

    let equal = [
      [make 0; make ~step:2 0; make (-4); make 10 ~step:0; make 10 ~step:(-1); make ~start:5 3; empty];
      [make 10; make ~start:0 ~step:1 10];
      [make ~start:5 ~step:3 9; make ~start:5 ~step:3 10; make ~start:5 ~step:3 11]
    ]

    let different = [
      (make 0, make 1);
      (make ~start:5 ~step:3 11, make ~start:5 ~step:3 12);
    ]
  end

  let test = "IntRange" >:: [
    (let module T = Traits.Representable.Tests.Make0(Self)(Examples) in T.test);
    (let module T = Traits.Equatable.Tests.Make0(Self)(Examples) in T.test);
    "to_list" >:: [
      "simplest" >: (lazy (check_int_list ~expected:[0; 1; 2; 3; 4] (to_list (make 5))));
      "with start" >: (lazy (check_int_list ~expected:[2; 3; 4] (to_list (make 5 ~start:2))));
      "with stop < 0" >: (lazy (check_int_list ~expected:[] (to_list (make (-5)))));
      "with stop <= start" >: (lazy (check_int_list ~expected:[] (to_list (make 0))));
      "with stop <= start" >: (lazy (check_int_list ~expected:[] (to_list (make (-5)))));
      "with stop <= start" >: (lazy (check_int_list ~expected:[] (to_list (make 5 ~start:5))));
      "with step" >: (lazy (check_int_list ~expected:[0; 2; 4] (to_list (make 5 ~step:2))));
      "with step" >: (lazy (check_int_list ~expected:[0; 2; 4] (to_list (make 6 ~step:2))));
      "with step = 0" >: (lazy (check_int_list ~expected:[] (to_list (make 6 ~step:0))));
      "with start and step" >: (lazy (check_int_list ~expected:[2; 4; 6] (to_list (make ~start:2 ~step:2 7))));
      "with start and step" >: (lazy (check_int_list ~expected:[2; 4; 6] (to_list (make ~start:2 ~step:2 8))));
      "with start and step" >: (lazy (check_int_list ~expected:[3; 5; 7] (to_list (make ~start:3 ~step:2 8))));
      "with start and step" >: (lazy (check_int_list ~expected:[3; 5; 7] (to_list (make ~start:3 ~step:2 9))));
      "with step < 0" >: (lazy (check_int_list ~expected:[] (to_list (make 6 ~step:(-1)))));
      "with step < 0" >: (lazy (check_int_list ~expected:[0; -1; -2; -3; -4; -5] (to_list (make (-6) ~step:(-1)))));
      "with step < 0" >: (lazy (check_int_list ~expected:[0; -2; -4] (to_list (make (-6) ~step:(-2)))));
      "with step < 0 and start" >: (lazy (check_int_list ~expected:[-3; -5; -7; -9] (to_list (make (-10) ~start:(-3) ~step:(-2)))));
      "with step < 0 and start" >: (lazy (check_int_list ~expected:[-3; -5; -7; -9] (to_list (make (-11) ~start:(-3) ~step:(-2)))));
      (* "fail" >: (lazy (fail "fail")); *)
    ];
  ]
end
