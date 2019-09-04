include Foundations.Lazy

open Testing

module Tests = struct
  let test = "Lazy" >:: [
    "value" >: (lazy (check_42 (value (lazy 42))));
    "value exc" >: (lazy (expect_exception ~expected:(Exception.Failure "nope") (lazy (value (lazy (Exception.failure "nope"))))));
    "is_value" >: (lazy (
      let x = lazy ((fun () -> 42)()) in
      check_false (is_value x);
      check_42 (value x);
      check_true (is_value x);
    ));
    "map" >: (lazy (check_42 (value (map (lazy 21) ~f:(( * ) 2)))));
    "map is lazy" >: (lazy (
      let x = lazy (Exception.failure "You called me!") in
      let y = map x ~f:(fun _ -> Exception.failure "Don't call me") in
      expect_exception ~expected:(Exception.Failure "You called me!") (lazy (value y))
    ));
  ]
end
