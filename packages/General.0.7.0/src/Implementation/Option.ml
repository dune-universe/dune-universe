module SelfA = struct
  include Foundations.Option
  include Traits.Equatable.Different.Make1(Foundations.Option)
  include Traits.Comparable.GreaterLessThan.Make1(Foundations.Option)
  include Traits.Comparable.MinMax.Make1(Foundations.Option)
end

module Self = struct
  include SelfA
  include Traits.Comparable.Between.Make1(SelfA)
end

module Specialize(A: sig type t end) = struct
  type t = A.t option

  include (Self: module type of Self with type 'a t := 'a Self.t)
end

include Self

module Tests = struct
  open Testing

  module Examples = struct
    module A = Int

    let repr = [
      (None, "None");
      (Some 42, "Some 42");
    ]

    let equal = [
      [None];
      [Some 42];
    ]

    let different = [
      (None, Some 42);
      (Some 42, Some 43);
    ]

    let ordered = [
      [None; Some 0; Some 1];
    ]
  end

  let test = "Option" >:: [
    (let module T = Concepts.Able.Tests.Make1(Self)(Examples) in T.test);
    "some_if true" >: (lazy (check_some_42 (some_if true (lazy 42))));
    "some_if false" >: (lazy (check_none_int (some_if false (lazy (Exception.failure "Don't call me"))))); (*BISECT-IGNORE*)
    "some_if' true" >: (lazy (check_some_42 (some_if' true 42)));
    "some_if' false" >: (lazy (check_none_int (some_if' false 42)));
    "is_some None" >: (lazy (check_false (is_some None)));
    "is_some Some" >: (lazy (check_true (is_some (Some 42))));
    "is_none None" >: (lazy (check_true (is_none None)));
    "is_none Some" >: (lazy (check_false (is_none (Some 42))));
    "value_def None" >: (lazy (check_string ~expected:"def" (value_def None ~def:"def")));
    "value_def Some" >: (lazy (check_string ~expected:"val" (value_def (Some "val") ~def:"def")));
    "value Some" >: (lazy (check_string ~expected:"val" (value (Some "val"))));
    "value None" >: (lazy (expect_exception ~expected:(Exception.Failure "Option.value") (lazy (value None))));
    "value ~exc None" >: (lazy (expect_exception ~expected:(Exception.Failure "Nope") (lazy (value ~exc:(Exception.Failure "Nope") None))));
    "repr None" >: (lazy (check_string ~expected:"None" (repr ~repr_a:(fun _ -> Exception.failure "Don't call me") None))); (*BISECT-IGNORE*) (*BISECT-IGNORE*)
    "map None" >: (lazy (check_none_int (map ~f:(fun _ -> Exception.failure "Don't call me") None))); (*BISECT-IGNORE*)
    "map Some" >: (lazy (check_some_42 (map ~f:(( * ) 2) (Some 21))));
    "value_map None" >: (lazy (check_42 (value_map ~def:42 ~f:(fun _ -> Exception.failure "Don't call me") None))); (*BISECT-IGNORE*)
    "value_map Some" >: (lazy (check_42 (value_map ~def:57 ~f:(( * ) 2) (Some 21))));
    "iter None" >: (lazy (iter ~f:(fun _ -> Exception.failure "Don't call me") None)); (*BISECT-IGNORE*)
    "iter Some" >: (lazy (check_42 (let x = ref 0 in iter ~f:(fun n -> x := n) (Some 42); !x)));
    "filter None" >: (lazy (check_none_int (filter ~f:(fun _ -> Exception.failure "Don't call me") None))); (*BISECT-IGNORE*)
    "filter Some true" >: (lazy (check_some_42 (filter ~f:(fun _ -> true) (Some 42))));
    "filter Some false" >: (lazy (check_none_int (filter ~f:(fun _ -> false) (Some 42))));
    "filter_map None" >: (lazy (check_none_int (filter_map ~f:(fun _ -> Exception.failure "Don't call me") None))); (*BISECT-IGNORE*)
    "filter_map Some true" >: (lazy (check_some_int ~expected:57 (filter_map ~f:(fun _ -> Some 57) (Some 42))));
    "filter_map Some false" >: (lazy (check_none_int (filter_map ~f:(fun _ -> None) (Some 42))));
  ]
end
