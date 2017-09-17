#define TUPLE(N) \
module CONCAT(TupleA, N) = struct \
  include Foundations.Tuples.CONCAT(Tuple, N) \
  include Traits.Comparable.GreaterLessThan.CONCAT(Make, N)(Foundations.Tuples.CONCAT(Tuple, N)) \
  include Traits.Comparable.MinMax.CONCAT(Make, N)(Foundations.Tuples.CONCAT(Tuple, N)) \
  include Traits.Equatable.Different.CONCAT(Make, N)(Foundations.Tuples.CONCAT(Tuple, N)) \
end \
module CONCAT(Tuple, N) = struct \
  include CONCAT(TupleA, N) \
  include Traits.Comparable.Between.CONCAT(Make, N)(CONCAT(TupleA, N)) \
end

TUPLE(2)
TUPLE(3)
TUPLE(4)
TUPLE(5)

module Tests = struct
  open Testing

  module Examples2 = struct
    module A = Foundations.Int
    module B = Foundations.String

    let repr = [
      ((1, "a"), "(1, \"a\")");
    ]

    let equal = [
      [(1, "a")];
    ]

    let different = [
      ((1, "a"), (1, "b"));
      ((1, "a"), (2, "a"));
    ]

    let ordered = [
      [(0, "a"); (0, "b"); (1, "a")]
    ]
  end

  module Examples3 = struct
    module A = Foundations.Int
    module B = Foundations.String
    module C = Foundations.Float

    let repr = [
      ((1, "a", 2.), "(1, \"a\", 2.)");
    ]

    let equal = [
      [(1, "a", 2.)];
    ]

    let different = [
      ((1, "a", 2.), (1, "a", 3.));
      ((1, "a", 2.), (1, "b", 2.));
      ((1, "a", 2.), (2, "a", 2.));
    ]

    let ordered = [
      [(0, "a", 0.); (0, "a", 1.); (0, "b", 0.); (1, "a", 0.)]
    ]
  end

  module Examples4 = struct
    module A = Foundations.Int
    module B = Foundations.String
    module C = Foundations.Float
    module D = Foundations.Int

    let repr = [
      ((1, "a", 2., 3), "(1, \"a\", 2., 3)");
    ]

    let equal = [
      [(1, "a", 2., 3)];
    ]

    let different = [
      ((1, "a", 2., 3), (1, "a", 2., 4));
      ((1, "a", 2., 3), (1, "a", 3., 3));
      ((1, "a", 2., 3), (1, "b", 2., 3));
      ((1, "a", 2., 3), (0, "a", 2., 3));
    ]

    let ordered = [
      [(1, "a", 2., 3); (1, "a", 2., 4); (1, "a", 3., 3); (1, "b", 2., 3); (2, "a", 2., 3)]
    ]
  end

  module Examples5 = struct
    module A = Foundations.Int
    module B = Foundations.String
    module C = Foundations.Float
    module D = Foundations.Int
    module E = Foundations.Int

    let repr = [
      ((1, "a", 2., 3, 4), "(1, \"a\", 2., 3, 4)");
    ]

    let equal = [
      [(1, "a", 2., 3, 4)];
    ]

    let different = [
      ((1, "a", 2., 3, 4), (1, "a", 2., 3, 5));
      ((1, "a", 2., 3, 4), (1, "a", 2., 4, 4));
      ((1, "a", 2., 3, 4), (1, "a", 3., 3, 4));
      ((1, "a", 2., 3, 4), (1, "b", 2., 3, 4));
      ((1, "a", 2., 3, 4), (0, "a", 2., 3, 4));
    ]

    let ordered = [
      [(1, "a", 2., 3, 4); (1, "a", 2., 3, 5); (1, "a", 2., 4, 4); (1, "a", 3., 3, 4); (1, "b", 2., 3, 4); (2, "a", 2., 3, 4)]
    ]
  end

  #define TEST_TRAITS(N) \
    (let module T = Concepts.Able.Tests.CONCAT(Make, N)(CONCAT(Tuple, N))(CONCAT(Examples, N)) in T.test)

  let test = "Tuples" >:: [
    "Tuple2" >:: Tuple2.[
      TEST_TRAITS(2);
      "make" >: (lazy (check_int_tuple2 ~expected:(1, 2) (make 1 2)));
      "flip" >: (lazy (check_int_tuple2 ~expected:(1, 2) (flip (2, 1))));
      "get_0" >: (lazy (check_42 (get_0 (42, 0))));
      "get_1" >: (lazy (check_42 (get_1 (0, 42))));
    ];
    "Tuple3" >:: Tuple3.[
      TEST_TRAITS(3);
      "make" >: (lazy (check_int_tuple3 ~expected:(1, 2, 3) (make 1 2 3)));
      "flip" >: (lazy (check_int_tuple3 ~expected:(1, 2, 3) (flip (3, 2, 1))));
      "get_0" >: (lazy (check_42 (get_0 (42, 0, 0))));
      "get_1" >: (lazy (check_42 (get_1 (0, 42, 0))));
      "get_2" >: (lazy (check_42 (get_2 (0, 0, 42))));
    ];
    "Tuple4" >:: Tuple4.[
      TEST_TRAITS(4);
      "make" >: (lazy (check_int_tuple4 ~expected:(1, 2, 3, 4) (make 1 2 3 4)));
      "flip" >: (lazy (check_int_tuple4 ~expected:(1, 2, 3, 4) (flip (4, 3, 2, 1))));
      "get_0" >: (lazy (check_42 (get_0 (42, 0, 0, 0))));
      "get_1" >: (lazy (check_42 (get_1 (0, 42, 0, 0))));
      "get_2" >: (lazy (check_42 (get_2 (0, 0, 42, 0))));
      "get_3" >: (lazy (check_42 (get_3 (0, 0, 0, 42))));
    ];
    "Tuple5" >:: Tuple5.[
      TEST_TRAITS(5);
      "make" >: (lazy (check_int_tuple5 ~expected:(1, 2, 3, 4, 5) (make 1 2 3 4 5)));
      "flip" >: (lazy (check_int_tuple5 ~expected:(1, 2, 3, 4, 5) (flip (5, 4, 3, 2, 1))));
      "get_0" >: (lazy (check_42 (get_0 (42, 0, 0, 0, 0))));
      "get_1" >: (lazy (check_42 (get_1 (0, 42, 0, 0, 0))));
      "get_2" >: (lazy (check_42 (get_2 (0, 0, 42, 0, 0))));
      "get_3" >: (lazy (check_42 (get_3 (0, 0, 0, 42, 0))));
      "get_4" >: (lazy (check_42 (get_4 (0, 0, 0, 0, 42))));
    ];
  ]
end
