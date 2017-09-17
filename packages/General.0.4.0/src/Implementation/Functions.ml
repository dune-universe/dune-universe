include Foundations.Functions

module Tests = struct
  open Testing

  let test = "Functions" >:: [
    "Function1" >:: Function1.[
      "identity 42" >: (lazy (check_42 (identity 42)));
      "apply" >: (lazy (check_42 (apply (( * ) 2) 21)));
      "rev_apply" >: (lazy (check_42 (rev_apply 21 (( * ) 2))));
      "compose" >: (lazy (check_string ~expected:"42" ((compose (Format.apply "%d") (( * ) 2)) 21)));
    ];
    "Function2" >:: Function2.[
      "flip" >: (lazy (check_int ~expected:128 ((flip Int.exponentiate) 7 2)));
      "curry" >: (lazy (check_int ~expected:5 ((curry (fun (x, y) -> x - y)) 7 2)));
      "uncurry" >: (lazy (check_int ~expected:5 ((uncurry (-)) (7, 2))));
    ];
    "Function3" >:: Function3.[
      "flip" >: (lazy (check_string ~expected:"2 7 5" ((flip (Format.apply "%d %d %d")) 5 7 2)));
      "curry" >: (lazy (check_int ~expected:1 ((curry (fun (x, y, z) -> x - y * z)) 7 2 3)));
      "uncurry" >: (lazy (check_string ~expected:"7 2 4" ((uncurry (Format.apply "%d %d %d")) (7, 2, 4))));
    ];
    "Function4" >:: Function4.[
      "flip" >: (lazy (check_string ~expected:"2 7 5 9" ((flip (Format.apply "%d %d %d %d")) 9 5 7 2)));
      "curry" >: (lazy (check_int ~expected:6 ((curry (fun (x, y, z, u) -> x - y * z + u)) 7 2 3 5)));
      "uncurry" >: (lazy (check_string ~expected:"7 2 4 5" ((uncurry (Format.apply "%d %d %d %d")) (7, 2, 4, 5))));
    ];
    "Function5" >:: Function5.[
      "flip" >: (lazy (check_string ~expected:"2 7 5 9 3" ((flip (Format.apply "%d %d %d %d %d")) 3 9 5 7 2)));
      "curry" >: (lazy (check_int ~expected:4 ((curry (fun (x, y, z, u, v) -> x - y * z + u -v)) 7 2 3 5 2)));
      "uncurry" >: (lazy (check_string ~expected:"7 2 4 5 3" ((uncurry (Format.apply "%d %d %d %d %d")) (7, 2, 4, 5, 3))));
    ];
  ]
end
