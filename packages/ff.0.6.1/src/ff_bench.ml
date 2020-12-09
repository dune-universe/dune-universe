open Core_bench

module MakeBench (F : Ff_sig.BASE) = struct
  let e1 = F.random ()

  let e2 = F.random ()

  let zero = F.zero

  let one = F.one

  let generate_random_element () = ignore @@ F.random ()

  let generate_zero () = ignore @@ F.zero

  let generate_one () = ignore @@ F.one

  let e1_bytes = F.to_bytes e1

  let zero_bytes = F.to_bytes zero

  let one_bytes = F.to_bytes one

  let check_if_zero_on_pregenerated_random_element () = ignore @@ F.is_zero e1

  let check_if_zero_on_pregenerated_zero_element () = ignore @@ F.is_zero zero

  let check_if_zero_on_pregenerated_one_element () = ignore @@ F.is_zero one

  let check_if_one_on_pregenerated_random_element () = ignore @@ F.is_one e1

  let check_if_one_on_pregenerated_zero_element () = ignore @@ F.is_one zero

  let check_if_one_on_pregenerated_one_element () = ignore @@ F.is_one one

  let compute_addition_on_pregenerated_random_element () = ignore @@ F.add e1 e2

  let compute_multiplication_on_pregenerated_random_element () =
    ignore @@ F.mul e1 e2

  let compute_square_on_pregenerated_random_element () = ignore @@ F.square e1

  let compute_double_on_pregenerated_random_element () = ignore @@ F.double e1

  let compute_eq_on_pregenerated_random_elements () = ignore @@ F.eq e1 e2

  let compute_eq_on_same_pregenerated_random_element () = ignore @@ F.eq e1 e1

  let compute_opposite_on_pregenerated_random_element () = ignore @@ F.negate e1

  let compute_opposite_on_pregenerated_one_element () = ignore @@ F.negate one

  let compute_opposite_on_pregenerated_zero_element () = ignore @@ F.negate zero

  let compute_inverse_on_pregenerated_random_element () =
    ignore @@ F.inverse_exn e1

  let compute_inverse_on_pregenerated_one_element () =
    ignore @@ F.inverse_exn one

  let compute_inverse_opt_on_pregenerated_random_element () =
    ignore @@ F.inverse_opt e1

  let compute_inverse_opt_on_pregenerated_one_element () =
    ignore @@ F.inverse_opt one

  let compute_inverse_opt_on_pregenerated_zero_element () =
    ignore @@ F.inverse_opt zero

  let get_benches ff_name =
    [ Bench.Test.create
        ~name:
          (Printf.sprintf
             "%s compute addition pregenerated random element"
             ff_name)
        compute_addition_on_pregenerated_random_element;
      Bench.Test.create
        ~name:(Printf.sprintf "%s random generation" ff_name)
        generate_random_element;
      Bench.Test.create
        ~name:(Printf.sprintf "%s zero generation" ff_name)
        generate_zero;
      Bench.Test.create
        ~name:(Printf.sprintf "%s one generation" ff_name)
        generate_one;
      Bench.Test.create
        ~name:(Printf.sprintf "%s check if zero on pregenerated random" ff_name)
        check_if_zero_on_pregenerated_random_element;
      Bench.Test.create
        ~name:(Printf.sprintf "%s check if zero on pregenerated one" ff_name)
        check_if_zero_on_pregenerated_one_element;
      Bench.Test.create
        ~name:(Printf.sprintf "%s check if zero on pregenerated zero" ff_name)
        check_if_zero_on_pregenerated_zero_element;
      Bench.Test.create
        ~name:(Printf.sprintf "%s check if one on pregenerated random" ff_name)
        check_if_one_on_pregenerated_random_element;
      Bench.Test.create
        ~name:(Printf.sprintf "%s check if one on pregenerated one" ff_name)
        check_if_one_on_pregenerated_one_element;
      Bench.Test.create
        ~name:(Printf.sprintf "%s check if one on pregenerated zero" ff_name)
        check_if_one_on_pregenerated_zero_element;
      Bench.Test.create
        ~name:
          (Printf.sprintf "%s compute addition on pregenerate random" ff_name)
        compute_addition_on_pregenerated_random_element;
      Bench.Test.create
        ~name:
          (Printf.sprintf
             "%s compute multiplication on pregenerate random"
             ff_name)
        compute_multiplication_on_pregenerated_random_element;
      Bench.Test.create
        ~name:(Printf.sprintf "%s compute square on pregenerate random" ff_name)
        compute_square_on_pregenerated_random_element;
      Bench.Test.create
        ~name:(Printf.sprintf "%s compute double on pregenerate random" ff_name)
        compute_double_on_pregenerated_random_element;
      Bench.Test.create
        ~name:(Printf.sprintf "%s compute equality on random" ff_name)
        compute_eq_on_pregenerated_random_elements;
      Bench.Test.create
        ~name:(Printf.sprintf "%s compute equality on same element" ff_name)
        compute_eq_on_same_pregenerated_random_element;
      Bench.Test.create
        ~name:
          (Printf.sprintf
             "%s compute opposite of pregenerated random element"
             ff_name)
        compute_opposite_on_pregenerated_random_element;
      Bench.Test.create
        ~name:
          (Printf.sprintf
             "%s compute opposite of pregenerated one element"
             ff_name)
        compute_opposite_on_pregenerated_one_element;
      Bench.Test.create
        ~name:
          (Printf.sprintf
             "%s compute opposite of pregenerated zero element"
             ff_name)
        compute_opposite_on_pregenerated_zero_element;
      Bench.Test.create
        ~name:
          (Printf.sprintf
             "%s compute inverse of pregenerated random element"
             ff_name)
        compute_inverse_on_pregenerated_random_element;
      Bench.Test.create
        ~name:
          (Printf.sprintf
             "%s compute inverse of pregenerated one element"
             ff_name)
        compute_inverse_on_pregenerated_one_element;
      Bench.Test.create
        ~name:
          (Printf.sprintf
             "%s compute inverse opt of pregenerated random element"
             ff_name)
        compute_inverse_opt_on_pregenerated_random_element;
      Bench.Test.create
        ~name:
          (Printf.sprintf
             "%s compute inverse opt of pregenerated one element"
             ff_name)
        compute_inverse_opt_on_pregenerated_one_element;
      Bench.Test.create
        ~name:
          (Printf.sprintf
             "%s compute inverse opt of pregenerated zero element"
             ff_name)
        compute_inverse_opt_on_pregenerated_zero_element ]
end
