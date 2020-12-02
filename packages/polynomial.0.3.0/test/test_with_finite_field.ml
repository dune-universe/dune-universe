let is_prime_number n =
  let two = Z.succ Z.one in
  let rec internal div n =
    if Z.gt (Z.mul div div) n then true
    else if Z.equal (Z.erem n div) Z.zero then false
    else internal (Z.add div two) n
  in
  if not (Z.gt n Z.one) then false
  else if Z.equal two n then true
  else if Z.equal (Z.erem n two) Z.zero then false
  else internal (Z.succ two) n

let rec repeat n f =
  if n <= 0 then
    let f () = () in
    f
  else (
    f () ;
    repeat (n - 1) f )

module F379 = Ff.MakeFp (struct
  let prime_order = Z.of_int 379
end)

module Poly = Polynomial.MakeUnivariate (F379)
module TestDegree_F379 = Functors.MakeTestDegree (F379) (Poly)

module TestEvaluation_F379 = struct
  include Functors.MakeTestEvaluation (F379) (Poly)

  let test_eval_some_test_vectors () =
    let one = F379.one in
    let p = Poly.of_coefficients [(one, 2); (one, 1); (one, 0)] in
    let evaluation_point_with_expected_value =
      [ (F379.of_string "5", F379.of_string "31");
        (F379.of_string "42", F379.of_string "291");
        (F379.of_string "3", F379.of_string "13");
        (F379.of_string "0", F379.of_string "1") ]
    in
    List.iter
      (fun (x, expected_value) ->
        assert (F379.eq (Poly.evaluation p x) expected_value))
      evaluation_point_with_expected_value ;
    let p =
      Poly.of_coefficients
        [(one, 17); (F379.of_string "-42", 2); (F379.of_string "10", 0)]
    in
    let evaluation_point_with_expected_value =
      [ (F379.of_string "5", F379.of_string "40");
        (F379.of_string "42", F379.of_string "148");
        (F379.of_string "3", F379.of_string "93");
        (F379.of_string "0", F379.of_string "10") ]
    in
    List.iter
      (fun (x, expected_value) ->
        assert (F379.eq (Poly.evaluation p x) expected_value))
      evaluation_point_with_expected_value

  let get_tests () =
    let open Alcotest in
    let specific_tests =
      [test_case "evaluation test vectors" `Quick test_eval_some_test_vectors]
    in
    let (desc, tests) = get_tests () in
    (desc, List.concat [specific_tests; tests])
end

module TestAdd_F379 = struct
  let test_vectors () =
    (* 2 X^2 + 3X *)
    let p1 =
      Poly.of_coefficients [(F379.of_string "2", 2); (F379.of_string "3", 1)]
    in
    (* 10 X + 3 *)
    let p2 =
      Poly.of_coefficients [(F379.of_string "10", 1); (F379.of_string "3", 0)]
    in
    let expected_result =
      Poly.of_coefficients
        [ (F379.of_string "2", 2);
          (F379.of_string "13", 1);
          (F379.of_string "3", 0) ]
    in
    assert (Poly.equal (Poly.add p1 p2) expected_result)

  let get_tests () =
    let open Alcotest in
    ("Test add", [test_case "test vectors" `Quick test_vectors])
end

module TestMultByScalar_F379 = struct
  let test_vectors () =
    (* X/2 in F379 *)
    let p1 =
      Poly.mult_by_scalar
        (F379.inverse_exn (F379.of_string "2"))
        (Poly.of_coefficients [(F379.of_string "1", 1)])
    in
    assert (Poly.equal (Poly.of_coefficients [(F379.of_string "190", 1)]) p1)

  let test_multiply_constants_by_scalar_zero_is_zero () =
    let p1 = Poly.constants (F379.random ()) in
    assert (Poly.is_null (Poly.mult_by_scalar F379.zero p1))

  let test_multiply_degree_one_by_scalar_zero_is_zero () =
    let p1 = Poly.of_coefficients [(F379.of_string "1", 1)] in
    assert (Poly.is_null (Poly.mult_by_scalar F379.zero p1))

  let get_tests () =
    let open Alcotest in
    ( "Test mult by scalar",
      [ test_case "test vectors" `Quick test_vectors;
        test_case
          "test multiply constants by scalar zero is zero"
          `Quick
          test_multiply_constants_by_scalar_zero_is_zero;
        test_case
          "test multiply degree one by scalar zero is zero"
          `Quick
          test_multiply_degree_one_by_scalar_zero_is_zero ] )
end

module TestOpposite_F379 = struct
  let test_property_of_twice_opposite () =
    let p1 = Poly.of_coefficients [(F379.of_string "10", 1)] in
    assert (Poly.equal (Poly.opposite (Poly.opposite p1)) p1)

  let test_property_opposite_of_constant () =
    let random = F379.random () in
    assert (
      Poly.equal
        (Poly.opposite (Poly.constants random))
        (Poly.constants (F379.negate random)) )

  let test_property_opposite_of_zero () =
    assert (Poly.(Poly.opposite Poly.zero = Poly.zero))

  let get_tests () =
    let open Alcotest in
    ( "Test opposite",
      [ test_case
          "test property opposite twice"
          `Quick
          test_property_of_twice_opposite;
        test_case
          "test property opposite of constant"
          `Quick
          test_property_opposite_of_constant;
        test_case
          "test property opposite of zero"
          `Quick
          test_property_opposite_of_zero ] )
end

module TestLagrangeInterpolation_F379 = struct
  include Functors.MakeTestLagrangeInterpolation (F379) (Poly)

  let test_vector () =
    let points =
      [ (F379.of_string "2", F379.of_string "3");
        (F379.of_string "0", F379.of_string "1") ]
    in
    let interpolated_polynomial = Poly.lagrange_interpolation points in
    match Poly.degree interpolated_polynomial with
    | Polynomial.Infinity -> assert false
    | Natural n ->
        assert (n <= List.length points - 1) ;
        assert (
          Poly.equal
            (Poly.of_coefficients
               [(F379.of_string "1", 1); (F379.of_string "1", 0)])
            interpolated_polynomial ) ;
        assert (
          F379.eq
            (Poly.evaluation interpolated_polynomial (F379.of_string "0"))
            (F379.of_string "1") ) ;
        assert (
          F379.eq
            (Poly.evaluation interpolated_polynomial (F379.of_string "2"))
            (F379.of_string "3") ) ;
        (* Other random points *)
        assert (
          F379.eq
            (Poly.evaluation interpolated_polynomial (F379.of_string "1"))
            (F379.of_string "2") ) ;
        assert (
          F379.eq
            (Poly.evaluation interpolated_polynomial (F379.of_string "17"))
            (F379.of_string "18") )

  let get_tests () =
    let open Alcotest in
    let specific_tests = [test_case "test vector" `Quick test_vector] in
    let (desc, tests) = get_tests () in
    (desc, List.concat [specific_tests; tests])
end

module TestSplitPolynomial_F379 = struct
  let test_even_polynomial () =
    let x = F379.random () in
    let test_vectors =
      [ (Poly.zero, Poly.zero);
        (Poly.constants x, Poly.constants x);
        (Poly.of_coefficients [(x, 2)], Poly.of_coefficients [(x, 2)]);
        (Poly.of_coefficients [(x, 1)], Poly.zero);
        (Poly.of_coefficients [(x, 3); (x, 1)], Poly.zero);
        (Poly.of_coefficients [(x, 4); (x, 1)], Poly.of_coefficients [(x, 4)]);
        ( Poly.of_coefficients
            [ (x, 34534);
              (x, 345);
              (x, 23);
              (x, 21);
              (x, 17);
              (x, 14);
              (x, 3);
              (x, 1);
              (x, 0) ],
          Poly.of_coefficients [(x, 34534); (x, 14); (x, 0)] ) ]
    in
    List.iter
      (fun (v, expected_result) ->
        assert (Poly.equal expected_result (Poly.even_polynomial v)))
      test_vectors

  let test_odd_polynomial () =
    let x = F379.random () in
    let test_vectors =
      [ (Poly.zero, Poly.zero);
        (Poly.constants x, Poly.zero);
        (Poly.of_coefficients [(x, 2)], Poly.zero);
        (Poly.of_coefficients [(x, 1)], Poly.of_coefficients [(x, 1)]);
        ( Poly.of_coefficients [(x, 3); (x, 1)],
          Poly.of_coefficients [(x, 3); (x, 1)] );
        (Poly.of_coefficients [(x, 4); (x, 1)], Poly.of_coefficients [(x, 1)]);
        ( Poly.of_coefficients
            [ (x, 34534);
              (x, 345);
              (x, 23);
              (x, 21);
              (x, 17);
              (x, 14);
              (x, 3);
              (x, 1);
              (x, 0) ],
          Poly.of_coefficients
            [(x, 345); (x, 23); (x, 21); (x, 17); (x, 3); (x, 1)] ) ]
    in
    List.iter
      (fun (v, expected_result) ->
        assert (Poly.equal expected_result (Poly.odd_polynomial v)))
      test_vectors

  let get_tests () =
    let open Alcotest in
    ( "Split polynomials",
      [ test_case
          "Test even polynomial with test vectors"
          `Quick
          test_even_polynomial;
        test_case
          "Test odd polynomial with test vectors"
          `Quick
          test_odd_polynomial ] )
end

module TestDensifiedPolynomial_F379 =
  Functors.MakeTestDensifiedPolynomial (F379) (Poly)

module TestFFT_F337 = struct
  module F337 = Ff.MakeFp (struct
    let prime_order = Z.of_string "337"
  end)

  module Poly = Polynomial.MakeUnivariate (F337)

  let test_evaluation_fft_vectors () =
    let test_vectors =
      [ ( Poly.of_coefficients
            [ (F337.of_string "6", 7);
              (F337.of_string "2", 6);
              (F337.of_string "9", 5);
              (F337.of_string "5", 4);
              (F337.of_string "1", 3);
              (F337.of_string "4", 2);
              (F337.of_string "1", 1);
              (F337.of_string "3", 0) ],
          F337.of_string "85",
          Z.of_string "8",
          [ F337.of_string "31";
            F337.of_string "70";
            F337.of_string "109";
            F337.of_string "74";
            F337.of_string "334";
            F337.of_string "181";
            F337.of_string "232";
            F337.of_string "4" ] ) ]
    in
    List.iter
      (fun (polynomial, generator, power, expected_result) ->
        let res = Poly.evaluation_fft polynomial ~generator ~power in
        assert (res = expected_result))
      test_vectors

  let test_evaluation_random_values_against_normal_evaluation () =
    (* to keep 85 in F337 *)
    let degree = 8 in
    (* generate some evaluation points, not twice the same *)
    let rec generate_evaluation_points i n acc =
      if i < n then
        let r = F337.random () in
        if List.mem r acc then generate_evaluation_points i n acc
        else generate_evaluation_points (i + 1) n (r :: acc)
      else acc
    in
    let evaluation_points = generate_evaluation_points 0 degree [] in
    let polynomial =
      Poly.generate_random_polynomial (Polynomial.Natural degree)
    in
    let expected_results =
      List.map (fun x -> Poly.evaluation polynomial x) evaluation_points
    in
    let results =
      Poly.evaluation_fft
        ~generator:(F337.of_string "85")
        ~power:(Z.of_int degree)
        polynomial
    in
    assert (expected_results = results)

  let get_tests () =
    let open Alcotest in
    ( Printf.sprintf "FFT for prime field %s" (Z.to_string F337.order),
      [ test_case
          "test vectors for evaluation"
          `Quick
          test_evaluation_fft_vectors;
        test_case
          "test evaluation at random points"
          `Quick
          (repeat 10 test_evaluation_fft_vectors) ] )
end

module TestInverseFFT_F337 = struct
  module F337 = Ff.MakeFp (struct
    let prime_order = Z.of_string "337"
  end)

  module Poly = Polynomial.MakeUnivariate (F337)

  let nth_root_of_unity = F337.of_string "85"

  let power = Z.of_string "8"

  let test_interpolation_fft_vectors () =
    let test_vectors =
      [ ( [ F337.of_string "31";
            F337.of_string "70";
            F337.of_string "109";
            F337.of_string "74";
            F337.of_string "334";
            F337.of_string "181";
            F337.of_string "232";
            F337.of_string "4" ],
          Poly.of_coefficients
            [ (F337.of_string "6", 7);
              (F337.of_string "2", 6);
              (F337.of_string "9", 5);
              (F337.of_string "5", 4);
              (F337.of_string "1", 3);
              (F337.of_string "4", 2);
              (F337.of_string "1", 1);
              (F337.of_string "3", 0) ] ) ]
    in
    List.iter
      (fun (points, expected_polynomial) ->
        let res =
          Poly.interpolation_fft ~generator:nth_root_of_unity ~power points
        in
        assert (Poly.equal res expected_polynomial))
      test_vectors ;
    (* With a null coefficient *)
    let points =
      [ F337.of_string "34";
        F337.of_string "34";
        F337.of_string "34";
        F337.of_string "34";
        F337.of_string "34";
        F337.of_string "34";
        F337.of_string "34";
        F337.of_string "0" ]
    in
    let res =
      Poly.interpolation_fft ~generator:nth_root_of_unity ~power points
    in
    let domain =
      List.init (Z.to_int power) (fun i ->
          F337.pow nth_root_of_unity (Z.of_int i))
    in
    let expected_results =
      Poly.lagrange_interpolation (List.combine domain points)
    in
    assert (Poly.equal res expected_results)

  let test_interpolation_fft_random_values_against_lagrange_interpolation () =
    let random_polynomial =
      Poly.generate_random_polynomial (Polynomial.Natural (Z.to_int power - 1))
    in
    let evaluation_points =
      Poly.get_dense_polynomial_coefficients random_polynomial
    in
    let domain =
      List.init (Z.to_int power) (fun i ->
          F337.pow nth_root_of_unity (Z.of_int i))
    in
    let expected_results =
      Poly.lagrange_interpolation (List.combine domain evaluation_points)
    in
    let results =
      Poly.interpolation_fft
        ~generator:nth_root_of_unity
        ~power
        evaluation_points
    in
    assert (Poly.equal results expected_results)

  let get_tests () =
    let open Alcotest in
    ( Printf.sprintf "Inverse FFT for prime field %s" (Z.to_string F337.order),
      [ test_case
          "test vectors for interpolation fft"
          `Quick
          test_interpolation_fft_vectors;
        test_case
          "test interpolation at random points"
          `Quick
          (repeat
             1
             test_interpolation_fft_random_values_against_lagrange_interpolation)
      ] )
end

module TestPolynomialMultiplicationFFT_F337 = struct
  module F337 = Ff.MakeFp (struct
    let prime_order = Z.of_string "337"
  end)

  module Poly = Polynomial.MakeUnivariate (F337)

  let generator = F337.of_string "85"

  let power = Z.of_string "8"

  let test_vectors () =
    let vectors =
      [ ( Poly.zero,
          Poly.generate_random_polynomial (Polynomial.Natural 1000),
          Poly.zero );
        ( Poly.generate_random_polynomial (Polynomial.Natural 100),
          Poly.zero,
          Poly.zero );
        ( Poly.zero,
          Poly.generate_random_polynomial (Polynomial.Natural 1000),
          Poly.zero );
        ( Poly.of_coefficients
            [ (F337.of_string "3", 3);
              (F337.of_string "2", 2);
              (F337.of_string "1", 1);
              (F337.of_string "1", 0) ],
          Poly.of_coefficients
            [ (F337.of_string "3", 3);
              (F337.of_string "2", 2);
              (F337.of_string "1", 1);
              (F337.of_string "1", 0) ],
          Poly.of_coefficients
            [ (F337.of_string "9", 6);
              (F337.of_string "12", 5);
              (F337.of_string "10", 4);
              (F337.of_string "10", 3);
              (F337.of_string "5", 2);
              (F337.of_string "2", 1);
              (F337.of_string "1", 0) ] );
        ( Poly.of_coefficients
            [ (F337.of_string "23", 3);
              (F337.of_string "35", 2);
              (F337.of_string "213", 1);
              (F337.of_string "32", 0) ],
          Poly.of_coefficients
            [ (F337.of_string "121", 3);
              (F337.of_string "43", 2);
              (F337.of_string "56", 1);
              (F337.of_string "82", 0) ],
          Poly.of_coefficients
            [ (F337.of_string "87", 6);
              (F337.of_string "169", 5);
              (F337.of_string "258", 4);
              (F337.of_string "27", 3);
              (F337.of_string "335", 2);
              (F337.of_string "49", 1);
              (F337.of_string "265", 0) ] ) ]
    in
    List.iter
      (fun (p, q, expected_result) ->
        assert (
          let res = Poly.polynomial_multiplication_fft ~generator ~power p q in
          Poly.equal expected_result res ))
      vectors

  let get_tests () =
    let open Alcotest in
    ( "Polynomial multiplication FFT",
      [ test_case
          "test vectors for polynomial multiplication"
          `Quick
          test_vectors ] )
end

module TestEuclidianDivision_F379 = struct
  include Functors.MakeTestEuclidianDivision (F379) (Poly)

  let test_vectors () =
    let vectors =
      [ (* X^2 + 7X + 6 / X + 6 -> Q = X + 1 and R = 0 *)
        ( Poly.of_coefficients
            [ (F379.of_string "1", 2);
              (F379.of_string "7", 1);
              (F379.of_string "6", 0) ],
          Poly.of_coefficients [(F379.of_string "1", 1); (F379.of_string "6", 0)],
          Some
            ( Poly.of_coefficients
                [(F379.of_string "1", 1); (F379.of_string "1", 0)],
              Poly.zero ) );
        (* 2X^2 + 4X + 6 / 2 -> Q = X^2 + 2X + 3 and R = 0 *)
        ( Poly.of_coefficients
            [ (F379.of_string "2", 2);
              (F379.of_string "4", 1);
              (F379.of_string "6", 0) ],
          Poly.of_coefficients [(F379.of_string "2", 0)],
          Some
            ( Poly.of_coefficients
                [ (F379.of_string "1", 2);
                  (F379.of_string "2", 1);
                  (F379.of_string "3", 0) ],
              Poly.zero ) );
        (* 2X^2 + 4X + 6 / 1 -> Q = 2X^2 + 4X + 6 and R = 0 *)
        ( Poly.of_coefficients
            [ (F379.of_string "2", 2);
              (F379.of_string "4", 1);
              (F379.of_string "6", 0) ],
          Poly.of_coefficients [(F379.of_string "1", 0)],
          Some
            ( Poly.of_coefficients
                [ (F379.of_string "2", 2);
                  (F379.of_string "4", 1);
                  (F379.of_string "6", 0) ],
              Poly.zero ) );
        (* X^2 + 7X + 6 / X + 1 -> Q = X + 6 and R = 0 *)
        ( Poly.of_coefficients
            [ (F379.of_string "1", 2);
              (F379.of_string "7", 1);
              (F379.of_string "6", 0) ],
          Poly.of_coefficients [(F379.of_string "1", 1); (F379.of_string "1", 0)],
          Some
            ( Poly.of_coefficients
                [(F379.of_string "1", 1); (F379.of_string "6", 0)],
              Poly.zero ) );
        (* X^2 + 7X + 10 / X + 1 -> Q = X + 6 and R = 4 *)
        ( Poly.of_coefficients
            [ (F379.of_string "1", 2);
              (F379.of_string "7", 1);
              (F379.of_string "10", 0) ],
          Poly.of_coefficients [(F379.of_string "1", 1); (F379.of_string "1", 0)],
          Some
            ( Poly.of_coefficients
                [(F379.of_string "1", 1); (F379.of_string "6", 0)],
              Poly.constants (F379.of_string "4") ) );
        (* X + 1 / X^2 + 1 -> Q = 0 and R = X^2 + 1*)
        ( Poly.of_coefficients [(F379.of_string "1", 1); (F379.of_string "1", 0)],
          Poly.of_coefficients [(F379.of_string "1", 2); (F379.of_string "1", 0)],
          Some
            ( Poly.zero,
              Poly.of_coefficients
                [(F379.of_string "1", 1); (F379.of_string "1", 0)] ) );
        (* Zero / X^2 + 1 -> Q = 0 and R = 0*)
        ( Poly.zero,
          Poly.of_coefficients [(F379.of_string "1", 2); (F379.of_string "1", 0)],
          Some (Poly.zero, Poly.zero) );
        (* Random polynomial / Zero -> None *)
        ( Poly.generate_random_polynomial (Polynomial.Natural (Random.int 10000)),
          Poly.zero,
          None ) ]
    in
    List.iter
      (fun (a, b, expected_result) ->
        let res = Poly.euclidian_division_opt a b in
        match (res, expected_result) with
        | (None, None) -> assert true
        | (None, _) | (_, None) -> assert false
        | (Some (q, r), Some (expected_q, expected_r)) ->
            assert (Poly.equal q expected_q && Poly.equal r expected_r))
      vectors

  let get_tests () =
    let open Alcotest in
    let specific_tests =
      [test_case "test vectors for euclidian division" `Quick test_vectors]
    in
    let (desc, tests) = get_tests () in
    (desc, List.concat [specific_tests; tests])
end

module TestExtendedEuclide_F379 = Functors.MakeTestExtendedEuclide (F379) (Poly)

let make_test_battery_for_prime_order_field p =
  let module Fp = Ff.MakeFp (struct
    let prime_order = p
  end) in
  let module Poly = Polynomial.MakeUnivariate (Fp) in
  let module TestDegree = Functors.MakeTestDegree (Fp) (Poly) in
  let module TestDensifiedPolynomial =
    Functors.MakeTestDensifiedPolynomial (Fp) (Poly)
  in
  let module TestEvaluation = Functors.MakeTestEvaluation (Fp) (Poly) in
  let module TestEuclidianDivision =
    Functors.MakeTestEuclidianDivision (Fp) (Poly)
  in
  let module TestExtendedEuclide = Functors.MakeTestExtendedEuclide (Fp) (Poly)
  in
  let module TestLagrangeInterpolation =
    Functors.MakeTestLagrangeInterpolation (Fp) (Poly)
  in
  let module TestPolynomialMultiplication =
    Functors.MakeTestPolynomialMultiplication (Fp) (Poly)
  in
  let module TestConstant = Functors.MakeTestConstant (Fp) (Poly) in
  Printf.printf "Generating test battery for prime field %s\n" (Z.to_string p) ;
  [ TestDegree.get_tests ();
    TestEvaluation.get_tests ();
    TestEuclidianDivision.get_tests ();
    TestPolynomialMultiplication.get_tests ();
    TestExtendedEuclide.get_tests ();
    TestDensifiedPolynomial.get_tests ();
    TestConstant.get_tests ();
    TestLagrangeInterpolation.get_tests () ]

let rec make_test_battery_with_random_fields acc n =
  if n = 0 then acc
  else
    let prime_number = Random.int 0x3FFFFFFF (* max random *) in
    if not (is_prime_number (Z.of_int prime_number)) then
      make_test_battery_with_random_fields acc n
    else
      let tests =
        make_test_battery_for_prime_order_field (Z.of_int prime_number)
      in
      make_test_battery_with_random_fields (List.concat [acc; tests]) (n - 1)

let () =
  let open Alcotest in
  let random_prime_fields_tests = make_test_battery_with_random_fields [] 5 in
  let tests_for_BLS_Fr =
    make_test_battery_for_prime_order_field
      (Z.of_string
         "52435875175126190479447740508185965837690552500527637822603658699938581184513")
  in
  let tests_for_BLS_Fq =
    make_test_battery_for_prime_order_field
      (Z.of_string
         "4002409555221667393417789825735904156556882819939007885332058136124031650490837864442687629129015664037894272559787")
  in
  let test_for_base_field_curve25519 =
    make_test_battery_for_prime_order_field Z.(pow (succ one) 255 - of_int 19)
  in
  run
    "Polynomials with F379 and some random prime fields"
    (List.concat
       [ random_prime_fields_tests;
         tests_for_BLS_Fr;
         tests_for_BLS_Fq;
         test_for_base_field_curve25519;
         [ TestDegree_F379.get_tests ();
           TestEvaluation_F379.get_tests ();
           TestLagrangeInterpolation_F379.get_tests ();
           TestMultByScalar_F379.get_tests ();
           TestOpposite_F379.get_tests ();
           TestSplitPolynomial_F379.get_tests ();
           TestDensifiedPolynomial_F379.get_tests ();
           TestInverseFFT_F337.get_tests ();
           TestPolynomialMultiplicationFFT_F337.get_tests ();
           TestFFT_F337.get_tests ();
           TestEuclidianDivision_F379.get_tests ();
           TestExtendedEuclide_F379.get_tests ();
           TestAdd_F379.get_tests () ] ])
