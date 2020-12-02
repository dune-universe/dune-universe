let rec repeat n f =
  if n <= 0 then
    let f () = () in
    f
  else (
    f () ;
    repeat (n - 1) f )

let rec non_null_int bound =
  let r = Random.int bound in
  if r = 0 then non_null_int bound else r

module MakeTestConstant
    (Scalar : Ff_sig.PRIME)
    (Poly : Polynomial.UNIVARIATE with type scalar = Scalar.t) =
struct
  let test_zero () = assert (Poly.is_constant Poly.zero)

  let test_random () =
    assert (Poly.is_constant (Poly.constants (Scalar.random ())))

  let test_random_polynomials () =
    assert (
      not
        (Poly.is_constant
           (Poly.generate_random_polynomial
              (Polynomial.Natural (non_null_int 100)))) )

  let get_tests () =
    let open Alcotest in
    ( Printf.sprintf
        "Tests for constant polynomials, field order = %s"
        (Z.to_string Scalar.order),
      [ test_case "zero polynomial is constant" `Quick test_zero;
        test_case "Constant random value" `Quick (repeat 100 test_random);
        test_case
          "Non constant polynomial"
          `Quick
          (repeat 100 test_random_polynomials) ] )
end

module MakeTestDegree
    (Scalar : Ff_sig.PRIME)
    (Poly : Polynomial.UNIVARIATE with type scalar = Scalar.t) =
struct
  let test_degree_zero_is_infinity () =
    assert (Poly.degree Poly.zero = Polynomial.Infinity)

  let test_degree_of_constants_is_one () =
    assert (
      Poly.degree (Poly.constants (Scalar.random ())) = Polynomial.Infinity )

  let test_degree_int_test_vectors () =
    let vectors =
      [ (Poly.zero, -1);
        (Poly.generate_random_polynomial (Polynomial.Natural 10), 10);
        (Poly.generate_random_polynomial (Polynomial.Natural 100), 100);
        (Poly.generate_random_polynomial (Polynomial.Natural 0), 0);
        (Poly.generate_random_polynomial (Polynomial.Natural 42), 42) ]
    in
    List.iter
      (fun (p, expected_result) -> assert (Poly.degree_int p = expected_result))
      vectors

  let test_have_same_degree () =
    let rec generate_random_non_null () =
      let r = Scalar.random () in
      if Scalar.is_zero r then generate_random_non_null () else r
    in
    let random_non_null = generate_random_non_null () in
    let test_vectors =
      [ (Poly.zero, Poly.zero, true);
        (Poly.zero, Poly.constants random_non_null, false);
        (Poly.constants random_non_null, Poly.zero, false);
        (Poly.constants random_non_null, Poly.constants random_non_null, true);
        ( Poly.generate_random_polynomial (Polynomial.Natural 10),
          Poly.generate_random_polynomial (Polynomial.Natural 10),
          true );
        ( Poly.generate_random_polynomial (Polynomial.Natural 10),
          Poly.zero,
          false );
        ( Poly.generate_random_polynomial (Polynomial.Natural 10),
          Poly.constants (Scalar.random ()),
          false );
        ( Poly.generate_random_polynomial (Polynomial.Natural 10),
          Poly.generate_random_polynomial (Polynomial.Natural 20),
          false );
        ( Poly.generate_random_polynomial (Polynomial.Natural 20),
          Poly.generate_random_polynomial (Polynomial.Natural 10),
          false ) ]
    in
    List.iter
      (fun (p, q, expected_result) ->
        assert (Poly.have_same_degree p q = expected_result))
      test_vectors

  let get_tests () =
    let open Alcotest in
    ( Printf.sprintf
        "Tests on degrees, field order = %s"
        (Z.to_string Scalar.order),
      [ test_case
          "degree of zero is infinity"
          `Quick
          test_degree_zero_is_infinity;
        test_case
          "degree of constants is one"
          `Quick
          test_degree_zero_is_infinity;
        test_case "degree int test vectors" `Quick test_degree_int_test_vectors;
        test_case "have same degree" `Quick test_have_same_degree ] )
end

module MakeTestEvaluation
    (Scalar : Ff_sig.PRIME)
    (Poly : Polynomial.UNIVARIATE with type scalar = Scalar.t) =
struct
  let test_eval_random_point_zero_polynomial () =
    assert (Scalar.is_zero (Poly.evaluation Poly.zero (Scalar.random ())))

  let test_eval_at_zero_of_zero_polynomial () =
    assert (Scalar.is_zero (Poly.evaluation Poly.zero Scalar.zero))

  let test_eval_at_zero_point_of_random_constant_polynomial () =
    let constant = Scalar.random () in
    assert (
      Scalar.eq (Poly.evaluation (Poly.constants constant) Scalar.zero) constant
    )

  let test_eval_random_point_constant_polynomial () =
    let constant = Scalar.random () in
    assert (
      Scalar.eq
        (Poly.evaluation (Poly.constants constant) (Scalar.random ()))
        constant )

  let test_eval_x_to_random_point () =
    let p = Scalar.random () in
    assert (
      Scalar.eq (Poly.evaluation (Poly.of_coefficients [(Scalar.one, 1)]) p) p
    )

  let get_tests () =
    let open Alcotest in
    ( Printf.sprintf
        "Test evaluation, field order = %s"
        (Z.to_string Scalar.order),
      [ test_case
          "evaluation at any point of the zero polynomial"
          `Quick
          (repeat 100 test_eval_random_point_zero_polynomial);
        test_case
          "evaluation at any point of a random constant polynomial"
          `Quick
          (repeat 100 test_eval_random_point_constant_polynomial);
        test_case
          "evaluation at zero of a random constant polynomial"
          `Quick
          (repeat 100 test_eval_at_zero_point_of_random_constant_polynomial);
        test_case
          "evaluation at zero of the zero polynomial"
          `Quick
          (repeat 100 test_eval_at_zero_of_zero_polynomial);
        test_case
          "evaluation at any point of the polynomial X"
          `Quick
          (repeat 100 test_eval_x_to_random_point) ] )
end

module MakeTestLagrangeInterpolation
    (Scalar : Ff_sig.PRIME)
    (Poly : Polynomial.UNIVARIATE with type scalar = Scalar.t) =
struct
  let rec test_with_random_number_of_points () =
    let rec generate_evaluation_points i n acc =
      if i < n then
        let r = Scalar.random () in
        if List.mem r acc then generate_evaluation_points i n acc
        else generate_evaluation_points (i + 1) n (r :: acc)
      else acc
    in
    let n = Random.int 30 in
    if n <= 0 then test_with_random_number_of_points ()
    else
      let points =
        List.combine
          (generate_evaluation_points 0 n [])
          (List.init n (fun _i -> Scalar.random ()))
      in
      let interpolated_polynomial = Poly.lagrange_interpolation points in
      match Poly.degree interpolated_polynomial with
      | Polynomial.Infinity ->
          if
            List.length points = 1
            &&
            let (_, x) = List.hd points in
            Scalar.is_zero x
          then assert true
          else assert false
      | Natural n ->
          assert (n <= List.length points - 1) ;
          List.iter
            (fun (x, y) ->
              assert (Scalar.eq (Poly.evaluation interpolated_polynomial x) y))
            points

  let get_tests () =
    let open Alcotest in
    ( Printf.sprintf
        "Test lagrange interpolation, prime field order %s"
        (Z.to_string Scalar.order),
      [ test_case
          "test random number of points"
          `Quick
          (repeat 10 test_with_random_number_of_points) ] )
end

module MakeTestEuclidianDivision
    (Scalar : Ff_sig.PRIME)
    (Poly : Polynomial.UNIVARIATE with type scalar = Scalar.t) =
struct
  let test_verify_equality_with_random () =
    let a = Poly.generate_random_polynomial (Polynomial.Natural 100) in
    let b = Poly.generate_random_polynomial (Polynomial.Natural 50) in
    let res = Poly.euclidian_division_opt a b in
    match res with
    | None -> assert false
    | Some (q, r) ->
        assert (Poly.equal a (Poly.add (Poly.polynomial_multiplication b q) r))

  let test_verify_equality_with_random_divided_by_constant () =
    let a =
      Poly.generate_random_polynomial (Polynomial.Natural (Random.int 1000))
    in
    let b = Poly.generate_random_polynomial (Polynomial.Natural 0) in
    let res = Poly.euclidian_division_opt a b in
    match res with
    | None -> assert false
    | Some (q, r) ->
        assert (Poly.equal a (Poly.add (Poly.polynomial_multiplication b q) r))

  let rec test_with_constants () =
    let a = Scalar.random () in
    let b = Scalar.random () in
    if Scalar.is_zero b || Scalar.is_zero a then test_with_constants ()
    else
      let res =
        Poly.euclidian_division_opt (Poly.constants a) (Poly.constants b)
      in
      match res with
      | None -> assert false
      | Some (q, r) ->
          assert (Poly.equal (Poly.constants Scalar.(a / b)) q && Poly.is_null r)

  let get_tests () =
    let open Alcotest in
    ( Printf.sprintf
        "Euclidian division for prime field %s"
        (Z.to_string Scalar.order),
      [ test_case
          "test vectors for random"
          `Quick
          (repeat 10 test_verify_equality_with_random);
        test_case "test with constants" `Quick (repeat 10 test_with_constants);
        test_case
          "test vectors for random divided by constant"
          `Quick
          (repeat 10 test_verify_equality_with_random_divided_by_constant) ] )
end

module MakeTestDensifiedPolynomial
    (Scalar : Ff_sig.PRIME)
    (Poly : Polynomial.UNIVARIATE with type scalar = Scalar.t) =
struct
  let test_vectors () =
    let rec generate_non_null () =
      let r = Scalar.random () in
      if Scalar.is_zero r then generate_non_null () else r
    in
    let x = generate_non_null () in
    let zero = Scalar.zero in
    let test_vectors =
      [ (Poly.zero, [Scalar.zero]);
        (Poly.constants x, [x]);
        (Poly.of_coefficients [(x, 2)], [x; zero; zero]);
        (Poly.of_coefficients [(x, 1)], [x; zero]);
        (Poly.of_coefficients [(x, 3); (x, 1)], [x; zero; x; zero]);
        (Poly.of_coefficients [(x, 4); (x, 1)], [x; zero; zero; x; zero]);
        ( Poly.of_coefficients [(x, 17); (x, 14); (x, 3); (x, 1); (x, 0)],
          [ x;
            zero;
            zero;
            x;
            zero;
            zero;
            zero;
            zero;
            zero;
            zero;
            zero;
            zero;
            zero;
            zero;
            x;
            zero;
            x;
            x ] ) ]
    in
    List.iter
      (fun (v, expected_result) ->
        let r = Poly.get_dense_polynomial_coefficients v in
        assert (expected_result = r))
      test_vectors

  let get_tests () =
    let open Alcotest in
    ( (Printf.sprintf "Dense polynomial coefficients for prime field %s")
        (Z.to_string Scalar.order),
      [test_case "test vectors" `Quick (repeat 10 test_vectors)] )
end

module MakeTestExtendedEuclide
    (Scalar : Ff_sig.PRIME)
    (Poly : Polynomial.UNIVARIATE with type scalar = Scalar.t) =
struct
  let test_random_properties () =
    let test poly_1 poly_2 =
      let (gcd1, u1, v1) = Poly.extended_euclide poly_1 poly_2 in
      let (gcd2, u2, v2) = Poly.extended_euclide poly_2 poly_1 in
      assert (Poly.equal gcd1 gcd2) ;
      assert (Poly.equal u1 v2) ;
      assert (Poly.equal v1 u2) ;
      assert (
        Poly.equal
          (Poly.add
             (Poly.polynomial_multiplication poly_1 u1)
             (Poly.polynomial_multiplication poly_2 v1))
          gcd1 ) ;
      let remainder_poly_1 =
        Poly.euclidian_division_opt poly_1 gcd1 |> Option.get |> snd
      in
      assert (Poly.is_null remainder_poly_1) ;
      let remainder_poly_2 =
        Poly.euclidian_division_opt poly_2 gcd1 |> Option.get |> snd
      in
      assert (Poly.is_null remainder_poly_2)
    in
    let n = Random.int 100 in
    let m = Random.int 50 in
    let poly_1 = Poly.generate_random_polynomial (Polynomial.Natural n) in
    (* let poly_2 = Poly.generate_random_polynomial (Polynomial.Natural n) in *)
    let poly_3 = Poly.generate_random_polynomial (Polynomial.Natural m) in

    (* test poly_1 poly_2 ; *)
    test poly_1 Poly.zero ;
    test Poly.zero poly_1 ;
    test poly_1 poly_3 ;
    test poly_3 poly_1

  let get_tests () =
    let open Alcotest in
    ( Printf.sprintf
        "Extended Euclide alogrithm for prime field %s"
        (Z.to_string Scalar.order),
      [ test_case
          "test properties on random polynomials"
          `Quick
          (repeat 10 test_random_properties) ] )
end

module MakeTestPolynomialMultiplication
    (Scalar : Ff_sig.PRIME)
    (Poly : Polynomial.UNIVARIATE with type scalar = Scalar.t) =
struct
  let test_multiply_by_zero_is_zero () =
    let r = Poly.generate_random_polynomial (Natural (Random.int 1000)) in
    assert (Poly.equal (Poly.polynomial_multiplication r Poly.zero) Poly.zero) ;
    assert (Poly.equal (Poly.polynomial_multiplication Poly.zero r) Poly.zero)

  let test_communitativity () =
    let p = Poly.generate_random_polynomial (Natural (Random.int 30)) in
    let q = Poly.generate_random_polynomial (Natural (Random.int 30)) in
    assert (
      Poly.equal
        (Poly.polynomial_multiplication p q)
        (Poly.polynomial_multiplication q p) )

  let test_distributivity () =
    let a = Scalar.random () in
    let b = Scalar.random () in
    let p = Poly.generate_random_polynomial (Natural (Random.int 30)) in
    let q = Poly.generate_random_polynomial (Natural (Random.int 30)) in
    assert (
      Poly.equal
        (Poly.polynomial_multiplication
           (Poly.mult_by_scalar a p)
           (Poly.mult_by_scalar b q))
        (Poly.polynomial_multiplication
           (Poly.mult_by_scalar a q)
           (Poly.mult_by_scalar b p)) ) ;
    assert (
      Poly.equal
        (Poly.polynomial_multiplication
           (Poly.mult_by_scalar Scalar.(a * b) p)
           q)
        (Poly.polynomial_multiplication
           (Poly.mult_by_scalar Scalar.(a * b) q)
           p) ) ;
    assert (
      Poly.equal
        (Poly.polynomial_multiplication
           p
           (Poly.mult_by_scalar Scalar.(a * b) q))
        (Poly.polynomial_multiplication
           q
           (Poly.mult_by_scalar Scalar.(a * b) p)) ) ;
    assert (
      Poly.equal
        (Poly.polynomial_multiplication
           (Poly.mult_by_scalar a p)
           (Poly.mult_by_scalar b q))
        Poly.(mult_by_scalar Scalar.(a * b) (polynomial_multiplication p q)) )

  let get_tests () =
    let open Alcotest in
    ( Printf.sprintf
        "Polynomial multiplication for prime field %s"
        (Z.to_string Scalar.order),
      [ test_case
          "test properties nullifier 0 * P = P * 0 = 0"
          `Quick
          (repeat 10 test_multiply_by_zero_is_zero);
        test_case
          "test properties commutativity p * q = p * q"
          `Quick
          (repeat 10 test_communitativity);
        test_case
          "test properties distributivity and communtativity a p * b q = (a * \
           b) (p * q) = (b p) * (a q) = p * (a * b) q"
          `Quick
          (repeat 10 test_distributivity) ] )
end
