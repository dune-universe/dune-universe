open! Core
open! Expect_test_helpers_kernel

let%test_unit "Bignum.(//)" =
  let open Bignum.O in
  for i = -4 to 4 do
    for j = -4 to 4 do
      [%test_eq: Bignum.t] (i // j) (of_int i / of_int j)
    done
  done
;;

let%test_unit "Bignum.(//)" =
  let open Bignum.O in
  Quickcheck.test
    (Quickcheck.Generator.tuple2 Int.gen Int.gen)
    ~sexp_of:[%sexp_of: int * int]
    ~f:(fun (i, j) -> [%test_eq: Bignum.t] (i // j) (of_int i / of_int j))
;;

let compare_float_and_bignum_repr ~include_scientific_notation x =
  let s =
    let s = Float.to_string x in
    Option.value (String.chop_suffix s ~suffix:".") ~default:s
  in
  if not include_scientific_notation && String.contains s 'e'
  then ()
  else (
    let s' =
      Bignum.of_string s
      |> Bignum.sexp_of_t
      |> Sexp.to_string
    in
    match s, s' with
    | "-0", "0" -> ()
    | _ ->
      if not (String.(=) s s') then
        raise_s [%message "mismatch" (s : string) (s' : string)])
;;

let%expect_test "Bignum.sexp_of_t matches Float.to_string when it can" =
  require_does_not_raise [%here] (fun () ->
    Quickcheck.test ~sexp_of:[%sexp_of:float] Float.gen_finite ~f:(fun x ->
      compare_float_and_bignum_repr ~include_scientific_notation:false x));
  [%expect {| |}]
;;

let%expect_test "Bignum.sexp_of_t does use Scientific Notation" =
  show_raise (fun () ->
    Quickcheck.test ~sexp_of:[%sexp_of:float] Float.gen_finite ~f:(fun x ->
      compare_float_and_bignum_repr ~include_scientific_notation:true x));
  [%expect {|
    (raised (
      "random input"
      (value -3.950862943457765E-284)
      (error (
        (mismatch
          (s -3.950862943457765e-284)
          (s'
           -0.00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000003950862943457765))
        "")))) |}]
;;

let compare_floats ~of_float x =
  let x' =
    x
    |> of_float
    |> Bignum.to_float
  in
  if not (Float.(=) x x' || (Float.is_nan x && Float.is_nan x'))
  then
    raise_s [%message "mismatch" (x : float) (x' : float)]
;;

let%expect_test "roundtrip: f |> Bignum.of_float_decimal |> Bignum.to_float" =
  require_does_not_raise [%here] (fun () ->
    Quickcheck.test ~sexp_of:[%sexp_of:float] Float.gen ~f:(fun x ->
      let skip_test_for_now =
        match Float.classify x with
        | Subnormal -> true
        | Infinite | Nan
        | Normal | Zero -> false
      in
      if not skip_test_for_now
      then compare_floats ~of_float:Bignum.of_float_decimal x));
  [%expect {| |}]
;;

let%expect_test "Be notified when [Zarith.Q.to_float] will be fixed" =
  require_does_raise [%here] (fun () ->
    let x = 7.56181796669062E-309 in
    let x' = x |> Bignum.of_float_decimal |> Bignum.to_float in
    if not (Float.(=) x x')
    then raise_s [%message "mismatch" (x : float) (x' : float)]);
  [%expect {|
    (mismatch
      (x  7.56181796669062E-309)
      (x' 7.561817966690623E-309)) |}];
;;

let%expect_test "roundtrip: f |> Bignum.of_float_dyadic |> Bignum.to_float" =
  require_does_not_raise [%here] (fun () ->
    Quickcheck.test ~sexp_of:[%sexp_of:float] Float.gen ~f:(fun x ->
      compare_floats ~of_float:Bignum.of_float_dyadic x));
  [%expect {| |}]
;;

let%expect_test "to_string_accurate |> of_string" =
  require_does_not_raise [%here] (fun () ->
    Quickcheck.test ~sexp_of:[%sexp_of:Bignum.t] Bignum.gen ~f:(fun x ->
      [%test_result: Bignum.t] ~expect:x
        (x |> Bignum.to_string_accurate |> Bignum.of_string)));
  [%expect {| |}];
;;

let%expect_test "to_string_accurate matches sexp_of_t" =
  require_does_not_raise [%here] (fun () ->
    Quickcheck.test ~sexp_of:[%sexp_of:Bignum.t] Bignum.gen ~f:(fun x ->
      [%test_result: string] ~expect:(Bignum.to_string_accurate x)
        (x |> Bignum.sexp_of_t |> Sexp.to_string)));
  [%expect {| |}];
;;

let%expect_test "to_string_hum |> of_string" =
  require_does_not_raise [%here] (fun () ->
    Quickcheck.test ~sexp_of:[%sexp_of:Bignum.t] Bignum.gen ~f:(fun x ->
      let decimals = 9 in
      let dx  = Bignum.to_string_hum ~decimals x |> Bignum.of_string in
      let dx2 = Bignum.to_string_hum ~decimals ~delimiter:'_' x |> Bignum.of_string in
      [%test_eq: Bignum.t] dx dx2;
      let expect =
        if Bignum.is_zero (Bignum.den x)
        then x
        else Bignum.round_decimal_to_nearest_half_to_even ~digits:decimals x
      in
      [%test_result: Bignum.t] ~expect dx));
  [%expect {| |}];
;;

let%expect_test "Float.to_string_hum matches Bignum.to_string_hum" =
  require_does_not_raise [%here] (fun () ->
    let delimiter = '_' and decimals = 7 in
    Quickcheck.test ~sexp_of:[%sexp_of:float] Float.gen_without_nan ~f:(fun x ->
      let s1 = Float. to_string_hum ~delimiter ~decimals ~strip_zero:false x in
      let s2 = Bignum.to_string_hum ~delimiter ~decimals ~strip_zero:false
                 (Bignum.of_float_dyadic x)
      in
      match s1, s2 with
      | "-0.0000000", "0.0000000" (* An acceptable difference. *) -> ()
      | _ -> [%test_eq: string] s1 s2));
  [%expect {| |}];
;;

let%expect_test "to_string_hum" =
  let test ?delimiter ?decimals ?strip_zero str =
    Bignum.to_string_hum ?delimiter ?decimals ?strip_zero (Bignum.of_string str)
    |> print_endline
  in
  test "0";
  [%expect {| 0 |}];
  test "1";
  [%expect {| 1 |}];
  test "10_000_000_000" ~delimiter:',';
  [%expect {| 10,000,000,000 |}];
  test "1_000" ~delimiter:',' ~strip_zero:false ~decimals:0;
  [%expect {| 1,000. |}];
  test "1_000" ~delimiter:',' ~strip_zero:false ~decimals:2;
  [%expect {| 1,000.00 |}];
  test "inf";
  [%expect {| inf |}];
  test "-inf";
  [%expect {| -inf |}];
  test "nan";
  [%expect {| nan |}];
  test "1" ~strip_zero:false;
  [%expect {| 1.000000000 |}];
  test "1000.1999" ~strip_zero:false ~decimals:3 ~delimiter:',';
  [%expect {| 1,000.200 |}];
  test "-1000.1999" ~strip_zero:false ~decimals:3 ~delimiter:'_';
  [%expect {| -1_000.200 |}];
  test "1000.1999" ~strip_zero:false ~decimals:5;
  [%expect {| 1000.19990 |}];
  test "1000.1999" ~strip_zero:true  ~decimals:5;
  [%expect {| 1000.1999 |}];
  test "3194450417.0351562";
  [%expect {| 3194450417.0351562 |}];
  test "1.25" ~decimals:1;
  [%expect {| 1.2 |}];
  print_endline (Float.to_string_hum 1.25 ~decimals:1);
  [%expect {| 1.2 |}];
  test "-1.25" ~decimals:1;
  [%expect {| -1.2 |}];
  print_endline (Float.to_string_hum (-.1.25) ~decimals:1);
  [%expect {| -1.2 |}];
  test "1.35" ~decimals:1;
  [%expect {| 1.4 |}];
  print_endline (Float.to_string_hum 1.35 ~decimals:1);
  [%expect {| 1.4 |}];
  test "-1.35" ~decimals:1;
  [%expect {| -1.4 |}];
  print_endline (Float.to_string_hum (-.1.35) ~decimals:1);
  [%expect {| -1.4 |}];
;;

let%test_module "Bignum.gen" =
  (module struct
    let sexp_of = Bignum.sexp_of_t
    let compare = Bignum.compare
    ;;

    let%test_unit _ =
      Quickcheck.test_distinct_values
        Bignum.gen
        ~sexp_of
        ~compare
        ~trials:1_000
        ~distinct_values:500
    ;;

    let%test_unit _ =
      Quickcheck.test_can_generate ~sexp_of Bignum.gen ~f:(fun bignum ->
        Bignum.equal bignum Bignum.zero)
    ;;

    let%test_unit _ =
      Quickcheck.test_can_generate ~sexp_of Bignum.gen ~f:(fun bignum ->
        Bignum.equal bignum Bignum.one)
    ;;

    let%test_unit _ =
      Quickcheck.test_can_generate ~sexp_of Bignum.gen ~f:(fun bignum ->
        Bignum.equal bignum (Bignum.neg Bignum.one))
    ;;

    let%test_unit _ =
      Quickcheck.test_can_generate ~sexp_of Bignum.gen ~f:(fun bignum ->
        Bignum.( > ) bignum Bignum.one
        && Option.is_some (Bignum.to_int bignum)
        && Bignum.equal bignum (Bignum.of_int (Bignum.to_int_exn bignum)))
    ;;

    let%test_unit _ =
      Quickcheck.test_can_generate ~sexp_of Bignum.gen ~f:(fun bignum ->
        Bignum.( < ) bignum (Bignum.neg Bignum.one)
        && Option.is_some (Bignum.to_int bignum)
        && Bignum.equal bignum (Bignum.of_int (Bignum.to_int_exn bignum)))
    ;;

    let%test_unit _ =
      Quickcheck.test_can_generate ~sexp_of Bignum.gen ~f:(fun bignum ->
        Bignum.( > ) bignum Bignum.zero
        && Bigint.equal (Bignum.den_as_bigint bignum) Bigint.one
        && Option.is_none (Bigint.to_int (Bignum.num_as_bigint bignum)))
    ;;

    let%test_unit _ =
      Quickcheck.test_can_generate ~sexp_of Bignum.gen ~f:(fun bignum ->
        Bignum.( < ) bignum Bignum.zero
        && Bigint.equal (Bignum.den_as_bigint bignum) Bigint.one
        && Option.is_none (Bigint.to_int (Bignum.num_as_bigint bignum)))
    ;;

    let%test_unit _ =
      Quickcheck.test_can_generate ~sexp_of Bignum.gen ~f:(fun bignum ->
        Bignum.( > ) bignum Bignum.zero
        && Option.is_none (Bigint.to_int (Bignum.num_as_bigint bignum))
        && Option.is_none (Bigint.to_int (Bignum.den_as_bigint bignum)))
    ;;

    let%test_unit _ =
      Quickcheck.test_can_generate ~sexp_of Bignum.gen ~f:(fun bignum ->
        Bignum.( < ) bignum Bignum.zero
        && Option.is_none (Bigint.to_int (Bignum.num_as_bigint bignum))
        && Option.is_none (Bigint.to_int (Bignum.den_as_bigint bignum)))
    ;;

    let%test_unit _ =
      Quickcheck.test_can_generate ~sexp_of Bignum.gen ~f:(fun bignum ->
        let float = Bignum.to_float bignum in
        Float.( > ) float 0. && Float.( < ) float Float.epsilon_float)
    ;;

    let%test_unit _ =
      Quickcheck.test_can_generate ~sexp_of Bignum.gen ~f:(fun bignum ->
        let float = Bignum.to_float bignum in
        Float.( < ) float 0. && Float.( > ) float (-. Float.epsilon_float))
    ;;
  end)

let%test_unit "Bignum.gen_uniform_excl" =
  Quickcheck.test
    (Bignum.gen_uniform_excl Bignum.zero Bignum.one)
    ~sexp_of:Bignum.sexp_of_t
    ~f:(fun bignum ->
      assert (Bignum.between bignum ~low:Bignum.zero ~high:Bignum.one))
;;
