open! Core_kernel
open! Expect_test_helpers_core
open Bignum
open Bignum.For_testing

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
    (Quickcheck.Generator.tuple2 Int.quickcheck_generator Int.quickcheck_generator)
    ~sexp_of:[%sexp_of: int * int]
    ~f:(fun (i, j) -> [%test_eq: Bignum.t] (i // j) (of_int i / of_int j))
;;

let compare_float_and_bignum_repr ~include_scientific_notation x =
  let s =
    let s = Float.to_string x in
    Option.value (String.chop_suffix s ~suffix:".") ~default:s
  in
  if (not include_scientific_notation) && String.contains s 'e'
  then ()
  else (
    let s' = Bignum.of_string s |> Bignum.sexp_of_t |> Sexp.to_string in
    match s, s' with
    | "-0", "0" -> ()
    | _ ->
      if not (String.( = ) s s')
      then raise_s [%message "mismatch" (s : string) (s' : string)])
;;

let%expect_test "Bignum.sexp_of_t matches Float.to_string when it can" =
  require_does_not_raise [%here] (fun () ->
    Quickcheck.test ~sexp_of:[%sexp_of: float] Float.gen_finite ~f:(fun x ->
      compare_float_and_bignum_repr ~include_scientific_notation:false x));
  [%expect {| |}]
;;

let%expect_test "Bignum.sexp_of_t does use Scientific Notation" =
  show_raise (fun () ->
    Quickcheck.test ~sexp_of:[%sexp_of: float] Float.gen_finite ~f:(fun x ->
      compare_float_and_bignum_repr ~include_scientific_notation:true x));
  [%expect
    {|
    (raised (
      "Base_quickcheck.Test.run: test failed"
      (input -3.950862943457765E-284)
      (error (
        mismatch
        (s -3.950862943457765e-284)
        (s'
         -0.00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000003950862943457765))))) |}]
;;

let compare_floats ~of_float x =
  let x' = x |> of_float |> Bignum.to_float in
  if not (Float.( = ) x x' || (Float.is_nan x && Float.is_nan x'))
  then raise_s [%message "mismatch" (x : float) (x' : float)]
;;

let%expect_test "roundtrip: f |> Bignum.of_float_decimal |> Bignum.to_float" =
  require_does_not_raise [%here] (fun () ->
    Quickcheck.test ~sexp_of:[%sexp_of: float] Float.quickcheck_generator ~f:(fun x ->
      let skip_test_for_now =
        match Float.classify x with
        | Subnormal -> true
        | Infinite | Nan | Normal | Zero -> false
      in
      if not skip_test_for_now
      then compare_floats ~of_float:Bignum.of_float_decimal x));
  [%expect {| |}]
;;

let%expect_test "Be notified when [Zarith.Q.to_float] will be fixed" =
  require_does_raise [%here] (fun () ->
    let x = 7.56181796669062E-309 in
    let x' = x |> Bignum.of_float_decimal |> Bignum.to_float in
    if not (Float.( = ) x x')
    then raise_s [%message "mismatch" (x : float) (x' : float)]);
  [%expect
    {|
    (mismatch
      (x  7.56181796669062E-309)
      (x' 7.561817966690623E-309)) |}]
;;

let%expect_test "roundtrip: f |> Bignum.of_float_dyadic |> Bignum.to_float" =
  require_does_not_raise [%here] (fun () ->
    Quickcheck.test ~sexp_of:[%sexp_of: float] Float.quickcheck_generator ~f:(fun x ->
      compare_floats ~of_float:Bignum.of_float_dyadic x));
  [%expect {| |}]
;;

let%expect_test "to_string_accurate |> of_string" =
  require_does_not_raise [%here] (fun () ->
    Quickcheck.test
      ~sexp_of:[%sexp_of: Bignum.t]
      Bignum.quickcheck_generator
      ~f:(fun x ->
        [%test_result: Bignum.t]
          ~expect:x
          (x |> Bignum.to_string_accurate |> Bignum.of_string)));
  [%expect {| |}]
;;

let%expect_test "to_string_accurate matches sexp_of_t" =
  require_does_not_raise [%here] (fun () ->
    Quickcheck.test
      ~sexp_of:[%sexp_of: Bignum.t]
      Bignum.quickcheck_generator
      ~f:(fun x ->
        [%test_result: string]
          ~expect:(Bignum.to_string_accurate x)
          (x |> Bignum.sexp_of_t |> Sexp.to_string)));
  [%expect {| |}]
;;

let%expect_test "to_string_hum |> of_string" =
  require_does_not_raise [%here] (fun () ->
    Quickcheck.test
      ~sexp_of:[%sexp_of: Bignum.t]
      Bignum.quickcheck_generator
      ~f:(fun x ->
        let decimals = 9 in
        let dx = Bignum.to_string_hum ~decimals x |> Bignum.of_string in
        let dx2 =
          Bignum.to_string_hum ~decimals ~delimiter:'_' x |> Bignum.of_string
        in
        [%test_eq: Bignum.t] dx dx2;
        let expect =
          if Bignum.is_zero (Bignum.den x)
          then x
          else Bignum.round_decimal_to_nearest_half_to_even ~digits:decimals x
        in
        [%test_result: Bignum.t] ~expect dx));
  [%expect {| |}]
;;

let%expect_test ("Float.to_string_hum matches Bignum.to_string_hum"[@tags "no-js"]) =
  require_does_not_raise [%here] (fun () ->
    let delimiter = '_'
    and decimals = 7 in
    Quickcheck.test ~sexp_of:[%sexp_of: float] Float.gen_without_nan ~f:(fun x ->
      let s1 = Float.to_string_hum ~delimiter ~decimals ~strip_zero:false x in
      let s2 =
        Bignum.to_string_hum
          ~delimiter
          ~decimals
          ~strip_zero:false
          (Bignum.of_float_dyadic x)
      in
      match s1, s2 with
      | "-0.0000000", "0.0000000" (* An acceptable difference. *) -> ()
      | _ -> [%test_eq: string] s1 s2));
  [%expect {| |}]
;;

(* The following two tests illustrate the difference between js and native wrt float
   printing. This difference is the reason the test above was disabled in JavaScript.
*)

let%expect_test ("Float.to_string_hum big exponents (no-js)"[@tags "no-js"]) =
  let x = -3.3810849992682576E+37 in
  print_s
    [%sexp (Float.to_string_hum ~delimiter:'_' ~decimals:7 ~strip_zero:false x : string)];
  [%expect {| -33_810_849_992_682_574_344_623_022_087_906_263_040.0000000 |}]
;;

let%expect_test ("Float.to_string_hum big exponents (js-only)"[@tags "js-only"]) =
  let x = -3.3810849992682576E+37 in
  print_s
    [%sexp (Float.to_string_hum ~delimiter:'_' ~decimals:7 ~strip_zero:false x : string)];
  [%expect {| -33_810_849_992_682_576_000_000_000_000_000_000_000.0000000 |}]
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
  test "1000.1999" ~strip_zero:true ~decimals:5;
  [%expect {| 1000.1999 |}];
  test "3194450417.0351562";
  [%expect {| 3194450417.0351562 |}];
  test "1.25" ~decimals:1;
  [%expect {| 1.2 |}];
  test "-1.25" ~decimals:1;
  [%expect {| -1.2 |}];
  test "1.35" ~decimals:1;
  [%expect {| 1.4 |}];
  print_endline (Float.to_string_hum 1.35 ~decimals:1);
  [%expect {| 1.4 |}];
  test "-1.35" ~decimals:1;
  [%expect {| -1.4 |}];
  print_endline (Float.to_string_hum (-1.35) ~decimals:1);
  [%expect {| -1.4 |}]
;;

let%expect_test ("to_string_hum, tie resolved differently, native"[@tags "no-js"]) =
  print_endline (Float.to_string_hum (-1.25) ~decimals:1);
  [%expect {| -1.2 |}];
  print_endline (Float.to_string_hum 1.25 ~decimals:1);
  [%expect {| 1.2 |}]
;;

let%expect_test ("to_string_hum, tie resolved differently, js"[@tags "js-only"]) =
  print_endline (Float.to_string_hum (-1.25) ~decimals:1);
  [%expect {| -1.3 |}];
  print_endline (Float.to_string_hum 1.25 ~decimals:1);
  [%expect {| 1.3 |}]
;;

let%test_module "Bignum.gen" =
  (module struct
    let sexp_of = Bignum.sexp_of_t
    let compare = Bignum.compare

    let%test_unit _ =
      Quickcheck.test_distinct_values
        Bignum.quickcheck_generator
        ~sexp_of
        ~compare
        ~trials:1_000
        ~distinct_values:500
    ;;

    let%test_unit _ =
      Quickcheck.test_can_generate ~sexp_of Bignum.quickcheck_generator ~f:(fun bignum ->
        Bignum.equal bignum Bignum.zero)
    ;;

    let%test_unit _ =
      Quickcheck.test_can_generate ~sexp_of Bignum.quickcheck_generator ~f:(fun bignum ->
        Bignum.equal bignum Bignum.one)
    ;;

    let%test_unit _ =
      Quickcheck.test_can_generate ~sexp_of Bignum.quickcheck_generator ~f:(fun bignum ->
        Bignum.equal bignum (Bignum.neg Bignum.one))
    ;;

    let%test_unit _ =
      Quickcheck.test_can_generate ~sexp_of Bignum.quickcheck_generator ~f:(fun bignum ->
        Bignum.( > ) bignum Bignum.one
        && Option.is_some (Bignum.to_int bignum)
        && Bignum.equal bignum (Bignum.of_int (Bignum.to_int_exn bignum)))
    ;;

    let%test_unit _ =
      Quickcheck.test_can_generate ~sexp_of Bignum.quickcheck_generator ~f:(fun bignum ->
        Bignum.( < ) bignum (Bignum.neg Bignum.one)
        && Option.is_some (Bignum.to_int bignum)
        && Bignum.equal bignum (Bignum.of_int (Bignum.to_int_exn bignum)))
    ;;

    let%test_unit _ =
      Quickcheck.test_can_generate ~sexp_of Bignum.quickcheck_generator ~f:(fun bignum ->
        Bignum.( > ) bignum Bignum.zero
        && Bigint.equal (Bignum.den_as_bigint bignum) Bigint.one
        && Option.is_none (Bigint.to_int (Bignum.num_as_bigint bignum)))
    ;;

    let%test_unit _ =
      Quickcheck.test_can_generate ~sexp_of Bignum.quickcheck_generator ~f:(fun bignum ->
        Bignum.( < ) bignum Bignum.zero
        && Bigint.equal (Bignum.den_as_bigint bignum) Bigint.one
        && Option.is_none (Bigint.to_int (Bignum.num_as_bigint bignum)))
    ;;

    let%test_unit _ =
      Quickcheck.test_can_generate ~sexp_of Bignum.quickcheck_generator ~f:(fun bignum ->
        Bignum.( > ) bignum Bignum.zero
        && Option.is_none (Bigint.to_int (Bignum.num_as_bigint bignum))
        && Option.is_none (Bigint.to_int (Bignum.den_as_bigint bignum)))
    ;;

    let%test_unit _ =
      Quickcheck.test_can_generate ~sexp_of Bignum.quickcheck_generator ~f:(fun bignum ->
        Bignum.( < ) bignum Bignum.zero
        && Option.is_none (Bigint.to_int (Bignum.num_as_bigint bignum))
        && Option.is_none (Bigint.to_int (Bignum.den_as_bigint bignum)))
    ;;

    let%test_unit _ =
      Quickcheck.test_can_generate ~sexp_of Bignum.quickcheck_generator ~f:(fun bignum ->
        let float = Bignum.to_float bignum in
        Float.( > ) float 0. && Float.( < ) float Float.epsilon_float)
    ;;

    let%test_unit _ =
      Quickcheck.test_can_generate ~sexp_of Bignum.quickcheck_generator ~f:(fun bignum ->
        let float = Bignum.to_float bignum in
        Float.( < ) float 0. && Float.( > ) float (-.Float.epsilon_float))
    ;;
  end)
;;

let%test_unit "Bignum.gen_uniform_excl" =
  Quickcheck.test
    (Bignum.gen_uniform_excl Bignum.zero Bignum.one)
    ~sexp_of:Bignum.sexp_of_t
    ~f:(fun bignum -> assert (Bignum.between bignum ~low:Bignum.zero ~high:Bignum.one))
;;

let%test_unit "of_string_internal parse failures" =
  List.iter
    [ ""
    ; "hello"
    ; "123x"
    ; "."
    ; "-."
    ; "1.2.3"
    ; "+-1"
    ; "--1"
    ; "-+1"
    ; "++1"
    ; "-.e1"
    ; "e1"
    ; "e-1"
    ; "1e1.5"
    ]
    ~f:
      ([%test_pred: string] (fun s ->
         let doesn't_parse_with (type a) (f : _ -> a) =
           try
             let (_ : a) = f s in
             false
           with
           | _ -> true
         in
         doesn't_parse_with of_string_internal && doesn't_parse_with Float.of_string))
;;

let equal = equal

let%test _ =
  not
    (equal (of_float_dyadic 766.46249999999997726) (of_float_dyadic 766.462499999999864))
;;

let%test _ = equal (of_string_internal "+1/2") (of_string_internal ".5")
let%test _ = equal (of_string_internal "-1/2") (of_string_internal "-.5")
let%test _ = equal (of_string_internal "100_000") (of_string_internal "100000")
let%test _ = equal (of_string_internal "100_000") (of_int 100_000)
let%test _ = equal (of_string_internal "100_000.") (of_int 100_000)
let%test _ = equal (of_string_internal "100__000.") (of_int 100_000)
let%test _ = equal (of_string_internal "100_000.0_") (of_int 100_000)
let%test _ = equal (of_string_internal "-_1_0_/0_1") (of_int (-10))
let%test _ = equal (of_string_internal "+_1_0_/0_1") (of_int 10)
let%test _ = equal (of_string_internal ".00000000") zero
let%test _ = equal (of_string_internal "-.00000000") zero
let%test _ = equal (of_string_internal "-0.") zero
let%test _ = equal (of_string_internal "+0.") zero

let%expect_test "to_string_decimal_truncate" =
  let test t =
    t |> of_string |> to_string_decimal_truncate ~max_decimal_digits:9 |> print_endline
  in
  test "1/3";
  [%expect "0.333333333"]
;;

let%expect_test "Monitor chosen cases that have exhibited regressions at some point" =
  let test t = print_endline (Sexp.to_string (sexp_of_t (of_string t))) in
  test "0";
  [%expect {| 0 |}];
  test "-0";
  [%expect {| 0 |}];
  test "1024";
  [%expect {| 1024 |}];
  test "1/3";
  [%expect {| (0.333333333 + 1/3000000000) |}];
  test "-1/3";
  [%expect {| (-0.333333333 + -1/3000000000) |}];
  (* A decimal not representable as a float. *)
  test "3.14";
  [%expect {| 3.14 |}];
  (* Distinguish > 9 digits. *)
  test "22.4359988250446";
  [%expect {| 22.4359988250446 |}];
  test "22.4359988250445";
  [%expect {| 22.4359988250445 |}];
  (* Large int whose SN representations as long or shorter than decimal string. *)
  test "4.79538294005e+16";
  [%expect {| 47953829400500000 |}];
  test "4.79538294005e+20";
  [%expect {| 479538294005000000000 |}];
  (* Behavior for decimal < billionth. *)
  test "4.79538294005e-16";
  [%expect {| 0.000000000000000479538294005 |}];
  (* With some version of the code, there is a discontinuity between e-09 and e-10,
     with other it occurs between e-11 and e-12.  Monitor the range 09-12 here: *)
  test "3.062e-09";
  [%expect {| 0.000000003062 |}];
  test "3.062e-10";
  [%expect {| 0.0000000003062 |}];
  test "3.062e-11";
  [%expect {| 0.00000000003062 |}];
  test "3.062e-12";
  [%expect {| 0.000000000003062 |}];
  (* Decimals that should be displayed as such. *)
  test "(694943.472 + 7/100000000000)";
  [%expect {| 694943.47200000007 |}];
  test "(-694943.472 + -7/100000000000)";
  [%expect {| -694943.47200000007 |}];
  test "694943.47200000007";
  [%expect {| 694943.47200000007 |}];
  test "1894603.2000000002";
  [%expect {| 1894603.2000000002 |}];
  test "-3730192.0000000005";
  [%expect {| -3730192.0000000005 |}]
;;

let%expect_test _ =
  let test t = printf !"%{sexp:string Or_error.t}\n" (to_string_decimal_accurate t) in
  test (of_int 173 / of_int64 Int64.(one lsl 61));
  [%expect {| (Ok 0.0000000000000000750267903359969068333157338201999664306640625) |}];
  test (of_int 173 / of_bigint Bigint.(of_int 5 ** of_int 61));
  [%expect {| (Ok 0.0000000000000000000000000000000000000000398910840593969053696) |}];
  test (of_int 1745 / of_int64 1_000_000_000_000_000L);
  [%expect {| (Ok 0.000000000001745) |}];
  test (of_int 1745 / of_int 100);
  [%expect {| (Ok 17.45) |}];
  test (of_int 1 / of_int 3);
  [%expect {| (Error ("Not representable as decimal" (0.333333333 + 1/3000000000))) |}];
  test (of_int (-1) / of_int 3);
  [%expect {| (Error ("Not representable as decimal" (-0.333333333 + -1/3000000000))) |}];
  test (of_int 1 / of_int 7);
  [%expect {| (Error ("Not representable as decimal" (0.142857142 + 3/3500000000))) |}];
  test (of_int 1 / of_int 15);
  [%expect {| (Error ("Not representable as decimal" (0.066666666 + 1/1500000000))) |}];
  test (of_int 1 / of_int 21000);
  [%expect {| (Error ("Not representable as decimal" (0.000047619 + 1/21000000000))) |}];
  test (of_int 1 / of_int 0);
  [%expect {| (Error ("Not representable as decimal" inf)) |}];
  test (of_int 0 / of_int 0);
  [%expect {| (Error ("Not representable as decimal" nan)) |}]
;;

(* These are down here instead of with of_string because [%test_result: t] uses
   [sexp_of_t]. *)
let%test_unit "of_string matches Float.of_string" =
  let as_float s =
    [%test_result: t]
      ~expect:(of_float_dyadic (Float.of_string s))
      (of_string_internal s)
  in
  List.iter
    (* All representable exactly as floats *)
    [ "0"
    ; ".0"
    ; "0."
    ; "00"
    ; "1"
    ; ".5"
    ; "1."
    ; "01"
    ; "0.25"
    ; "0.0625"
    ; ".0625"
    ; "01.0625"
    ; "1.375"
    ; "1.75"
    ; "99.9375"
    ; "1.2e5"
    ; "1.2E5"
    ; "0.5e0"
    ; "125e-3"
    ]
    ~f:(fun s ->
      as_float s;
      as_float ("+" ^ s);
      as_float ("-" ^ s))
;;

let minus_one = of_int (-1)

module Extra_constants = struct
  let nan = zero / zero
  let infinity = one / zero
  let neg_infinity = minus_one / zero
end

open Extra_constants

let%test _ = equal (t_of_sexp (sexp_of_t nan)) nan
let%test _ = equal (t_of_sexp (sexp_of_t infinity)) infinity
let%test _ = equal (t_of_sexp (sexp_of_t neg_infinity)) neg_infinity

let%expect_test "to_float on degenerated cases" =
  let test str =
    print_endline (Sexp.to_string (Float.sexp_of_t (to_float (of_string str))))
  in
  test "inf";
  [%expect {| INF |}];
  test "-inf";
  [%expect {| -INF |}];
  test "nan";
  [%expect {| NAN |}];
  test "-nan";
  [%expect {| NAN |}]
;;

let%test_module _ =
  (module struct
    open Bignum.Stable.V1.For_testing

    let%test _ = equal (of_binable (to_binable nan)) nan
    let%test _ = equal (of_binable (to_binable infinity)) infinity
    let%test _ = equal (of_binable (to_binable neg_infinity)) neg_infinity
  end)
;;

let%test_module _ =
  (module struct
    open Bignum.Stable.V2.For_testing

    let%test _ = equal (of_binable (to_binable nan)) nan
    let%test _ = equal (of_binable (to_binable infinity)) infinity
    let%test _ = equal (of_binable (to_binable neg_infinity)) neg_infinity
  end)
;;

let%test_module _ =
  (module struct
    open! Core_kernel
    module Current = Bignum
    open Stable
    open Extra_constants

    let buf = Bigstring.create 256

    let roundtrip b =
      for pos = 0 to 17 do
        let (_ : int) = V1.bin_writer_t.Bin_prot.Type_class.write buf ~pos b in
        let result1 = V1.bin_reader_t.Bin_prot.Type_class.read buf ~pos_ref:(ref pos) in
        let (_ : int) = V2.bin_writer_t.Bin_prot.Type_class.write buf ~pos b in
        let result2 = V2.bin_reader_t.Bin_prot.Type_class.read buf ~pos_ref:(ref pos) in
        [%test_eq: V1.t] b result1;
        [%test_eq: V2.t] b result2
      done
    ;;

    let test b =
      let open Core_kernel in
      let v1 = Bin_prot.Writer.to_string V1.bin_writer_t b |> String.length in
      let v2 = Bin_prot.Writer.to_string V2.bin_writer_t b |> String.length in
      (* change to true if you want to see compaction rates during testing *)
      if false
      then printf "%s v1: %i v2: %i\n" (V1.sexp_of_t b |> Sexp.to_string_mach) v1 v2;
      roundtrip b
    ;;

    (* This checks an axiom used in the proof of [check_overflow] *)
    let%test _ = Int.(-max_value > min_value)
    (* This contains a test for all branches.*)
    let%test_unit _ = test Current.zero (* test for Zero *)
    let%test_unit _ = test Current.one (* test for Int *)
    let%test_unit _ = test Current.ten
    let%test_unit _ = test Current.hundred
    let%test_unit _ = test Current.thousand
    let%test_unit _ = test Current.million
    let%test_unit _ = test Current.billion
    let%test_unit _ = test Current.trillion

    let ( / ) = Current.( / )
    let ( * ) = Current.( * )

    (* Test for all Over_10^i *)
    let%test_unit _ = test (Current.one / Current.ten)
    let%test_unit _ = test (Current.one / Current.hundred)
    let%test_unit _ = test (Current.one / Current.thousand)
    let%test_unit _ = test (Current.one / (Current.thousand * Current.ten))
    let%test_unit _ = test (Current.one / (Current.thousand * Current.hundred))
    let%test_unit _ = test (Current.one / Current.million)
    let%test_unit _ = test (Current.one / (Current.million * Current.ten))
    let%test_unit _ = test (Current.one / (Current.million * Current.hundred))
    let%test_unit _ = test (Current.one / Current.billion)
    let%test_unit _ = test (Current.one / (Current.billion * Current.ten))
    let%test_unit _ = test (Current.one / (Current.billion * Current.hundred))
    let%test_unit _ = test (Current.one / Current.trillion)
    (* Test for Over_int *)
    let%test_unit _ = test (Current.of_int 2 / Current.of_int 3)

    include struct
      let mul_2exp t x = of_zarith_bignum (Zarith.Q.mul_2exp (to_zarith_bignum t) x)

      (* Test for overflow  : 2^62 / 25 would be Over_100(2^64) and should overflow,
         and fallback on Other(2^62, 25) *)
      let%test_unit _ = test (mul_2exp Current.one 62 / Current.of_int 25)
      let%test_unit _ = test (mul_2exp (Current.of_int (-1)) 62 / Current.of_int 25)
      (* This test tests for overflow in the numerator *)
      let%test_unit _ = test (mul_2exp Current.one 65 / Current.of_int 25)
      (* This test tests for overflow in the denominator *)
      let%test_unit _ = test (Current.one / mul_2exp Current.one 65)
    end

    (* Test for division by zero cases *)
    let%test_unit _ = test nan
    let%test_unit _ = test infinity
    let%test_unit _ = test neg_infinity

    let numbers =
      [ "-100.00000000"
      ; "100.00000000"
      ; "0.00000000"
      ; "-200.00000000"
      ; "200.00000000"
      ; "-300.00000000"
      ; "300.00000000"
      ; "-400.00000000"
      ; "-1000.00000000"
      ; "1000.00000000"
      ; "-1.00000000"
      ; "400.00000000"
      ; "-500.00000000"
      ; "1.00000000"
      ; "500.00000000"
      ; "-600.00000000"
      ; "-2000.00000000"
      ; "2.00000000"
      ; "-2.00000000"
      ; "600.00000000"
      ; "0.20720000"
      ; "-0.20227524"
      ; "0.18800000"
      ; "0.16550000"
      ; "0.15950000"
      ; "0.13000000"
      ; "0.12950000"
      ; "0.11950000"
      ; "-0.07232871"
      ; "0.05950000"
      ; "-0.05424653"
      ; "0.04600437"
      ; "0.04600000"
      ; "0.04050000"
      ; "-0.03616435"
      ; "0.03550391"
      ; "0.03550000"
      ; "0.02000000"
      ; "0.01950000"
      ; "0.01050000"
      ; "-316673.67291835"
      ; "217240000000.0"
      ; "-217240000000.0"
      ; "3423.123456789"
      ; "-3423.1234567891"
      ]
    ;;

    let%test_unit _ =
      List.iter numbers ~f:(fun s -> test (For_testing.of_string_internal s))
    ;;

    let bin_io_tests (module M : Binable.S with type t = t) =
      let max_int31 = 0x3fff_ffffL in
      let min_int31 = Int64.( - ) 0x3fff_ffffL 0x7fff_ffffL in
      let all =
        [ "0"
        ; "1"
        ; "-1"
        ; "000100000001"
        ; "0001000000.1"
        ; "000100000.01"
        ; "00010000.001"
        ; "0001000.0001"
        ; "000100.00001"
        ; "00010.000001"
        ; "0001.0000001"
        ; "000.10000001"
        ; "00.010000001"
        ; "0.0010000001"
        ; "10000000000000"
        ; "-10000000000000"
        ; "12345678901234567.12345678901234567"
        ; Int64.to_string 0x00ff_ffff_ffffL (* does not fit in 32bit *)
        ; Int64.to_string max_int31
        ; Int64.to_string min_int31
        ; Int64.to_string Int64.(max_int31 + one) (* does not fit on 31bit integer *)
        ; Int64.to_string Int64.(min_int31 - one) (* does not fit on 31bit integer *)
        ]
      in
      let pos = 0 in
      List.iter all ~f:(fun t ->
        let t = For_testing.of_string_internal t in
        let size_of_t = M.bin_size_t t in
        assert (Int.(size_of_t + pos <= Bigstring.length buf));
        let new_pos = M.bin_writer_t.Bin_prot.Type_class.write buf ~pos t in
        let pos_ref = ref pos in
        let t1 = M.bin_reader_t.Bin_prot.Type_class.read buf ~pos_ref in
        [%test_result: V1.t] t1 ~expect:t;
        [%test_result: int] !pos_ref ~expect:new_pos;
        let buf = Bigstring.To_string.sub buf ~pos ~len:(Int.( - ) new_pos pos) in
        let buf_as_bytes =
          String.to_list_rev buf
          |> List.rev_map ~f:(function
            | '0' .. '9' as c -> String.make 1 c
            | x -> sprintf "\\%03i" (Char.to_int x))
          |> String.concat
        in
        printf
          "%20s -> (%2d) %s\n"
          (Current.to_string_accurate t)
          (String.length buf)
          buf_as_bytes)
    ;;

    let%expect_test "bin_io serialization V1" =
      bin_io_tests (module V1);
      [%expect
        {|
                   0 -> ( 2) \0010
                   1 -> ( 2) \0011
                  -1 -> ( 3) \002\0451
           100000001 -> (10) \009100000001
           1000000.1 -> (12) \01110000001\04710
           100000.01 -> (13) \01210000001\047100
           10000.001 -> (14) \01310000001\0471000
           1000.0001 -> (15) \01410000001\04710000
           100.00001 -> (16) \01510000001\047100000
           10.000001 -> (17) \01610000001\0471000000
           1.0000001 -> (18) \01710000001\04710000000
          0.10000001 -> (19) \01810000001\047100000000
         0.010000001 -> (20) \01910000001\0471000000000
        0.0010000001 -> (21) \02010000001\04710000000000
      10000000000000 -> (15) \01410000000000000
     -10000000000000 -> (16) \015\04510000000000000
12345678901234567.12345678901234567 -> (54) 51234567890123456712345678901234567\047100000000000000000
       1099511627775 -> (14) \0131099511627775
          1073741823 -> (11) \0101073741823
         -1073741824 -> (12) \011\0451073741824
          1073741824 -> (11) \0101073741824
         -1073741825 -> (12) \011\0451073741825 |}]
    ;;

    (* Note that the V2 serialization is architecture dependent *)

    let%expect_test ("bin_io serialization V2 (64bits)"[@tags "64-bits-only"]) =
      bin_io_tests (module V2);
      [%expect
        {|
                   0 -> ( 1) \000
                   1 -> ( 2) \001\001
                  -1 -> ( 3) \001\255\255
           100000001 -> ( 6) \001\253\001\225\245\005
           1000000.1 -> ( 6) \002\253\129\150\152\000
           100000.01 -> ( 6) \003\253\129\150\152\000
           10000.001 -> ( 6) \004\253\129\150\152\000
           1000.0001 -> ( 6) \005\253\129\150\152\000
           100.00001 -> ( 6) \006\253\129\150\152\000
           10.000001 -> ( 6) \007\253\129\150\152\000
           1.0000001 -> ( 6) \008\253\129\150\152\000
          0.10000001 -> ( 6) \009\253\129\150\152\000
         0.010000001 -> (11) \010\253\129\150\152\000\253\000\202\154\059
        0.0010000001 -> (15) \010\253\129\150\152\000\252\000\228\011\084\002\000\000\000
      10000000000000 -> (10) \001\252\000\160\114\078\024\009\000\000
     -10000000000000 -> (10) \001\252\000\096\141\177\231\246\255\255
12345678901234567.12345678901234567 -> (55) \01151234567890123456712345678901234567\047100000000000000000
       1099511627775 -> (10) \001\252\255\255\255\255\255\000\000\000
          1073741823 -> ( 6) \001\253\255\255\255\063
         -1073741824 -> ( 6) \001\253\000\000\000\192
          1073741824 -> ( 6) \001\253\000\000\000\064
         -1073741825 -> ( 6) \001\253\255\255\255\191 |}]
    ;;

    let%expect_test ("bin_io serialization V2 (javascript)"[@tags "js-only"]) =
      bin_io_tests (module V2);
      [%expect
        {|
                   0 -> ( 1) \000
                   1 -> ( 2) \001\001
                  -1 -> ( 3) \001\255\255
           100000001 -> ( 6) \001\253\001\225\245\005
           1000000.1 -> ( 6) \002\253\129\150\152\000
           100000.01 -> ( 6) \003\253\129\150\152\000
           10000.001 -> ( 6) \004\253\129\150\152\000
           1000.0001 -> ( 6) \005\253\129\150\152\000
           100.00001 -> ( 6) \006\253\129\150\152\000
           10.000001 -> ( 6) \007\253\129\150\152\000
           1.0000001 -> ( 6) \008\253\129\150\152\000
          0.10000001 -> ( 6) \009\253\129\150\152\000
         0.010000001 -> (11) \010\253\129\150\152\000\253\000\202\154\059
        0.0010000001 -> (22) \011\02010000001\04710000000000
      10000000000000 -> (16) \011\01410000000000000
     -10000000000000 -> (17) \011\015\04510000000000000
12345678901234567.12345678901234567 -> (55) \01151234567890123456712345678901234567\047100000000000000000
       1099511627775 -> (15) \011\0131099511627775
          1073741823 -> ( 6) \001\253\255\255\255\063
         -1073741824 -> ( 6) \001\253\000\000\000\192
          1073741824 -> ( 6) \001\253\000\000\000\064
         -1073741825 -> ( 6) \001\253\255\255\255\191 |}]
    ;;

    let%expect_test "bin_io de-serialization V2" =
      (* Some bignums will have two bin_io representation depending on where their
         were serialized.  Make sure we're able to parse things back regardless of the
         architecture. *)
      let all =
        [ ( "0.0010000001"
          , [ "\011\02010000001\04710000000000"
            ; "\010\253\129\150\152\000\252\000\228\011\084\002\000\000\000"
            ] )
        ; ( "10000000000000"
          , [ "\011\01410000000000000"; "\001\252\000\160\114\078\024\009\000\000" ] )
        ; ( "-10000000000000"
          , [ "\011\015\04510000000000000"; "\001\252\000\096\141\177\231\246\255\255" ]
          )
        ; ( "1099511627775"
          , [ "\011\0131099511627775"; "\001\252\255\255\255\255\255\000\000\000" ] )
        ]
      in
      let module M = V2 in
      List.iter all ~f:(fun (t, l) ->
        let t = For_testing.of_string_internal t in
        List.iter l ~f:(fun bin ->
          let buf = Bigstring.of_string bin in
          let pos_ref = ref 0 in
          let t1 = M.bin_reader_t.Bin_prot.Type_class.read buf ~pos_ref in
          [%test_result: V1.t] t1 ~expect:t))
    ;;

    let%expect_test "bin_io serialization V3" =
      bin_io_tests (module V3);
      [%expect
        {|
                   0 -> ( 1) \000
                   1 -> ( 2) \001\001
                  -1 -> ( 3) \001\255\255
           100000001 -> ( 6) \001\253\001\225\245\005
           1000000.1 -> ( 6) \002\253\129\150\152\000
           100000.01 -> ( 6) \003\253\129\150\152\000
           10000.001 -> ( 6) \004\253\129\150\152\000
           1000.0001 -> ( 6) \005\253\129\150\152\000
           100.00001 -> ( 6) \006\253\129\150\152\000
           10.000001 -> ( 6) \007\253\129\150\152\000
           1.0000001 -> ( 6) \008\253\129\150\152\000
          0.10000001 -> ( 6) \009\253\129\150\152\000
         0.010000001 -> (11) \010\253\129\150\152\000\253\000\202\154\059
        0.0010000001 -> (15) \010\253\129\150\152\000\252\000\228\011\084\002\000\000\000
      10000000000000 -> (10) \001\252\000\160\114\078\024\009\000\000
     -10000000000000 -> (10) \001\252\000\096\141\177\231\246\255\255
12345678901234567.12345678901234567 -> (25) \011\028\135\0751\031\227\252\113\2212\151\255\111\222\060\016\000\000\138\093\120\069\099\001
       1099511627775 -> (10) \001\252\255\255\255\255\255\000\000\000
          1073741823 -> ( 6) \001\253\255\255\255\063
         -1073741824 -> ( 6) \001\253\000\000\000\192
          1073741824 -> ( 6) \001\253\000\000\000\064
         -1073741825 -> ( 6) \001\253\255\255\255\191 |}]
    ;;

    let%expect_test _ =
      print_endline [%bin_digest: V1.t];
      [%expect {| d9a8da25d5656b016fb4dbdc2e4197fb |}];
      print_endline [%bin_digest: V2.t];
      [%expect {| 0a6a4507059ec4c575f2b3e75ac65c1f |}];
      print_endline [%bin_digest: V3.t];
      [%expect {| f358e9c3caca8a9589275ba8d8349ae8 |}]
    ;;
  end)
;;

let%test _ = Int.equal (to_int_exn (of_int Int.max_value)) Int.max_value
let%test _ = Int.equal (to_int_exn (of_int Int.min_value)) Int.min_value

let%test _ =
  try
    let (_ : int) = to_int_exn (of_int Int.max_value + one) in
    false
  with
  | Zarith.Z.Overflow -> true
;;

let%test _ =
  try
    let (_ : int) = to_int_exn (of_int Int.min_value - one) in
    false
  with
  | Zarith.Z.Overflow -> true
;;

let%test _ =
  let t = t_of_sexp (Sexp.of_string "(26.710790545 + 9999/100000000000000)") in
  let low_bound = of_string_internal "26.710790545" in
  let high_bound = of_string_internal "26.710790546" in
  t > low_bound && t < high_bound
;;

let%test _ = inverse one = one
let%test _ = inverse (neg one) = neg one
let%test _ = inverse ten = of_string ".1"
let%test _ = inverse zero = infinity (* This is specifically claimed in the mli *)
let%test _ = inverse infinity = zero
let%test _ = inverse ten = of_string_internal ".1"
let%test _ = ten ** 0 = one
let%test _ = ten ** 1 = ten
let%test _ = ten ** 2 = hundred
let%test _ = ten ** 3 = thousand
let%test _ = ten ** 6 = million
let%test _ = ten ** 9 = billion
let%test _ = ten ** 12 = trillion
let%test _ = ten ** -2 = of_string_internal "0.01"
let%test _ = one ** Int.min_value = one

let%test _ =
  of_string_internal "2" ** 1000
  = of_string_internal
      ("107150860718626732094842504906000181056140481170553360744375038837035105112493612249"
       ^ "319837881569585812759467291755314682518714528569231404359845775746985748039345677748"
       ^ "242309854210746050623711418779541821530464749835819412673987675591655439460770629145"
       ^ "71196477686542167660429831652624386837205668069376")
;;

let%test_module "round" =
  (module struct
    let x = of_string "1.23456789"
    let neg_x = neg x

    let%test _ = round x = of_string "1"
    let%test _ = round ~to_multiple_of:tenth x = of_string "1.2"
    let%test _ = round ~to_multiple_of:hundredth x = of_string "1.23"
    let%test _ = round ~to_multiple_of:thousandth x = of_string "1.235"
    let%test _ = round ~to_multiple_of:millionth x = of_string "1.234568"
    let%test _ = round neg_x = of_string "-1"
    let%test _ = round ~to_multiple_of:tenth neg_x = of_string "-1.2"
    let%test _ = round ~to_multiple_of:hundredth neg_x = of_string "-1.23"
    let%test _ = round ~to_multiple_of:thousandth neg_x = of_string "-1.235"
    let%test _ = round ~to_multiple_of:millionth neg_x = of_string "-1.234568"
    let%test _ = round_decimal ~dir:`Nearest ~digits:0 x = of_string "1"
    let%test _ = round_decimal ~dir:`Nearest ~digits:1 x = of_string "1.2"
    let%test _ = round_decimal ~dir:`Nearest ~digits:2 x = of_string "1.23"
    let%test _ = round_decimal ~dir:`Nearest ~digits:3 x = of_string "1.235"
    let%test _ = round_decimal ~dir:`Nearest ~digits:4 x = of_string "1.2346"
    let%test _ = round_decimal ~dir:`Nearest ~digits:0 neg_x = of_string "-1"
    let%test _ = round_decimal ~dir:`Nearest ~digits:1 neg_x = of_string "-1.2"
    let%test _ = round_decimal ~dir:`Nearest ~digits:2 neg_x = of_string "-1.23"
    let%test _ = round_decimal ~dir:`Nearest ~digits:3 neg_x = of_string "-1.235"
    let%test _ = round_decimal ~dir:`Nearest ~digits:4 neg_x = of_string "-1.2346"
    let%test _ = round_decimal ~dir:`Up ~digits:0 x = of_string "2"
    let%test _ = round_decimal ~dir:`Up ~digits:1 x = of_string "1.3"
    let%test _ = round_decimal ~dir:`Up ~digits:2 x = of_string "1.24"
    let%test _ = round_decimal ~dir:`Up ~digits:3 x = of_string "1.235"
    let%test _ = round_decimal ~dir:`Up ~digits:4 x = of_string "1.2346"
    let%test _ = round_decimal ~dir:`Up ~digits:0 neg_x = of_string "-1"
    let%test _ = round_decimal ~dir:`Up ~digits:1 neg_x = of_string "-1.2"
    let%test _ = round_decimal ~dir:`Up ~digits:2 neg_x = of_string "-1.23"
    let%test _ = round_decimal ~dir:`Up ~digits:3 neg_x = of_string "-1.234"
    let%test _ = round_decimal ~dir:`Up ~digits:4 neg_x = of_string "-1.2345"
    let%test _ = round_decimal ~dir:`Down ~digits:0 x = of_string "1"
    let%test _ = round_decimal ~dir:`Down ~digits:1 x = of_string "1.2"
    let%test _ = round_decimal ~dir:`Down ~digits:2 x = of_string "1.23"
    let%test _ = round_decimal ~dir:`Down ~digits:3 x = of_string "1.234"
    let%test _ = round_decimal ~dir:`Down ~digits:4 x = of_string "1.2345"
    let%test _ = round_decimal ~dir:`Down ~digits:0 neg_x = of_string "-2"
    let%test _ = round_decimal ~dir:`Down ~digits:1 neg_x = of_string "-1.3"
    let%test _ = round_decimal ~dir:`Down ~digits:2 neg_x = of_string "-1.24"
    let%test _ = round_decimal ~dir:`Down ~digits:3 neg_x = of_string "-1.235"
    let%test _ = round_decimal ~dir:`Down ~digits:4 neg_x = of_string "-1.2346"
    let%test _ = round_decimal ~dir:`Zero ~digits:0 x = of_string "1"
    let%test _ = round_decimal ~dir:`Zero ~digits:1 x = of_string "1.2"
    let%test _ = round_decimal ~dir:`Zero ~digits:2 x = of_string "1.23"
    let%test _ = round_decimal ~dir:`Zero ~digits:3 x = of_string "1.234"
    let%test _ = round_decimal ~dir:`Zero ~digits:4 x = of_string "1.2345"
    let%test _ = round_decimal ~dir:`Zero ~digits:0 neg_x = of_string "-1"
    let%test _ = round_decimal ~dir:`Zero ~digits:1 neg_x = of_string "-1.2"
    let%test _ = round_decimal ~dir:`Zero ~digits:2 neg_x = of_string "-1.23"
    let%test _ = round_decimal ~dir:`Zero ~digits:3 neg_x = of_string "-1.234"
    let%test _ = round_decimal ~dir:`Zero ~digits:4 neg_x = of_string "-1.2345"

    let%test _ =
      try
        ignore (round ~to_multiple_of:zero one : t);
        false
      with
      | _ -> true
    ;;

    let%test _ = Option.is_none (iround ~to_multiple_of:0 one)

    let%test _ =
      try
        ignore (iround_exn ~to_multiple_of:0 one : int);
        false
      with
      | _ -> true
    ;;

    let dir_to_string = function
      | `Up -> "up"
      | `Down -> "down"
      | `Nearest -> "nearest"
      | `Zero -> "zero"
    ;;

    let as_float f =
      List.iter [ `Up; `Down; `Nearest; `Zero ] ~f:(fun dir ->
        [%test_result: float]
          ~message:(dir_to_string dir)
          ~expect:(Float.round ~dir f)
          (to_float (round ~dir (of_float_dyadic f))))
    ;;

    let%test_unit _ =
      List.iter [ 0.; 0.5; 99.5; 99.99; 1_000. ] ~f:(fun f ->
        as_float f;
        as_float (Float.neg f))
    ;;

    let%test_module "iround" =
      (module struct
        let as_int ~to_multiple_of i =
          List.iter [ `Up; `Down; `Nearest; `Zero ] ~f:(fun dir ->
            [%test_result: int]
              ~message:(dir_to_string dir)
              ~expect:(Int.round ~dir ~to_multiple_of i)
              (iround_exn ~dir ~to_multiple_of (of_int i)))
        ;;

        let%test_unit _ =
          List.iter [ 1; 327; 1_000_012 ] ~f:(fun to_multiple_of ->
            List.iter [ 0; 1; 3315; 98_765_432 ] ~f:(fun i ->
              as_int ~to_multiple_of i;
              as_int ~to_multiple_of (Int.neg i)))
        ;;

        let%test_unit _ = as_int ~to_multiple_of:1 Int.max_value
        let%test_unit _ = as_int ~to_multiple_of:1 Int.min_value

        let overflows t =
          [%test_pred: int option] Option.is_none (iround t);
          try
            ignore (iround_exn t : int);
            false
          with
          | _ -> true
        ;;

        let%test _ = overflows (of_int Int.max_value + one)
        let%test _ = overflows (of_int Int.min_value - one)
      end)
    ;;
  end)
;;

let%test "tag/binable constructors in sync v2" =
  List.for_all2_exn
    Bignum.Stable.V2.For_testing.tag_variants
    Bignum.Stable.V2.For_testing.bin_rep_variants
    ~f:(fun (tag_name, _) (bin_name, _) -> String.equal tag_name bin_name)
;;

let%test "tag/binable constructors in sync v3" =
  List.for_all2_exn
    Bignum.Stable.V3.For_testing.tag_variants
    Bignum.Stable.V3.For_testing.bin_rep_variants
    ~f:(fun (tag_name, _) (bin_name, _) -> String.equal tag_name bin_name)
;;
