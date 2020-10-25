(**************************************************************************)
(*                                                                        *)
(*                                FADBADml                                *)
(*                                                                        *)
(*           OCaml port by François Bidet and Ismail Bennani              *)
(*     Based on FADBAD++, written by Ole Stauning and Claus Bendtsen      *)
(*                                                                        *)
(*                          Copyright 2019-2020                           *)
(*                                                                        *)
(*   This file is distributed under the terms of the CeCILL-C license.    *)
(*                                                                        *)
(**************************************************************************)

open Fadbad

(* utils *)

let eq_float a b epsilon =
  let absA = abs_float a in
  let absB = abs_float b in
  let diff = abs_float (a -. b) in

  if a = b then
    (* shortcut, handles infinities *)
    true
  else if Float.is_nan a then Float.is_nan b
  else if Float.is_nan b then Float.is_nan a
  else if a = 0. || b = 0. || (absA +. absB < epsilon_float) then
    (* a or b is zero or both are extremely close to it
       relative error is less meaningful here *)
    diff < (epsilon *. epsilon_float)
  else
    diff /. (min (absA +. absB) max_float) < epsilon

let eq_float a b = eq_float a b 1e-10

let eq_float_tuple (a1,a2) (b1,b2) = (eq_float a1 b1) && (eq_float a2 b2)

let string_of_float_tuple (f1, f2) = Printf.sprintf "(%f,%f)" f1 f2

let check_results results =
  Array.for_all (fun (_, res) ->
      match res.QCheck.TestResult.state with
      | Success -> true
      | Error _ | Failed _ | Failed_other _ -> false
    ) results

let for_all2 (f : 'a -> 'b -> bool) (a1 : 'a array) (a2 : 'b array) : bool =
  Array.for_all (fun x -> x) (Array.map2 f a1 a2)

let string_of_float_tuple (f1,f2) = Printf.sprintf "(%f,%f)" f1 f2

(* main test functions *)

module type Compute =
sig
  type unary
  type binary
  type value

  val uname : unary -> string
  val udesc : unary -> string
  val uarbitrary : unary -> float QCheck.arbitrary
  val ufad : unary -> (value -> value)
  val udfdx : unary -> (int -> float -> float)

  val bname : binary -> string
  val bdesc : binary -> string
  val barbitrary : binary -> (float * float) QCheck.arbitrary
  val bfad : binary -> (value -> value -> value)
  val bdfdx : binary -> (int -> float -> float -> float)
  val bdfdy : binary -> (int -> float -> float -> float)

  val compute_unary : unary -> float -> (float array) * (float array)
  val compute_binary :
    binary -> float * float -> ((float * float) array) * ((float * float) array)

  val utests : unary array
  val btests : binary array
end

module MakeTestCases(Op : Fadbad.OpS) =
struct
  module TestCases = Test_cases.Make(Op)

  type unary = TestCases.unary_test
  type binary = TestCases.binary_test
  type value = Op.t

  let uname u = u.TestCases.uname
  let udesc u = u.TestCases.udesc
  let uarbitrary u = u.TestCases.uarbitrary
  let ufad u = u.TestCases.ufad
  let udfdx u = u.TestCases.udfdx

  let bname b = b.TestCases.bname
  let bdesc b = b.TestCases.bdesc
  let barbitrary b = b.TestCases.barbitrary
  let bfad b = b.TestCases.bfad
  let bdfdx b = b.TestCases.bdfdx
  let bdfdy b = b.TestCases.bdfdy

  let utests = TestCases.unary
  let btests = TestCases.binary
end

module ComputeFAD =
struct
  module Op = F(OpFloat)
  include MakeTestCases(Op)

  let compute_unary t v =
    let x = Op.make v in
    Op.diff x 0 1;
    let v_fad = (ufad t) x in
    (* value of f *)
    let vf = Op.get v_fad in
    let actual_vf = (udfdx t) 0 v in
    (* derivative of f *)
    let dfdx_fad = Op.d v_fad 0 in
    let actual_dfdx = (udfdx t) 1 v in
    [|vf; dfdx_fad|], [|actual_vf; actual_dfdx|]

  let compute_binary t (v1, v2) =
    let x = Op.make v1 in
    Op.diff x 0 2;
    let y = Op.make v2 in
    Op.diff y 1 2;
    let v_fad = (bfad t) x y in
    (* value of f *)
    let vf = Op.get v_fad in
    let actual_vf = (bdfdx t) 0 v1 v2 in
    (* derivative of f *)
    let dfdx_fad = Op.d v_fad 0 in
    let actual_dfdx = (bdfdx t) 1 v1 v2 in
    let dfdy_fad = Op.d v_fad 1 in
    let actual_dfdy = (bdfdy t) 1 v1 v2 in
    [|(vf,vf); (dfdx_fad,dfdy_fad)|],
    [|(actual_vf,actual_vf); (actual_dfdx,actual_dfdy)|]
end

module ComputeBAD =
struct
  module Op = B(OpFloat)
  include MakeTestCases(Op)

  let compute_unary t v =
    let x = Op.make v in
    let v_fad = (ufad t) x in
    Op.diff v_fad 0 1;
    Op.compute v_fad;
    (* value of f *)
    let vf = Op.get v_fad in
    let actual_vf = (udfdx t) 0 v in
    (* derivative of f *)
    let dfdx_fad = Op.d x 0 in
    let actual_dfdx = (udfdx t) 1 v in
    [|vf; dfdx_fad|], [|actual_vf; actual_dfdx|]

  let compute_binary t (v1, v2) =
    let x = Op.make v1 in
    let y = Op.make v2 in
    let v_fad = (bfad t) x y in
    Op.diff v_fad 0 1;
    Op.compute v_fad;
    (* value of f *)
    let vf = Op.get v_fad in
    let actual_vf = (bdfdx t) 0 v1 v2 in
    (* derivative of f *)
    let dfdx_fad = Op.d x 0 in
    let actual_dfdx = (bdfdx t) 1 v1 v2 in
    let dfdy_fad = Op.d y 0 in
    let actual_dfdy = (bdfdy t) 1 v1 v2 in
    [|(vf,vf); (dfdx_fad,dfdy_fad)|],
    [|(actual_vf,actual_vf); (actual_dfdx,actual_dfdy)|]
end

module ComputeTAD =
struct
  module Op = T(OpFloat)
  include MakeTestCases(Op)

  let max_order = 4

  let compute_unary t v =
    let x = Op.make v in
    let v_fad = (ufad t) x in
    (* values of d^if/dx^i in TAD *)
    Op.set x 1 (OpFloat.one ());
    ignore (Op.eval v_fad max_order);
    let tad_der = Op.get_derivatives v_fad in
    (* expected values of d^if/dx^i *)
    let expected_der = Array.init (max_order + 1) (fun i -> (udfdx t) i v) in
    tad_der, expected_der

  let compute_binary t (v1, v2) =
    let x = Op.make v1 in
    let y = Op.make v2 in
    let v_fad = (bfad t) x y in
    (* value of f *)
    Op.set x 1 (OpFloat.one ());
    ignore (Op.eval v_fad max_order);
    let tad_derx = Op.get_derivatives v_fad in
    Op.reset v_fad;
    Op.set y 1 (OpFloat.one ());
    ignore (Op.eval v_fad max_order);
    let tad_dery = Op.get_derivatives v_fad in
    try
      let tad_der = Array.map2 (fun x y -> (x,y)) tad_derx tad_dery in
      (* derivative of f *)
      let expected_der = Array.init (max_order + 1)
        (fun i -> (bdfdx t) i v1 v2, (bdfdy t) i v1 v2) in
      tad_der, expected_der
    with Invalid_argument s ->
      Printf.eprintf
        "Fatal error in compute_binary: exception Invalid_argument \"%s\"
          a1 = [%s]
          a2 = [%s]\n"
        s
        (String.concat "; "
           (Array.to_list (Array.map string_of_float tad_derx)))
        (String.concat "; "
           (Array.to_list (Array.map string_of_float tad_dery)));
      exit 1
end

module Test(Compute : Compute) =
struct
  let compare_unary t v =
    let tad_der, expected_der = Compute.compute_unary t v in
    try
      for_all2 eq_float tad_der expected_der
    with Invalid_argument s ->
      Printf.eprintf
        "Fatal error in compare_unary: %s\na1 = [%s]\na2 = [%s]\n"
        s
        (String.concat "; "
           (Array.to_list (Array.map string_of_float tad_der)))
        (String.concat "; "
           (Array.to_list (Array.map string_of_float expected_der)));
      false

  let compare_binary t (v1, v2) =
    let tad_der, expected_der = Compute.compute_binary t (v1, v2) in
    try
      for_all2 eq_float_tuple tad_der expected_der
    with Invalid_argument s ->
      Printf.eprintf
        "Fatal error in compare_binary: exception Invalid_argument \"%s\"
          a1 = [%s]
          a2 = [%s]\n"
        s
        (String.concat "; "
           (Array.to_list (Array.map string_of_float_tuple tad_der)))
        (String.concat "; "
           (Array.to_list (Array.map string_of_float_tuple expected_der)));
      false

  let test_unary ?count:(count=100) f =
    let cell = QCheck.(Test.make_cell ~name:(Compute.uname f)
                         ~count:count (Compute.uarbitrary f)
                         (compare_unary f)) in
    cell, QCheck.Test.check_cell cell

  let test_binary ?count:(count=100) f =
    let cell = QCheck.(Test.make_cell ~name:(Compute.bname f)
                         ~count:count (Compute.barbitrary f)
                         (compare_binary f)) in
    cell, QCheck.Test.check_cell cell

  let show_test_unary t v =
    let tad_der, expected_der = Compute.compute_unary t v in
    Printf.printf "\tDESC : %s\n" (Compute.udesc t);
    Printf.printf "\tx = %f\n" v;
    Printf.printf "\tdfdx : expected [%s], got [%s] ... %s\n"
      (String.concat "; " (Array.to_list (Array.map string_of_float expected_der)))
      (String.concat "; " (Array.to_list (Array.map string_of_float tad_der)))
      (string_of_bool (false))
      (* (string_of_bool (for_all2 eq_float tad_der expected_der)) *)

  let show_test_binary t (v1, v2) =
    let tad_der, expected_der = Compute.compute_binary t (v1, v2) in
    Printf.printf "\tDESC : %s\n" (Compute.bdesc t);
    Printf.printf "\tx, y = %e, %e\n" v1 v2;
    Printf.printf "\tdfdx, dfdy : expected [%s], got [%s] ... %s\n"
      (String.concat "; " (Array.to_list (Array.map string_of_float_tuple expected_der)))
      (String.concat "; " (Array.to_list (Array.map string_of_float_tuple tad_der)))
      (string_of_bool (false))
      (* (string_of_bool (for_all2 eq_float_tuple tad_der expected_der)) *)

  let show_result show_test test (cell, res) =
    begin match res.QCheck.TestResult.state with
    | Success ->
      Printf.printf "%s\t:\tOK\n" (QCheck.Test.get_name cell)
    | Failed { instances = instance :: _ }
    | Error { instance; _ } ->
      let name = QCheck.Test.get_name cell in
      let arb = QCheck.Test.get_arbitrary cell in
      let c_ex = instance.instance in
      Printf.printf "%s\t:\tNOT OK\n" name;
      Printf.printf "\tCounter-example: %s\n"
        (QCheck.Test.print_c_ex arb instance);
      show_test test c_ex
    | Failed { instances = [] } ->
      Printf.printf "%s\t:\tNOT OK\n" (QCheck.Test.get_name cell);
    | Failed_other { msg } ->
      Printf.printf "%s\t:\tNOT OK\n" (QCheck.Test.get_name cell);
      Printf.printf "\tMessage: %s\n" msg
    end; flush stdout

  let test_uarr ?count:(count=100) tests =
    let results =
      Array.map (fun t -> test_unary ~count:count t)
        tests in
    let ok = check_results results in
    Array.iter2 (show_result show_test_unary) tests results;
    print_endline (if ok then "OK" else "NOT OK");
    ok

  let test_barr ?count:(count=100) tests =
    let results =
      Array.map (fun t -> test_binary ~count:count t)
        tests in
    let ok = check_results results in
    Array.iter2 (show_result show_test_binary) tests results;
    print_endline (if ok then "OK" else "NOT OK");
    ok

  let test_all () =
    Random.self_init ();

    print_endline "---- UNARY FUNCTIONS";
    ignore (test_uarr ~count:1000 Compute.utests);

    print_endline "\n---- BINARY FUNCTIONS";
    ignore (test_barr ~count:1000 Compute.btests);

    ()
end

module TestFAD = Test(ComputeFAD)
module TestBAD = Test(ComputeBAD)
module TestTAD = Test(ComputeTAD)

let _ =
  print_endline "========================";
  print_endline "        TEST FAD";
  print_endline "========================";
  TestFAD.test_all ();

  print_newline ();

  print_endline "========================";
  print_endline "        TEST BAD";
  print_endline "========================";
  TestBAD.test_all ();

  print_newline ();

  print_endline "========================";
  print_endline "        TEST TAD";
  print_endline "========================";
  TestTAD.test_all ();
  ()
