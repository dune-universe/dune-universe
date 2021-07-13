(* The inline tests were moved here to avoid depending on ppx outside tests.  *)
(* The goal of these tests are to catch errors in the function and constant   *)
(* mappings. They do not try to catch functional errors in Bitwuzla.          *)

open Bitwuzla_c
open Format

let timeout n t = if Sys.time () -. t -. n  > 0. then 1 else 0

let bool c f = c (fun t -> print_int @@ if f t then 1 else 0)

let int c f = c (fun t -> print_int @@ f t)

let string c f = c (fun t -> print_string @@ f t)

let int_dump i _ pp = pp_print_int pp i

let array_dump pp a f ppf = Array.iter
    (fun t -> pp t f ppf; Format.print_space ()) a

let dump pp c f = c (fun t -> pp (f t) `Smt2 std_formatter)


let with_t_naked f =
  let t = create () in
  let r = f t in
  delete t;
  r

let with_t f =
  with_t_naked (fun t ->
      set_option t Incremental 1;
      mk_assert t @@ mk_true t;
      f t)

let with_sat_formula f =
  with_t (fun t ->
      set_option t Produce_models 1;
      let bv1 = mk_bool_sort t in
      let a = mk_const t bv1 "a" in
      mk_assert t @@ mk_term2 t Equal a @@ mk_true t;
      f (t, a))

let with_unsat_formula mk f =
  with_t (fun t ->
      set_option t Produce_unsat_cores 1;
      let bv1 = mk_bool_sort t in
      let a = mk_const t bv1 "a" in
      let e = mk_term2 t Equal a @@ mk_term1 t Not a in
      mk t e;
      f (t, e))

let with_sat_push_unsat_push2_formula f =
  with_t (fun t ->
      mk_assert t @@ mk_true t;
      push t 1;
      mk_assert t @@ mk_false t;
      push t 2;
      f t)

let with_hard_formula f =
  with_t_naked (fun t ->
      let bv64 = mk_bv_sort t 64 in
      let bv128 = mk_bv_sort t 128 in
      let a64 = mk_const t bv64 "a" in
      let b64 = mk_const t bv64 "b" in
      let a128 = mk_term1_indexed1 t Bv_zero_extend a64 64 in
      let b128 = mk_term1_indexed1 t Bv_zero_extend b64 64 in
      let ab128 = mk_term2 t Bv_mul a128 b128 in
      let p = mk_bv_value t bv128 "87e03acc9f5050086f083d2d5d6b9d47" Hex in
      let one128 = mk_bv_one t bv128 in
      mk_assert t @@ mk_term2 t Distinct a128 one128;
      mk_assert t @@ mk_term2 t Distinct b128 one128;
      mk_assert t @@ mk_term2 t Equal ab128 p;
      f t)

let with_rm_sort f =
  with_t (fun t -> f (t, mk_rm_sort t))

let with_bool_sort f =
  with_t (fun t -> f (t, mk_bool_sort t))

let with_bv8_sort f =
  with_t (fun t -> f (t, mk_bv_sort t 8))

let with_fp16_sort f =
  with_t (fun t -> f (t, mk_fp_sort t 5 11))

let with_ar32_8_sort f =
  with_t (fun t ->
      let bv8 = mk_bv_sort t 8 and bv32 = mk_bv_sort t 32 in
      f (t, mk_array_sort t bv32 bv8, bv32, bv8))

let with_fun1_1_1_sort f =
  with_bool_sort (fun (t, bv1) ->
      f (t, mk_fun_sort t [| bv1; bv1 |] bv1))

let with_sorts f =
  Array.iter (fun k -> f k)
    [| (fun f -> with_ar32_8_sort (fun (t, s, _, _) -> f (t, s)));
         with_bool_sort; with_bv8_sort; with_fp16_sort;
         with_fun1_1_1_sort; with_rm_sort |]

let with_bv_value f =
  with_bv8_sort (fun (t, s) -> f @@ mk_bv_zero t s)

let with_bv_const f =
  with_bv8_sort (fun (t, s) -> f @@ mk_const t s "a")

let with_bv_var f =
  with_bv8_sort (fun (t, s) -> f @@ mk_var t s "a")

let with_fp_value f =
  with_fp16_sort (fun (t, s) -> f @@ mk_fp_pos_zero t s)

let with_fp_const f =
  with_fp16_sort (fun (t, s) -> f @@ mk_const t s "a")

let with_ar_value f =
  with_ar32_8_sort (fun (t, ar32_8, _, bv8) ->
      f @@ mk_const_array t ar32_8 @@ mk_bv_zero t bv8)

let with_ar_const f =
  with_ar32_8_sort (fun (t, s, _, _) -> f @@ mk_const t s "a")

let with_fun_const f =
  with_fun1_1_1_sort (fun (t, s) -> f @@ mk_const t s "a")

let with_rm_value f =
  with_t (fun t -> f @@ mk_rm_value t Rtz)

let with_term1 f =
  with_bv8_sort (fun (t, s) -> f @@ mk_term1 t Bv_not @@ mk_const t s "a")

let with_term2 f =
  with_bv8_sort (fun (t, s) ->
      f @@ mk_term2 t Bv_add (mk_const t s "a") (mk_const t s "b"))

let with_term3 f =
  with_ar32_8_sort (fun (t, ar32_8, bv32, bv8) ->
      f @@ mk_term3 t Array_store (mk_const t ar32_8 "a")
        (mk_const t bv32 "b") (mk_const t bv8 "c"))

let with_term1_indexed2 f =
  with_bv8_sort (fun (t, s) ->
      f @@ mk_term1_indexed2 t Bv_extract (mk_const t s "a") 1 0)

let with_terms f =
  Array.iter f
    [| with_bv_value; with_bv_const; with_bv_var; with_fp_value;
       with_fp_const; with_ar_value; with_ar_const; with_fun_const;
       with_term1; with_term2; with_term3; with_term1_indexed2;
       with_rm_value |]

let%test "version" =
  0 <> String.length @@ with_t version

let%expect_test "copyright" =
  string with_t copyright;
  [%expect{|
    Bitwuzla is a Satisfiability Modulo Theories (SMT) Solver for bit-vectors,
    floating-points, arrays and uninterpreted functions.

    Copyright (C) 2007-2021 by its authors and contributors and their institutional
    affiliations as listed in file AUTHORS.

    MIT License

    Permission is hereby granted, free of charge, to any person obtaining a
    copy of this software and associated documentation files (the "Software"),
    to deal in the Software without restriction, including without limitation
    the rights to use, copy, modify, merge, publish, distribute, sublicense,
    and/or sell copies of the Software, and to permit persons to whom the
    Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included
    in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
    THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
    OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
    ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
    OTHER DEALINGS IN THE SOFTWARE.


    This version of Bitwuzla is linked against the following
    third party libraries. For copyright information of each
    library see the corresponding url.

      Btor2Tools - tools for the BTOR2 format
      https://https://github.com/Boolector/btor2tools

      GMP - GNU Multiple Precision Arithmetic Library
      https://gmplib.org

      CaDiCaL
      https://github.com/arminbiere/cadical

      SymFPU
      https://github.com/martin-cs/symfpu |}]

let%test "get_option" =
  with_t_naked (fun t -> get_option t Incremental = 0)

let%test "get_option" =
  with_t (fun t -> get_option t Incremental = 1)

let%expect_test "get_option_str" =
  string with_t (fun t -> get_option_str t Engine);
  [%expect {| fun |}]

let%test "terminate" =
  with_hard_formula (fun t ->
      ignore @@ set_termination_callback t (timeout 0.1) @@ Sys.time ();
      check_sat t = Unknown && terminate t)

let%test "get_termination_callback_state" =
  with_hard_formula (fun t ->
      let s = Sys.time () in
      let c = set_termination_callback t (timeout 0.1) s in
      ignore @@ check_sat t;
      get_termination_callback_state c = s)

let%expect_test "mk_array_sort" =
  dump sort_dump with_ar32_8_sort (fun (_, s, _, _) -> s);
  [%expect{| (Array (_ BitVec 32) (_ BitVec 8)) |}]

let%expect_test "mk_bool_sort" =
  dump sort_dump with_t (fun t -> mk_bool_sort t);
  [%expect{| (_ BitVec 1) |}]

let%expect_test "mk_bv_sort" =
  dump sort_dump with_bv8_sort snd;
  [%expect{| (_ BitVec 8) |}]

let%expect_test "mk_fp_sort" =
  dump sort_dump with_fp16_sort snd;
  [%expect{| (_ FloatingPoint 5 11) |}]

let%expect_test "mk_fun_sort" =
  dump sort_dump with_fun1_1_1_sort snd;
  [%expect{| ((_ BitVec 1) (_ BitVec 1)) (_ BitVec 1) |}]

let%test "mk_fun_sort" =
  try
    ignore @@ with_t (fun t ->
        let bv1 = mk_bool_sort t in
        mk_fun_sort t [||] bv1);
    false
  with Failure _ -> true

let%expect_test "mk_rm_sort" =
  dump sort_dump with_t (fun t -> mk_rm_sort t);
  [%expect{| RoundingMode |}]

let%expect_test "sort_hash" =
  with_sorts (fun f -> int f (fun (_ , s) -> sort_hash s));
  [%expect{| 512232 |}]

let%test "sort_bv_get_size" =
  with_bv8_sort (fun (_, s) -> sort_bv_get_size s) = 8

let%test "sort_fp_get_exp_size" =
  with_fp16_sort (fun (_, s) -> sort_fp_get_exp_size s) = 5

let%test "sort_fp_get_sig_size" =
  with_fp16_sort (fun (_, s) -> sort_fp_get_sig_size s) = 11

let%expect_test "sort_array_get_index" =
  dump sort_dump with_ar32_8_sort (fun (_, s, _, _) -> sort_array_get_index s);
  [%expect{| (_ BitVec 32) |}]

let%expect_test "sort_array_get_element" =
  dump sort_dump with_ar32_8_sort
    (fun (_, s, _, _) -> sort_array_get_element s);
  [%expect{| (_ BitVec 8) |}]

let%expect_test "sort_fun_get_domain_sorts" =
  dump (array_dump sort_dump)
    with_fun1_1_1_sort (fun (_ , s) -> sort_fun_get_domain_sorts s);
  [%expect{| (_ BitVec 1) (_ BitVec 1) |}]

let%expect_test "sort_fun_get_codomain" =
  dump sort_dump with_fun1_1_1_sort (fun (_, s) -> sort_fun_get_codomain s);
  [%expect{| (_ BitVec 1) |}]

let%test "sort_fun_get_arity" =
  with_fun1_1_1_sort (fun (_, s) -> sort_fun_get_arity s) = 2

let%test "sort_is_equal" =
  with_t (fun t -> mk_bool_sort t |> sort_is_equal @@ mk_bool_sort t)

let%test "sort_is_equal" =
  with_t (fun t -> not (mk_bv_sort t 32 |> sort_is_equal @@ mk_bool_sort t))

let%expect_test "sort_is_array" =
  with_sorts (fun f -> bool f (fun (_ , s) -> sort_is_array s));
  [%expect{| 100000 |}]

let%expect_test "sort_is_bv" =
  with_sorts (fun f -> bool f (fun (_ , s) -> sort_is_bv s));
  [%expect{| 011000 |}]

let%expect_test "sort_is_fp" =
  with_sorts (fun f -> bool f (fun (_ , s) -> sort_is_fp s));
  [%expect{| 000100 |}]

let%expect_test "sort_is_fun" =
  with_sorts (fun f -> bool f (fun (_ , s) -> sort_is_fun s));
  [%expect{| 100010 |}]

let%expect_test "sort_is_rm" =
  with_sorts (fun f -> bool f (fun (_ , s) -> sort_is_rm s));
  [%expect{| 000001 |}]

let%expect_test "mk_true" =
  dump term_dump with_t (fun t -> mk_true t);
  [%expect {| true |}]

let%expect_test "mk_false" =
  dump term_dump with_t (fun t -> mk_false t);
  [%expect {| false |}]

let%expect_test "mk_bv_zero" =
  dump term_dump with_bv8_sort (fun (t, s) -> mk_bv_zero t s);
  [%expect {| #b00000000 |}]

let%expect_test "mk_bv_one" =
  dump term_dump with_bv8_sort (fun (t, s) -> mk_bv_one t s);
  [%expect {| #b00000001 |}]

let%expect_test "mk_bv_ones" =
  dump term_dump with_bv8_sort (fun (t, s) -> mk_bv_ones t s);
  [%expect {| #b11111111 |}]

let%expect_test "mk_bv_min_signed" =
  dump term_dump with_bv8_sort (fun (t, s) -> mk_bv_min_signed t s);
  [%expect {| #b10000000 |}]

let%expect_test "mk_bv_max_signed" =
  dump term_dump with_bv8_sort (fun (t, s) -> mk_bv_max_signed t s);
  [%expect {| #b01111111 |}]

let%expect_test "mk_fp_pos_zero" =
  dump term_dump with_fp16_sort (fun (t, s) -> mk_fp_pos_zero t s);
  [%expect {| (fp #b0 #b00000 #b0000000000) |}]

let%expect_test "mk_fp_neg_zero" =
  dump term_dump with_fp16_sort (fun (t, s) -> mk_fp_neg_zero t s);
  [%expect {| (fp #b1 #b00000 #b0000000000) |}]

let%expect_test "mk_fp_pos_inf" =
  dump term_dump with_fp16_sort (fun (t, s) -> mk_fp_pos_inf t s);
  [%expect {| (fp #b0 #b11111 #b0000000000) |}]

let%expect_test "mk_fp_neg_inf" =
  dump term_dump with_fp16_sort (fun (t, s) -> mk_fp_neg_inf t s);
  [%expect {| (fp #b1 #b11111 #b0000000000) |}]

let%expect_test "mk_fp_nan" =
  dump term_dump with_fp16_sort (fun (t, s) -> mk_fp_nan t s);
  [%expect {| (fp #b0 #b11111 #b1000000000) |}]

let%expect_test "mk_bv_value" =
  dump term_dump with_bv8_sort (fun (t, s) -> mk_bv_value t s "42" Dec);
  [%expect {| #b00101010 |}]

let%expect_test "mk_bv_value" =
  dump term_dump with_bv8_sort (fun (t, s) -> mk_bv_value t s "2a" Hex);
  [%expect {| #b00101010 |}]

let%expect_test "mk_bv_value" =
  dump term_dump with_bv8_sort (fun (t, s) -> mk_bv_value t s "00101010" Bin);
  [%expect {| #b00101010 |}]

let%expect_test "mk_bv_value_int" =
  dump term_dump with_bv8_sort (fun (t, s) -> mk_bv_value_int t s 42);
  [%expect {| #b00101010 |}]

let%expect_test "mk_fp_value" =
  dump term_dump with_t (fun t ->
      let sign = mk_false t
      and expv = mk_bv_value t (mk_bv_sort t 5) "10010" Bin
      and sigv = mk_bv_value t (mk_bv_sort t 10) "100001100" Bin in
      mk_fp_value t sign expv sigv);
  [%expect {| (fp #b0 #b10010 #b0100001100) |}]

let%expect_test "mk_fp_value_from_real" =
  dump term_dump with_fp16_sort (fun (t, s) ->
      mk_fp_value_from_real t s (mk_rm_value t Rtz) "10.1");
  [%expect {| (fp #b0 #b10010 #b0100001100) |}]

let%expect_test "mk_fp_value_from_rational" =
  dump term_dump with_fp16_sort (fun (t, s) ->
      mk_fp_value_from_rational t s (mk_rm_value t Rtz) "101" "10");
  [%expect {| (fp #b0 #b10010 #b0100001100) |}]

let%expect_test "mk_rm_value" =
  [| Rne; Rna; Rtn; Rtp; Rtz |]
  |> Array.iter @@
  (fun x -> dump term_dump with_t (fun t -> mk_rm_value t x);
    Format.print_space ());
  [%expect {| RNE RNA RTN RTP RTZ |}]

let%expect_test "mk_term" =
  dump term_dump with_bv8_sort (fun (t, s) ->
      mk_term t Bv_and
        [| mk_const t s "a"; mk_const t s "b"; mk_const t s "c" |]);
  [%expect {| (bvand c (bvand b a)) |}]

let%expect_test "mk_term_indexed" =
  dump term_dump with_bv8_sort (fun (t, s) ->
      mk_term_indexed t Bv_repeat [| mk_bv_zero t s |] [| 4 |]);
  [%expect {| #b00000000000000000000000000000000 |}]

let%expect_test "mk_const" =
  dump term_dump with_bv8_sort (fun (t, s) -> mk_const t s "a");
  [%expect {| (declare-const a (_ BitVec 8)) |}]

let%expect_test "mk_const" =
  dump term_dump with_bv8_sort (fun (t, s) -> mk_const t s "");
  [%expect {| (declare-const v1 (_ BitVec 8)) |}]

let%expect_test "mk_const_array" =
  dump term_dump with_ar32_8_sort (fun (t, ar32_8, _, bv8) ->
      mk_const_array t ar32_8 @@ mk_bv_one t bv8);
  [%expect {| ((as const ((_ BitVec 32)) (_ BitVec 8)) #b00000001) |}]

let%expect_test "mk_var" =
  dump term_dump with_bv8_sort (fun (t, s) -> mk_var t s "a");
  [%expect {| a |}]

let%expect_test "substitute_term" =
  dump term_dump with_bv8_sort (fun (t, s) ->
      let a = mk_const t s "a" in
      substitute_term t a [| a, mk_bv_one t s |]);
  [%expect {| #b00000001 |}]

let%expect_test "substitute_terms" =
  dump (array_dump term_dump) with_bv8_sort (fun (t, s) ->
      let a = mk_const t s "a" and b = mk_const t s "b" in
      let terms = [| a; b |] in
      substitute_terms t terms [| b, mk_bv_ones t s; a, mk_bv_one t s |];
      terms);
  [%expect {| #b00000001 #b11111111 |}]

let%expect_test "term_hash" =
  with_terms (fun f -> int f (fun s -> term_hash s); Format.print_space ());
  [%expect {| 2 2 2 2 2 4 2 2 -2 4 6 3 2 |}]

let%test "term_get_kind" =
  with_term1 (fun t -> term_get_kind t = Bv_not)

let%expect_test "term_get_children" =
  dump (array_dump term_dump) with_term1 (fun t -> term_get_children t);
  [%expect {| (declare-const a (_ BitVec 8)) |}]

let%expect_test "term_get_children" =
  dump (array_dump term_dump) with_term2 (fun t -> term_get_children t);
  [%expect {|
    (declare-const b (_ BitVec 8))
     (declare-const a (_ BitVec 8)) |}]

let%expect_test "term_get_children" =
  dump (array_dump term_dump) with_term3 (fun t -> term_get_children t);
  [%expect {|
    (declare-const a (Array (_ BitVec 32) (_ BitVec 8)))

    (declare-const b (_ BitVec 32))
     (declare-const c (_ BitVec 8)) |}]

let%expect_test "term_get_indices" =
  dump (array_dump int_dump) with_term1_indexed2
          (fun t -> term_get_indices t);
  [%expect {| 1 0 |}]

let%expect_test "term_is_indexed" =
  with_terms (fun f -> bool f (fun t -> term_is_indexed t));
  [%expect {| 0000000000010 |}]

let%expect_test "term_get_sort" =
  with_terms (fun f -> dump sort_dump f (fun t -> term_get_sort t);
               Format.print_space ());
  [%expect {|
    (_ BitVec 8) (_ BitVec 8) (_ BitVec 8) (_ FloatingPoint 5 11)
    (_ FloatingPoint 5 11) (Array (_ BitVec 32) (_ BitVec 8))
    (Array (_ BitVec 32) (_ BitVec 8)) ((_ BitVec 1) (_ BitVec 1)) (_ BitVec 1)
    (_ BitVec 8) (_ BitVec 8) (Array (_ BitVec 32) (_ BitVec 8)) (_ BitVec 2)
    RoundingMode |}]

let%expect_test "term_array_get_index_sort" =
  dump sort_dump with_ar_const (fun t -> term_array_get_index_sort t);
  [%expect {| (_ BitVec 32) |}]

let%expect_test "term_array_get_element_sort" =
  dump sort_dump with_ar_const (fun t -> term_array_get_element_sort t);
  [%expect {| (_ BitVec 8) |}]

let%expect_test "term_fun_get_domain_sorts" =
  dump (array_dump sort_dump) with_fun_const
    (fun t -> term_fun_get_domain_sorts t);
  [%expect {| (_ BitVec 1) (_ BitVec 1) |}]

let%expect_test "term_fun_get_codomain_sort" =
  dump sort_dump with_fun_const (fun t -> term_fun_get_codomain_sort t);
  [%expect {| (_ BitVec 1) |}]

let%expect_test "term_bv_get_size" =
  int with_bv_const (fun t -> term_bv_get_size t);
  [%expect {| 8 |}]

let%expect_test "term_fp_get_exp_size" =
  int with_fp_const (fun t -> term_fp_get_exp_size t);
  [%expect {| 5 |}]

let%expect_test "term_fp_get_sig_size" =
  int with_fp_const (fun t -> term_fp_get_sig_size t);
  [%expect {| 11 |}]

let%expect_test "term_fun_get_arity" =
  int with_fun_const (fun t -> term_fun_get_arity t);
  [%expect {| 2 |}]

let%expect_test "term_get_symbol" =
  string with_bv_const (fun t -> term_get_symbol t);
  [%expect {| a |}]

let%test "term_get_symbol" =
  with_bv_value (fun t ->
      try ignore @@ term_get_symbol t; false with Not_found -> true)

let%expect_test "term_get_symbol" =
  string with_bv_value (fun t -> term_set_symbol t "a"; term_get_symbol t);
  [%expect {| a |}]

let%test "term_is_equal_sort" =
  with_t (fun t -> mk_true t |> term_is_equal_sort @@ mk_false t)

let%test "term_is_equal_sort" =
  with_t (fun t -> not (mk_rm_value t Rtz |> term_is_equal_sort @@ mk_true t))

let%expect_test "term_is_array" =
  with_terms (fun f -> bool f (fun t -> term_is_array t));
  [%expect {| 0000011000100 |}]

let%expect_test "term_is_const" =
  with_terms (fun f -> bool f (fun t -> term_is_const t));
  [%expect {| 0100101100000 |}]

let%expect_test "term_is_fun" =
  with_terms (fun f -> bool f (fun t -> term_is_fun t));
  [%expect {| 0000000100000 |}]

let%expect_test "term_is_var" =
  with_terms (fun f -> bool f (fun t -> term_is_var t));
  [%expect {| 0010000000000 |}]

let%test "term_is_bound_var" =
  with_bv_var (fun t -> not @@ term_is_bound_var t)

let%test "term_is_bound_var" =
  with_bv8_sort (fun (t, s) ->
      let a = mk_var t s "a" in
      let _f = mk_fun_sort t [| s |] s in
      ignore @@ mk_term2 t Lambda a a;
      term_is_bound_var a)

let%expect_test "term_is_value" =
  with_terms (fun f -> bool f (fun t -> term_is_value t));
  [%expect {| 1001000000001 |}]

let%expect_test "term_is_bv_value" =
  with_terms (fun f -> bool f (fun t -> term_is_bv_value t));
  [%expect {| 1000000000000 |}]

let%expect_test "term_is_fp_value" =
  with_terms (fun f -> bool f (fun t -> term_is_fp_value t));
  [%expect {| 0001000000000 |}]

let%expect_test "term_is_rm_value" =
  with_terms (fun f -> bool f (fun t -> term_is_rm_value t));
  [%expect {| 0000000000001 |}]

let%expect_test "term_is_bv" =
  with_terms (fun f -> bool f (fun t -> term_is_bv t));
  [%expect {| 1110000011010 |}]

let%expect_test "term_is_fp" =
  with_terms (fun f -> bool f (fun t -> term_is_fp t));
  [%expect {| 0001100000000 |}]

let%expect_test "term_is_rm" =
  with_terms (fun f -> bool f (fun t -> term_is_rm t));
  [%expect {| 0000000000001 |}]

let%test "term_is_bv_value_zero" =
  with_bv_var (fun t -> not @@ term_is_bv_value_zero t)

let%test "term_is_bv_value_zero" =
  with_bv8_sort (fun (t, s) -> term_is_bv_value_zero @@ mk_bv_zero t s)

let%test "term_is_bv_value_one" =
  with_bv_var (fun t -> not @@ term_is_bv_value_one t)

let%test "term_is_bv_value_one" =
  with_bv8_sort (fun (t, s) -> term_is_bv_value_one @@ mk_bv_one t s)

let%test "term_is_bv_value_ones" =
  with_bv_var (fun t -> not @@ term_is_bv_value_ones t)

let%test "term_is_bv_value_ones" =
  with_bv8_sort (fun (t, s) -> term_is_bv_value_ones @@ mk_bv_ones t s)

let%test "term_is_bv_value_min_signed" =
  with_bv_var (fun t -> not @@ term_is_bv_value_min_signed t)

let%test "term_is_bv_value_min_signed" =
  with_bv8_sort (fun (t, s) -> term_is_bv_value_min_signed
                  @@ mk_bv_min_signed t s)

let%test "term_is_bv_value_max_signed" =
  with_bv_var (fun t -> not @@ term_is_bv_value_max_signed t)

let%test "term_is_bv_value_max_signed" =
  with_bv8_sort (fun (t, s) -> term_is_bv_value_max_signed
                  @@ mk_bv_max_signed t s)

let%test "term_is_fp_value_pos_zero" =
  with_bv_var (fun t -> not @@ term_is_fp_value_pos_zero t)

let%test "term_is_fp_value_pos_zero" =
  with_fp16_sort (fun (t, s) -> term_is_fp_value_pos_zero
                   @@ mk_fp_pos_zero t s)

let%test "term_is_fp_value_neg_zero" =
  with_bv_var (fun t -> not @@ term_is_fp_value_neg_zero t)

let%test "term_is_fp_value_neg_zero" =
  with_fp16_sort (fun (t, s) -> term_is_fp_value_neg_zero
                   @@ mk_fp_neg_zero t s)

let%test "term_is_fp_value_pos_inf" =
  with_bv_var (fun t -> not @@ term_is_fp_value_pos_inf t)

let%test "term_is_fp_value_pos_inf" =
  with_fp16_sort (fun (t, s) -> term_is_fp_value_pos_inf
                   @@ mk_fp_pos_inf t s)

let%test "term_is_fp_value_neg_inf" =
  with_bv_var (fun t -> not @@ term_is_fp_value_neg_inf t)

let%test "term_is_fp_value_neg_inf" =
  with_fp16_sort (fun (t, s) -> term_is_fp_value_neg_inf
                   @@ mk_fp_neg_inf t s)

let%test "term_is_fp_value_nan" =
  with_bv_var (fun t -> not @@ term_is_fp_value_nan t)

let%test "term_is_fp_value_nan" =
  with_fp16_sort (fun (t, s) -> term_is_fp_value_nan @@ mk_fp_nan t s)

let%test "term_is_const_array" =
  with_bv_var (fun t -> not @@ term_is_const_array t)

let%test "term_is_const_array" =
  with_ar_value (fun t -> term_is_const_array t)

let%test "push/pop" =
  with_sat_push_unsat_push2_formula (fun t ->
      pop t 1;
      check_sat t = Unsat)

let%test "push/pop" =
  with_sat_push_unsat_push2_formula (fun t ->
      pop t 2;
      check_sat t = Unsat)

let%test "push/pop" =
  with_sat_push_unsat_push2_formula (fun t ->
      pop t 3;
      check_sat t = Sat)

let%test "fixate_assumptions" =
  (with_unsat_formula mk_assume) (fun (t, _) ->
      fixate_assumptions t;
      ignore @@ check_sat t;
      0 = Array.length @@ get_unsat_assumptions t)

let%test "reset_assumptions" =
  (with_unsat_formula mk_assume) (fun (t, _) ->
      reset_assumptions t;
      check_sat t = Sat)

let%expect_test "dump_formula" =
  with_hard_formula (fun t ->
      set_option_str t Output_number_format "hex";
      dump_formula t `Smt2 std_formatter);
  [%expect{|
    (set-logic QF_BV)
    (declare-const a (_ BitVec 64))
    (declare-const b (_ BitVec 64))
    (assert
     (not
      (= a #x0000000000000001)))
    (assert
     (not
      (= b #x0000000000000001)))
    (assert
     (=
      (bvmul
       (concat #x0000000000000000 a)
       (concat #x0000000000000000 b)) #x87e03acc9f5050086f083d2d5d6b9d47))
    (check-sat)
    (exit) |}]

let%test "simplify" =
  with_sat_formula (fun (t, _) -> simplify t = Sat)

let%test "simplify" =
  with_hard_formula (fun t -> simplify t = Unknown)

let%expect_test "get_value" =
  dump term_dump with_sat_formula (fun (t, a) ->
      ignore @@ check_sat t;
      get_value t a);
  [%expect{| true |}]

let%expect_test "get_bv_value" =
  string with_t_naked (fun t ->
      set_option t Produce_models 1;
      let bv8 = mk_bv_sort t 8 in
      let a = mk_const t bv8 "a" in
      let v = mk_bv_value_int t bv8 42 in
      mk_assert t @@ mk_term2 t Equal a v;
      ignore @@ check_sat t;
      get_bv_value t a
    );
  [%expect {| 00101010 |}]

let%expect_test "get_fp_value" =
  string with_t_naked (fun t ->
      set_option t Produce_models 1;
      let fp32 = mk_fp_sort t 8 24 in
      let a = mk_const t fp32 "a" in
      let r = mk_rm_value t Rtz in
      let v = mk_fp_value_from_real t fp32 r "42" in
      mk_assert t @@ mk_term2 t Equal a v;
      ignore @@ check_sat t;
      let sign, exponent, significand = get_fp_value t a in
      sign ^ exponent ^ significand
    );
  [%expect {| 01000010001010000000000000000000 |}]

let%expect_test "get_rm_value" =
  string with_t_naked (fun t ->
      set_option t Produce_models 1;
      let rm = mk_rm_sort t in
      let a = mk_const t rm "a" in
      let v = mk_rm_value t Rtz in
      mk_assert t @@ mk_term2 t Equal a v;
      ignore @@ check_sat t;
      get_rm_value t a);
  [%expect {| RTZ |}]

let%expect_test "get_array_value" =
  with_t_naked (fun t ->
      set_option t Produce_models 1;
      let bv1 = mk_bool_sort t in
      let ar1_1 = mk_array_sort t bv1 bv1 in
      let a = mk_const t ar1_1 "a" in
      let one = mk_true t in
      let v = mk_const_array t ar1_1 one in
      mk_assert t @@ mk_term2 t Equal a v;
      ignore @@ check_sat t;
      let values, default = get_array_value t a in
      Array.iter (fun (i, v) -> Format.printf "a[%s] = %s"
                     (get_bv_value t i) (get_bv_value t v)) values;
      match default with
      | None -> ()
      | Some d -> Format.printf "a[.] = %s" (get_bv_value t d));
  [%expect {| a[.] = 1 |}]

let%expect_test "get_array_value" =
  with_t_naked (fun t ->
      set_option t Produce_models 1;
      let bv1 = mk_bool_sort t in
      let ar1_1 = mk_array_sort t bv1 bv1 in
      let a = mk_const t ar1_1 "a" in
      let one = mk_true t and zero = mk_false t in
      let s = mk_term2 t Array_select a one in
      mk_assert t @@ mk_term2 t Equal s one;
      let s = mk_term2 t Array_select a zero in
      mk_assert t @@ mk_term2 t Equal s zero;
      ignore @@ check_sat t;
      let values, default = get_array_value t a in
      Array.iter (fun (i, v) -> Format.printf "a[%s] = %s; "
                     (get_bv_value t i) (get_bv_value t v)) values;
      match default with
      | None -> ()
      | Some d -> Format.printf "a[.] = %s" (get_bv_value t d));
  [%expect {| a[1] = 1; a[0] = 0; |}]

let%expect_test "get_array_value" =
  with_t_naked (fun t ->
      set_option t Produce_models 1;
      let bv1 = mk_bool_sort t in
      let ar1_1 = mk_array_sort t bv1 bv1 in
      let a = mk_const t ar1_1 "a" in
      let one = mk_true t and zero = mk_false t in
      let v = mk_const_array t ar1_1 one in
      let v = mk_term3 t Array_store v zero zero in
      mk_assert t @@ mk_term2 t Equal a v;
      ignore @@ check_sat t;
      let values, default = get_array_value t a in
      Array.iter (fun (i, v) -> Format.printf "a[%s] = %s; "
                     (get_bv_value t i) (get_bv_value t v)) values;
      match default with
      | None -> ()
      | Some d -> Format.printf "a[.] = %s" (get_bv_value t d));
  [%expect {| a[0] = 0; a[.] = 1 |}]

let%expect_test "get_fun_value" =
  with_t_naked (fun t ->
      set_option t Produce_models 1;
      let bv1 = mk_bool_sort t in
      let fn1_1_1 = mk_fun_sort t [| bv1; bv1 |] bv1 in
      let a = mk_const t fn1_1_1 "a" in
      let one = mk_true t and zero = mk_false t in
      let v = mk_term3 t Apply a zero zero in
      mk_assert t @@ mk_term2 t Equal v one;
      let v = mk_term3 t Apply a zero one in
      mk_assert t @@ mk_term2 t Equal v zero;
      ignore @@ check_sat t;
      let values = get_fun_value t a in
      Array.iter (fun a ->
          print_string "(";
          print_string (get_bv_value t @@ Array.get a 0);
          for i = 1 to Array.length a - 2 do
            print_string ", ";
            print_string (get_bv_value t @@ Array.get a i)
          done;
          print_string "): ";
          print_string (get_bv_value t @@ Array.get a @@ Array.length a - 1);
          print_string "; ";) values);
  [%expect {| (0, 0): 1; (0, 1): 0; |}]

let%expect_test "print_model" =
  with_sat_formula (fun (t, _) ->
      ignore @@ check_sat t;
      print_model t `Smt2 std_formatter);
  [%expect{|
    (
      (define-fun a () (_ BitVec 1) #b1)
    ) |}]

let%test "is_unsat_assumption" =
  (with_unsat_formula mk_assume) (fun (t, e) ->
      ignore @@ check_sat t;
      is_unsat_assumption t e)

let%expect_test "get_unsat_assumptions" =
  dump (array_dump term_dump) (with_unsat_formula mk_assume) (fun (t, _) ->
      ignore @@ check_sat t;
      get_unsat_assumptions t);
  [%expect{| false |}]

let%expect_test "get_unsat_core" =
  dump (array_dump term_dump) (with_unsat_formula mk_assert) (fun (t, _) ->
      ignore @@ check_sat t;
      get_unsat_core t);
  [%expect{| false |}]
