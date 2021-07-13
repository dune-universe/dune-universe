open Bitwuzla
open Format

module Term = struct

  module type Debug = sig
    include module type of Once ()
    val copy : ?bindings:(Bitwuzla_c.term * Bitwuzla_c.term) list ->
      'a term -> 'a term
  end

  let with_debug f =
    let module S = struct
      include Once ()
      external of_variadic : 'a Term.variadic -> Bitwuzla_c.term list
        = "%identity"
      external to_variadic : Bitwuzla_c.sort list -> 'a Sort.variadic
        = "%identity"
      external of_term : Bitwuzla_c.term -> 'a term = "%identity"
      external to_term : 'a term -> Bitwuzla_c.term = "%identity"
      let rec copy :
        type a. ?bindings:(Bitwuzla_c.term * Bitwuzla_c.term) list ->
        a term -> a term =
        fun ?(bindings=[]) e ->
        match Term.view e with
        | Value k -> (k :> a term)
        | Const _ -> e
        | Var _ -> of_term @@ List.assoc (to_term e) bindings
        | Lambda (vars, e) ->
          let vars = of_variadic vars in
          let sorts = to_variadic @@ List.map Bitwuzla_c.term_get_sort vars in
          Term.Uf.lambda sorts (fun vars' ->
              let vars' = of_variadic vars' in
              let bindings = List.map2 (fun p n -> p, n) vars vars' in
              copy ~bindings e)
        | Equal (e0, e1) ->
          Term.equal (copy ~bindings e0) (copy ~bindings e1)
        | Distinct (e0, e1) ->
          Term.distinct (copy ~bindings e0) (copy ~bindings e1)
        | Ite (e0, e1, e2) ->
          Term.ite (copy ~bindings e0) (copy ~bindings e1) (copy ~bindings e2)
        | Bv (Term.Bv.Add, (e0, e1)) ->
          Term.Bv.term Term.Bv.Add (copy ~bindings e0) (copy ~bindings e1)
        | Bv (Term.Bv.And, (e0, e1)) ->
          Term.Bv.term Term.Bv.And (copy ~bindings e0) (copy ~bindings e1)
        | Bv (Term.Bv.Ashr, (e0, e1)) ->
          Term.Bv.term Term.Bv.Ashr (copy ~bindings e0) (copy ~bindings e1)
        | Bv (Term.Bv.Concat, (e0, e1)) ->
          Term.Bv.term Term.Bv.Concat (copy ~bindings e0) (copy ~bindings e1)
        | Bv (Term.Bv.Extract, (hi, lo, e0)) ->
          Term.Bv.term Term.Bv.Extract ~hi ~lo (copy ~bindings e0)
        | Bv (Term.Bv.Mul, (e0, e1)) ->
          Term.Bv.term Term.Bv.Mul (copy ~bindings e0) (copy ~bindings e1)
        | Bv (Term.Bv.Neg, (e0)) ->
          Term.Bv.term Term.Bv.Neg (copy ~bindings e0)
        | Bv (Term.Bv.Not, (e0)) ->
          Term.Bv.term Term.Bv.Not (copy ~bindings e0)
        | Bv (Term.Bv.Or, (e0, e1)) ->
          Term.Bv.term Term.Bv.Or (copy ~bindings e0) (copy ~bindings e1)
        | Bv (Term.Bv.Rol, (e0, e1)) ->
          Term.Bv.term Term.Bv.Rol (copy ~bindings e0) (copy ~bindings e1)
        | Bv (Term.Bv.Ror, (e0, e1)) ->
          Term.Bv.term Term.Bv.Ror (copy ~bindings e0) (copy ~bindings e1)
        | Bv (Term.Bv.Sdiv, (e0, e1)) ->
          Term.Bv.term Term.Bv.Sdiv (copy ~bindings e0) (copy ~bindings e1)
        | Bv (Term.Bv.Sge, (e0, e1)) ->
          Term.Bv.term Term.Bv.Sge (copy ~bindings e0) (copy ~bindings e1)
        | Bv (Term.Bv.Sgt, (e0, e1)) ->
          Term.Bv.term Term.Bv.Sgt (copy ~bindings e0) (copy ~bindings e1)
        | Bv (Term.Bv.Shl, (e0, e1)) ->
          Term.Bv.term Term.Bv.Shl (copy ~bindings e0) (copy ~bindings e1)
        | Bv (Term.Bv.Shr, (e0, e1)) ->
          Term.Bv.term Term.Bv.Shr (copy ~bindings e0) (copy ~bindings e1)
        | Bv (Term.Bv.Sle, (e0, e1)) ->
          Term.Bv.term Term.Bv.Sle (copy ~bindings e0) (copy ~bindings e1)
        | Bv (Term.Bv.Slt, (e0, e1)) ->
          Term.Bv.term Term.Bv.Slt (copy ~bindings e0) (copy ~bindings e1)
        | Bv (Term.Bv.Smod, (e0, e1)) ->
          Term.Bv.term Term.Bv.Smod (copy ~bindings e0) (copy ~bindings e1)
        | Bv (Term.Bv.Srem, (e0, e1)) ->
          Term.Bv.term Term.Bv.Srem (copy ~bindings e0) (copy ~bindings e1)
        | Bv (Term.Bv.Sub, (e0, e1)) ->
          Term.Bv.term Term.Bv.Sub (copy ~bindings e0) (copy ~bindings e1)
        | Bv (Term.Bv.Udiv, (e0, e1)) ->
          Term.Bv.term Term.Bv.Udiv (copy ~bindings e0) (copy ~bindings e1)
        | Bv (Term.Bv.Uge, (e0, e1)) ->
          Term.Bv.term Term.Bv.Uge (copy ~bindings e0) (copy ~bindings e1)
        | Bv (Term.Bv.Ugt, (e0, e1)) ->
          Term.Bv.term Term.Bv.Ugt (copy ~bindings e0) (copy ~bindings e1)
        | Bv (Term.Bv.Ule, (e0, e1)) ->
          Term.Bv.term Term.Bv.Ule (copy ~bindings e0) (copy ~bindings e1)
        | Bv (Term.Bv.Ult, (e0, e1)) ->
          Term.Bv.term Term.Bv.Ult (copy ~bindings e0) (copy ~bindings e1)
        | Bv (Term.Bv.Urem, (e0, e1)) ->
          Term.Bv.term Term.Bv.Urem (copy ~bindings e0) (copy ~bindings e1)
        | Bv (Term.Bv.Xor, (e0, e1)) ->
          Term.Bv.term Term.Bv.Xor (copy ~bindings e0) (copy ~bindings e1)
        | Fp (Term.Fp.Abs, (e0)) ->
          Term.Fp.term Term.Fp.Abs (copy ~bindings e0)
        | Fp (Term.Fp.Add, (e0, e1, e2)) ->
          Term.Fp.term Term.Fp.Add (copy ~bindings e0)
            (copy ~bindings e1) (copy ~bindings e2)
        | Fp (Term.Fp.Div, (e0, e1, e2)) ->
          Term.Fp.term Term.Fp.Div (copy ~bindings e0)
            (copy ~bindings e1) (copy ~bindings e2)
        | Fp (Term.Fp.Eq, (e0, e1)) ->
          Term.Fp.term Term.Fp.Eq (copy ~bindings e0) (copy ~bindings e1)
        | Fp (Term.Fp.Fma, (e0, e1, e2, e3)) ->
          Term.Fp.term Term.Fp.Fma (copy ~bindings e0)
            (copy ~bindings e1) (copy ~bindings e2) (copy ~bindings e3)
        | Fp (Term.Fp.Fp, { sign; exponent; significand }) ->
          Term.Fp.term Term.Fp.Fp ~sign:(copy ~bindings sign)
            ~exponent:(copy ~bindings exponent) (copy ~bindings significand)
        | Fp (Term.Fp.Geq, (e0, e1)) ->
          Term.Fp.term Term.Fp.Geq (copy ~bindings e0) (copy ~bindings e1)
        | Fp (Term.Fp.Gt, (e0, e1)) ->
          Term.Fp.term Term.Fp.Gt (copy ~bindings e0) (copy ~bindings e1)
        | Fp (Term.Fp.Is_inf, (e0)) ->
          Term.Fp.term Term.Fp.Is_inf (copy ~bindings e0)
        | Fp (Term.Fp.Is_nan, (e0)) ->
          Term.Fp.term Term.Fp.Is_nan (copy ~bindings e0)
        | Fp (Term.Fp.Is_neg, (e0)) ->
          Term.Fp.term Term.Fp.Is_neg (copy ~bindings e0)
        | Fp (Term.Fp.Is_normal, (e0)) ->
          Term.Fp.term Term.Fp.Is_normal (copy ~bindings e0)
        | Fp (Term.Fp.Is_pos, (e0)) ->
          Term.Fp.term Term.Fp.Is_pos (copy ~bindings e0)
        | Fp (Term.Fp.Is_subnormal, (e0)) ->
          Term.Fp.term Term.Fp.Is_subnormal (copy ~bindings e0)
        | Fp (Term.Fp.Is_zero, (e0)) ->
          Term.Fp.term Term.Fp.Is_zero (copy ~bindings e0)
        | Fp (Term.Fp.Leq, (e0, e1)) ->
          Term.Fp.term Term.Fp.Leq (copy ~bindings e0) (copy ~bindings e1)
        | Fp (Term.Fp.Lt, (e0, e1)) ->
          Term.Fp.term Term.Fp.Lt (copy ~bindings e0) (copy ~bindings e1)
        | Fp (Term.Fp.Max, (e0, e1)) ->
          Term.Fp.term Term.Fp.Max (copy ~bindings e0) (copy ~bindings e1)
        | Fp (Term.Fp.Min, (e0, e1)) ->
          Term.Fp.term Term.Fp.Min (copy ~bindings e0) (copy ~bindings e1)
        | Fp (Term.Fp.Mul, (e0, e1, e2)) ->
          Term.Fp.term Term.Fp.Mul (copy ~bindings e0)
            (copy ~bindings e1) (copy ~bindings e2)
        | Fp (Term.Fp.Neg, (e0)) ->
          Term.Fp.term Term.Fp.Neg (copy ~bindings e0)
        | Fp (Term.Fp.Rem, (e0, e1)) ->
          Term.Fp.term Term.Fp.Rem (copy ~bindings e0) (copy ~bindings e1)
        | Fp (Term.Fp.Rti, (e0, e1)) ->
          Term.Fp.term Term.Fp.Rti (copy ~bindings e0) (copy ~bindings e1)
        | Fp (Term.Fp.Sqrt, (e0, e1)) ->
          Term.Fp.term Term.Fp.Sqrt (copy ~bindings e0) (copy ~bindings e1)
        | Fp (Term.Fp.Sub, (e0, e1, e2)) ->
          Term.Fp.term Term.Fp.Sub (copy ~bindings e0)
            (copy ~bindings e1) (copy ~bindings e2)
        | Fp (Term.Fp.From_bv, (exponent, sz, e2)) ->
          Term.Fp.term Term.Fp.From_bv ~exponent sz (copy ~bindings e2)
        | Fp (Term.Fp.From_fp, (exponent, sz, e1, e2)) ->
          Term.Fp.term Term.Fp.From_fp ~exponent sz
            (copy ~bindings e1) (copy ~bindings e2)
        | Fp (Term.Fp.From_sbv, (exponent, sz, e1, e2)) ->
          Term.Fp.term Term.Fp.From_sbv ~exponent sz
            (copy ~bindings e1) (copy ~bindings e2)
        | Fp (Term.Fp.From_ubv, (exponent, sz, e1, e2)) ->
          Term.Fp.term Term.Fp.From_ubv ~exponent sz
            (copy ~bindings e1) (copy ~bindings e2)
        | Fp (Term.Fp.To_sbv, (sz, e1, e2)) ->
          Term.Fp.term Term.Fp.To_sbv sz
            (copy ~bindings e1) (copy ~bindings e2)
        | Fp (Term.Fp.To_ubv, (sz, e1, e2)) ->
          Term.Fp.term Term.Fp.To_ubv sz
            (copy ~bindings e1) (copy ~bindings e2)
        | Select (e0, e1) ->
          Term.Ar.select (copy ~bindings e0) (copy ~bindings e1)
        | Store (e0, e1, e2) ->
          Term.Ar.store (copy ~bindings e0)
            (copy ~bindings e1) (copy ~bindings e2)
        | Apply (f, args) ->
          Term.Uf.apply f args
    end in
    f (module S : Debug)

  let%expect_test "equal" =
    let open Once () in
    let a = Term.const Sort.bool "a" and b = Term.const Sort.bool "b" in
    Term.pp std_formatter @@ Term.equal a b;
    [%expect {| (= a b) |}]

  let%expect_test "equal" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.equal a b;
    [%expect {| (= a b) |}]

  let%expect_test "equal" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    let a = Term.const fp16 "a" and b = Term.const fp16 "b" in
    Term.pp std_formatter @@ Term.equal a b;
    [%expect {| (= a b) |}]

  let%expect_test "equal" =
    let open Once () in
    let bv8 = Sort.bv 8 and bv32 = Sort.bv 32 in
    let ar32_8 = Sort.ar bv32 bv8 in
    let a = Term.const ar32_8 "a" and b = Term.const ar32_8 "b" in
    Term.pp std_formatter @@ Term.equal a b;
    [%expect {| (= a b) |}]

  let%expect_test "distinct" =
    let open Once () in
    let a = Term.const Sort.bool "a" and b = Term.const Sort.bool "b" in
    Term.pp std_formatter @@ Term.distinct a b;
    [%expect {| (not (= a b)) |}]

  let%expect_test "distinct" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.distinct a b;
    [%expect {| (not (= a b)) |}]

  let%expect_test "distinct" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    let a = Term.const fp16 "a" and b = Term.const fp16 "b" in
    Term.pp std_formatter @@ Term.distinct a b;
    [%expect {| (not (= a b)) |}]

  let%expect_test "distinct" =
    let open Once () in
    let bv8 = Sort.bv 8 and bv32 = Sort.bv 32 in
    let ar32_8 = Sort.ar bv32 bv8 in
    let a = Term.const ar32_8 "a" and b = Term.const ar32_8 "b" in
    Term.pp std_formatter @@ Term.distinct a b;
    [%expect {| (not (= a b)) |}]

  let%expect_test "ite" =
    let open Once () in
    let a = Term.const Sort.bool "a" and b = Term.const Sort.bool "b"
    and c = Term.const Sort.bool "c" in
    Term.pp std_formatter @@ Term.ite a b c;
    [%expect {| (bvand (bvnot (bvand a (bvnot b))) (bvnot (bvand (bvnot a) (bvnot c)))) |}]

  let%expect_test "ite" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b"
    and c = Term.const bv8 "c" in
    Term.pp std_formatter @@ Term.ite (Term.Bv.to_bl a) b c;
    [%expect {| (ite (= a #b00000000) c b) |}]

  let%expect_test "ite" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    let a = Term.const Sort.bool "a"
    and b = Term.const fp16 "b" and c = Term.const fp16 "c" in
    Term.pp std_formatter @@ Term.ite a b c;
    [%expect {| (ite (= #b1 a) b c) |}]

  let%expect_test "ite" =
    let open Once () in
    let bv8 = Sort.bv 8 and bv32 = Sort.bv 32 in
    let ar32_8 = Sort.ar bv32 bv8 in
    let a = Term.const Sort.bool "a"
    and b = Term.const ar32_8 "b" and c = Term.const ar32_8 "c" in
    Term.pp std_formatter @@ Term.ite a b c;
    [%expect {| (ite (= #b1 a) b c) |}]

  let%expect_test "hash" =
    let open Once () in
    let a = Term.const Sort.bool "a" in
    print_int @@ Term.hash a;
    [%expect {| 7 |}]

  let%expect_test "hash" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" in
    print_int @@ Term.hash a;
    [%expect {| 7 |}]

  let%expect_test "sort" =
    let open Once () in
    let a = Term.const Sort.bool "a" in
    Sort.pp std_formatter @@ Term.sort a;
    [%expect {| (_ BitVec 1) |}]

  let%expect_test "sort" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" in
    Sort.pp std_formatter @@ Term.sort a;
    [%expect {| (_ BitVec 8) |}]

  let%expect_test "Bl.zero" =
    let open Once () in
    Term.pp std_formatter Term.Bl.false';
    [%expect {| false |}]

  let%expect_test "Bl.true'" =
    let open Once () in
    Term.pp std_formatter Term.Bl.true';
    [%expect {| true |}]

  let%expect_test "Bl.assignment" =
    let open Once () in
    pp_print_bool std_formatter
    @@ Term.Bl.assignment @@ get_value @@ Term.Bl.false';
    [%expect {| false |}]

  let%expect_test "Bl.assignment" =
    let open Once () in
    pp_print_bool std_formatter
    @@ Term.Bl.assignment @@ get_value @@ Term.Bl.true';
    [%expect {| true |}]

  let%expect_test "Bl.logand" =
    let open Once () in
    let a = Term.const Sort.bool "a" and b = Term.const Sort.bool "b" in
    Term.pp std_formatter @@ Term.Bl.logand a b;
    [%expect {| (bvand a b) |}]

  let%expect_test "Bl.lognand" =
    let open Once () in
    let a = Term.const Sort.bool "a" and b = Term.const Sort.bool "b" in
    Term.pp std_formatter @@ Term.Bl.lognand a b;
    [%expect {| (bvnot (bvand a b)) |}]

  let%expect_test "Bl.redand" =
    let open Once () in
    let a = Term.const Sort.bool "a" and b = Term.const Sort.bool "b"
    and c = Term.const Sort.bool "c" in
    Term.pp std_formatter @@ Term.Bl.redand [| a; b; c |];
    [%expect {| (bvand c (bvand a b)) |}]


  let%expect_test "Bl.logor" =
    let open Once () in
    let a = Term.const Sort.bool "a" and b = Term.const Sort.bool "b" in
    Term.pp std_formatter @@ Term.Bl.logor a b;
    [%expect {| (bvnot (bvand (bvnot a) (bvnot b))) |}]

  let%expect_test "Bl.lognor" =
    let open Once () in
    let a = Term.const Sort.bool "a" and b = Term.const Sort.bool "b" in
    Term.pp std_formatter @@ Term.Bl.lognor a b;
    [%expect {| (bvand (bvnot a) (bvnot b)) |}]

  let%expect_test "Bl.redor" =
    let open Once () in
    let a = Term.const Sort.bool "a" and b = Term.const Sort.bool "b"
    and c = Term.const Sort.bool "c" in
    Term.pp std_formatter @@ Term.Bl.redor [| a; b; c |];
    [%expect {| (bvnot (bvand (bvnot c) (bvand (bvnot a) (bvnot b)))) |}]

  let%expect_test "Bl.logxor" =
    let open Once () in
    let a = Term.const Sort.bool "a" and b = Term.const Sort.bool "b" in
    Term.pp std_formatter @@ Term.Bl.logxor a b;
    [%expect {| (bvand (bvnot (bvand (bvnot a) (bvnot b))) (bvnot (bvand a b))) |}]

  let%expect_test "Bl.logxnor" =
    let open Once () in
    let a = Term.const Sort.bool "a" and b = Term.const Sort.bool "b" in
    Term.pp std_formatter @@ Term.Bl.logxnor a b;
    [%expect {| (bvnot (bvand (bvnot (bvand (bvnot a) (bvnot b))) (bvnot (bvand a b)))) |}]

  let%expect_test "Bl.redxor" =
    let open Once () in
    let a = Term.const Sort.bool "a" and b = Term.const Sort.bool "b"
    and c = Term.const Sort.bool "c" in
    Term.pp std_formatter @@ Term.Bl.redxor [| a; b; c |];
    [%expect {| (let (($e1 (bvand (bvnot (bvand (bvnot a) (bvnot b))) (bvnot (bvand a b))))) (bvand (bvnot (bvand (bvnot c) (bvnot $e1))) (bvnot (bvand c $e1)))) |}]

  let%expect_test "Bl.lognot" =
    let open Once () in
    let a = Term.const Sort.bool "a" in
    Term.pp std_formatter @@ Term.Bl.lognot a;
    [%expect {| (bvnot a) |}]

  let%expect_test "Bl.iff" =
    let open Once () in
    let a = Term.const Sort.bool "a" and b = Term.const Sort.bool "b" in
    Term.pp std_formatter @@ Term.Bl.iff a b;
    [%expect {| (= a b) |}]

  let%expect_test "Bl.implies" =
    let open Once () in
    let a = Term.const Sort.bool "a" and b = Term.const Sort.bool "b" in
    Term.pp std_formatter @@ Term.Bl.implies a b;
    [%expect {| (bvnot (bvand a (bvnot b))) |}]

  let%expect_test "Bv.zero" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    Term.pp std_formatter @@ Term.Bv.zero bv8;
    [%expect {| #b00000000 |}]

  let%expect_test "Bv.one" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    Term.pp std_formatter @@ Term.Bv.one bv8;
    [%expect {| #b00000001 |}]

  let%expect_test "Bv.ones" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    Term.pp std_formatter @@ Term.Bv.ones bv8;
    [%expect {| #b11111111 |}]

  let%expect_test "Bv.min_signed" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    Term.pp std_formatter @@ Term.Bv.min_signed bv8;
    [%expect {| #b10000000 |}]

  let%expect_test "Bv.max_signed" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    Term.pp std_formatter @@ Term.Bv.max_signed bv8;
    [%expect {| #b01111111 |}]

  let%expect_test "Bv.of_int" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    Term.pp std_formatter @@ Term.Bv.of_int bv8 42;
    [%expect {| #b00101010 |}]

  let%expect_test "Bv.of_string" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    Term.pp std_formatter @@ Term.Bv.of_string bv8 "42";
    [%expect {| #b00101010 |}]

  let%expect_test "Bv.of_string" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    Term.pp std_formatter @@ Term.Bv.of_string bv8 "0b00101010";
    [%expect {| #b00101010 |}]

  let%expect_test "Bv.of_string" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    Term.pp std_formatter @@ Term.Bv.of_string bv8 "0x2a";
    [%expect {| #b00101010 |}]

  let%expect_test "Bv.assignment" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    Z.print @@ Term.Bv.assignment @@ get_value @@ Term.Bv.of_int bv8 42;
    [%expect {| 42 |}]

  let%expect_test "Bv.term" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" in
    Term.pp std_formatter @@ Term.Bv.term Term.Bv.Neg a;
    [%expect {| (bvadd (bvnot a) #b00000001) |}]

  let%expect_test "Bv.term" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.term Term.Bv.Add a b;
    [%expect {| (bvadd a b) |}]

  let%expect_test "Bv.term" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.term Term.Bv.Sub a b;
    [%expect {| (bvadd a (bvadd (bvnot b) #b00000001)) |}]

  let%expect_test "Bv.term" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.term Term.Bv.Mul a b;
    [%expect {| (bvmul a b) |}]

  let%expect_test "Bv.term" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.term Term.Bv.Sdiv a b;
    [%expect {| (let (($e1 ((_ extract 7 7) a))) (let (($e2 ((_ extract 7 7) b))) (let (($e3 (bvudiv (ite (= #b1 $e1) (bvadd (bvnot a) #b00000001) a) (ite (= #b1 $e2) (bvadd (bvnot b) #b00000001) b)))) (ite (= #b1 (bvand (bvnot (bvand (bvnot $e1) (bvnot $e2))) (bvnot (bvand $e1 $e2)))) (bvadd #b00000001 (bvnot $e3)) $e3)))) |}]

  let%expect_test "Bv.term" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.term Term.Bv.Udiv a b;
    [%expect {| (bvudiv a b) |}]

  let%expect_test "Bv.term" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.term Term.Bv.Smod a b;
    [%expect {| (let (($e1 ((_ extract 7 7) a))) (let (($e2 ((_ extract 7 7) b))) (let (($e3 (bvurem (ite (= #b1 $e1) (bvadd (bvnot a) #b00000001) a) (ite (= #b1 $e2) (bvadd (bvnot b) #b00000001) b)))) (let (($e4 (= #b00000000 $e3))) (let (($e5 (bvadd #b00000001 (bvnot $e3)))) (bvnot (bvand (bvand (bvnot (ite (= #b1 (bvand (bvnot $e1) (bvnot $e2))) $e3 #b00000000)) (bvnot (ite (= #b1 (bvand (bvand $e1 (bvnot $e2)) (bvnot (ite $e4 #b1 #b0)))) (bvadd b $e5) #b00000000))) (bvand (bvnot (ite (= #b1 (bvand (bvand (bvnot $e1) $e2) (bvnot (ite $e4 #b1 #b0)))) (bvadd b $e3) #b00000000)) (bvnot (ite (= #b1 (bvand $e1 $e2)) $e5 #b00000000)))))))))) |}]

  let%expect_test "Bv.term" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.term Term.Bv.Srem a b;
    [%expect {| (let (($e1 ((_ extract 7 7) a))) (let (($e2 (bvurem (ite (= #b1 $e1) (bvadd (bvnot a) #b00000001) a) (ite (= #b1 ((_ extract 7 7) b)) (bvadd (bvnot b) #b00000001) b)))) (ite (= #b1 $e1) (bvadd #b00000001 (bvnot $e2)) $e2))) |}]

  let%expect_test "Bv.term" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.term Term.Bv.Urem a b;
    [%expect {| (bvurem a b) |}]

  let%expect_test "Bv.term" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.term Term.Bv.And a b;
    [%expect {| (bvand a b) |}]

  let%expect_test "Bv.term" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.term Term.Bv.Or a b;
    [%expect {| (bvnot (bvand (bvnot a) (bvnot b))) |}]

  let%expect_test "Bv.term" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.term Term.Bv.Xor a b;
    [%expect {| (bvand (bvnot (bvand (bvnot a) (bvnot b))) (bvnot (bvand a b))) |}]

  let%expect_test "Bv.term" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" in
    Term.pp std_formatter @@ Term.Bv.term Term.Bv.Not a;
    [%expect {| (bvnot a) |}]

  let%expect_test "Bv.term" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.term Term.Bv.Shl a b;
    [%expect {| (bvshl a b) |}]

  let%expect_test "Bv.term" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.term Term.Bv.Ashr a b;
    [%expect {| (ite (= #b1 ((_ extract 7 7) a)) (bvnot (bvlshr (bvnot a) b)) (bvlshr a b)) |}]

  let%expect_test "Bv.term" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.term Term.Bv.Shr a b;
    [%expect {| (bvlshr a b) |}]

  let%expect_test "Bv.term" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.term Term.Bv.Rol a b;
    [%expect {| (let (($e1 (bvurem b #b00001000))) (ite (= $e1 #b00000000) a (bvnot (bvand (bvnot (bvshl a $e1)) (bvnot (bvlshr a (bvadd (bvnot $e1) #b00001001))))))) |}]

  let%expect_test "Bv.term" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.term Term.Bv.Ror a b;
    [%expect {| (let (($e1 (bvurem b #b00001000))) (ite (= $e1 #b00000000) a (bvnot (bvand (bvnot (bvshl a (bvadd (bvnot $e1) #b00001001))) (bvnot (bvlshr a $e1)))))) |}]

  let%expect_test "Bv.term" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.term Term.Bv.Concat a b;
    [%expect {| (concat a b) |}]

  let%expect_test "Bv.term" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" in
    Term.pp std_formatter @@ Term.Bv.term Term.Bv.Extract ~hi:3 ~lo:2 a;
    [%expect {| ((_ extract 3 2) a) |}]

  let%expect_test "Bv.term" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.term Term.Bv.Sge a b;
    [%expect {| (not (bvslt a b)) |}]

  let%expect_test "Bv.term" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.term Term.Bv.Uge a b;
    [%expect {| (not (bvult a b)) |}]

  let%expect_test "Bv.term" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.term Term.Bv.Sgt a b;
    [%expect {| (bvslt b a) |}]

  let%expect_test "Bv.term" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.term Term.Bv.Ugt a b;
    [%expect {| (bvult b a) |}]


  let%expect_test "Bv.term" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.term Term.Bv.Sle a b;
    [%expect {| (not (bvslt b a)) |}]

  let%expect_test "Bv.term" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.term Term.Bv.Ule a b;
    [%expect {| (not (bvult b a)) |}]

  let%expect_test "Bv.term" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.term Term.Bv.Slt a b;
    [%expect {| (bvslt a b) |}]

  let%expect_test "Bv.term" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.term Term.Bv.Ult a b;
    [%expect {| (bvult a b) |}]

  let%expect_test "Bv.pred" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" in
    Term.pp std_formatter @@ Term.Bv.pred a;
    [%expect {| (bvadd a #b11111111) |}]

  let%expect_test "Bv.succ" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" in
    Term.pp std_formatter @@ Term.Bv.succ a;
    [%expect {| (bvadd a #b00000001) |}]

  let%expect_test "Bv.neg" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" in
    Term.pp std_formatter @@ Term.Bv.neg a;
    [%expect {| (bvadd (bvnot a) #b00000001) |}]

  let%expect_test "Bv.add" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.add a b;
    [%expect {| (bvadd a b) |}]

  let%expect_test "Bv.sadd_overflow" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.sadd_overflow a b;
    [%expect {| (let (($e1 ((_ extract 7 7) a))) (let (($e2 ((_ extract 7 7) b))) (let (($e3 ((_ extract 7 7) (bvadd a b)))) (bvnot (bvand (bvnot (bvand (bvnot $e3) (bvand $e1 $e2))) (bvnot (bvand $e3 (bvand (bvnot $e1) (bvnot $e2))))))))) |}]

  let%expect_test "Bv.uadd_overflow" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.uadd_overflow a b;
    [%expect {| ((_ extract 8 8) (bvadd (concat #b0 a) (concat #b0 b))) |}]

  let%expect_test "Bv.sub" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.sub a b;
    [%expect {| (bvadd a (bvadd (bvnot b) #b00000001)) |}]

  let%expect_test "Bv.ssub_overflow" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.ssub_overflow a b;
    [%expect {| (let (($e1 ((_ extract 7 7) a))) (let (($e2 ((_ extract 7 7) b))) (let (($e3 ((_ extract 7 7) (bvadd a (bvadd (bvnot b) #b00000001))))) (bvnot (bvand (bvnot (bvand $e3 (bvand (bvnot $e1) $e2))) (bvnot (bvand (bvnot $e3) (bvand $e1 (bvnot $e2))))))))) |}]

  let%expect_test "Bv.usub_overflow" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.usub_overflow a b;
    [%expect {| (bvnot ((_ extract 8 8) (bvadd (concat #b0 a) (bvadd (concat #b0 (bvnot b)) #b000000001)))) |}]

  let%expect_test "Bv.mul" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.mul a b;
    [%expect {| (bvmul a b) |}]

  let%expect_test "Bv.smul_overflow" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.smul_overflow a b;
    [%expect {| (let (($e1 ((_ extract 7 7) a))) (let (($e2 ((_ extract 7 7) b))) (let (($e3 (concat (ite (= #b1 $e1) #b1111111 #b0000000) $e1))) (let (($e4 (concat (ite (= #b1 $e2) #b1111111 #b0000000) $e2))) (let (($e5 (bvand (bvnot (bvand (bvnot a) (bvnot $e3))) (bvnot (bvand a $e3))))) (let (($e6 (bvand (bvnot (bvand (bvnot b) (bvnot $e4))) (bvnot (bvand b $e4))))) (let (($e7 ((_ extract 6 6) $e6))) (let (($e8 (bvand (bvnot $e7) (bvnot ((_ extract 5 5) $e6))))) (let (($e9 (bvand $e8 (bvnot ((_ extract 4 4) $e6))))) (let (($e10 (bvand $e9 (bvnot ((_ extract 3 3) $e6))))) (let (($e11 (bvand $e10 (bvnot ((_ extract 2 2) $e6))))) (let (($e12 (bvmul (concat $e1 a) (concat $e2 b)))) (let (($e13 ((_ extract 8 8) $e12))) (let (($e14 ((_ extract 7 7) $e12))) (bvnot (bvand (bvand (bvand (bvand (bvand (bvand (bvnot (bvand $e7 ((_ extract 1 1) $e5))) (bvnot (bvand (bvnot $e8) ((_ extract 2 2) $e5)))) (bvnot (bvand (bvnot $e9) ((_ extract 3 3) $e5)))) (bvnot (bvand (bvnot $e10) ((_ extract 4 4) $e5)))) (bvnot (bvand (bvnot $e11) ((_ extract 5 5) $e5)))) (bvnot (bvand (bvnot (bvand $e11 (bvnot ((_ extract 1 1) $e6)))) ((_ extract 6 6) $e5)))) (bvnot (bvand (bvnot (bvand (bvnot $e13) (bvnot $e14))) (bvnot (bvand $e13 $e14)))))))))))))))))))) |}]

  let%expect_test "Bv.umul_overflow" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.umul_overflow a b;
    [%expect {| (let (($e1 ((_ extract 7 7) b))) (let (($e2 (bvand (bvnot $e1) (bvnot ((_ extract 6 6) b))))) (let (($e3 (bvand $e2 (bvnot ((_ extract 5 5) b))))) (let (($e4 (bvand $e3 (bvnot ((_ extract 4 4) b))))) (let (($e5 (bvand $e4 (bvnot ((_ extract 3 3) b))))) (let (($e6 (bvand $e5 (bvnot ((_ extract 2 2) b))))) (bvnot (bvand (bvand (bvand (bvand (bvand (bvand (bvand (bvnot (bvand $e1 ((_ extract 1 1) a))) (bvnot (bvand (bvnot $e2) ((_ extract 2 2) a)))) (bvnot (bvand (bvnot $e3) ((_ extract 3 3) a)))) (bvnot (bvand (bvnot $e4) ((_ extract 4 4) a)))) (bvnot (bvand (bvnot $e5) ((_ extract 5 5) a)))) (bvnot (bvand (bvnot $e6) ((_ extract 6 6) a)))) (bvnot (bvand (bvnot (bvand $e6 (bvnot ((_ extract 1 1) b)))) ((_ extract 7 7) a)))) (bvnot ((_ extract 8 8) (bvmul (concat #b0 a) (concat #b0 b)))))))))))) |}]

  let%expect_test "Bv.sdiv" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.sdiv a b;
    [%expect {| (let (($e1 ((_ extract 7 7) a))) (let (($e2 ((_ extract 7 7) b))) (let (($e3 (bvudiv (ite (= #b1 $e1) (bvadd (bvnot a) #b00000001) a) (ite (= #b1 $e2) (bvadd (bvnot b) #b00000001) b)))) (ite (= #b1 (bvand (bvnot (bvand (bvnot $e1) (bvnot $e2))) (bvnot (bvand $e1 $e2)))) (bvadd #b00000001 (bvnot $e3)) $e3)))) |}]

  let%expect_test "Bv.sdiv_overflow" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.sdiv_overflow a b;
    [%expect {| (and (= b #b11111111) (= a #b10000000)) |}]

  let%expect_test "Bv.udiv" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.udiv a b;
    [%expect {| (bvudiv a b) |}]

  let%expect_test "Bv.smod" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.smod a b;
    [%expect {| (let (($e1 ((_ extract 7 7) a))) (let (($e2 ((_ extract 7 7) b))) (let (($e3 (bvurem (ite (= #b1 $e1) (bvadd (bvnot a) #b00000001) a) (ite (= #b1 $e2) (bvadd (bvnot b) #b00000001) b)))) (let (($e4 (= #b00000000 $e3))) (let (($e5 (bvadd #b00000001 (bvnot $e3)))) (bvnot (bvand (bvand (bvnot (ite (= #b1 (bvand (bvnot $e1) (bvnot $e2))) $e3 #b00000000)) (bvnot (ite (= #b1 (bvand (bvand $e1 (bvnot $e2)) (bvnot (ite $e4 #b1 #b0)))) (bvadd b $e5) #b00000000))) (bvand (bvnot (ite (= #b1 (bvand (bvand (bvnot $e1) $e2) (bvnot (ite $e4 #b1 #b0)))) (bvadd b $e3) #b00000000)) (bvnot (ite (= #b1 (bvand $e1 $e2)) $e5 #b00000000)))))))))) |}]

  let%expect_test "Bv.srem" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.srem a b;
    [%expect {| (let (($e1 ((_ extract 7 7) a))) (let (($e2 (bvurem (ite (= #b1 $e1) (bvadd (bvnot a) #b00000001) a) (ite (= #b1 ((_ extract 7 7) b)) (bvadd (bvnot b) #b00000001) b)))) (ite (= #b1 $e1) (bvadd #b00000001 (bvnot $e2)) $e2))) |}]

  let%expect_test "Bv.urem" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.urem a b;
    [%expect {| (bvurem a b) |}]

  let%expect_test "Bv.logand" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.logand a b;
    [%expect {| (bvand a b) |}]

  let%expect_test "Bv.lognand" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.lognand a b;
    [%expect {| (bvnot (bvand a b)) |}]

  let%expect_test "Bv.redand" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" in
    Term.pp std_formatter @@ Term.Bv.redand a;
    [%expect {| (= a #b11111111) |}]

  let%expect_test "Bv.logor" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.logor a b;
    [%expect {| (bvnot (bvand (bvnot a) (bvnot b))) |}]

  let%expect_test "Bv.lognor" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.lognor a b;
    [%expect {| (bvand (bvnot a) (bvnot b)) |}]

  let%expect_test "Bv.redor" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" in
    Term.pp std_formatter @@ Term.Bv.redor a;
    [%expect {| (not (= a #b00000000)) |}]

  let%expect_test "Bv.logxor" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.logxor a b;
    [%expect {| (bvand (bvnot (bvand (bvnot a) (bvnot b))) (bvnot (bvand a b))) |}]

  let%expect_test "Bv.logxnor" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.logxnor a b;
    [%expect {| (bvnot (bvand (bvnot (bvand (bvnot a) (bvnot b))) (bvnot (bvand a b)))) |}]

  let%expect_test "Bv.redxor" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" in
    Term.pp std_formatter @@ Term.Bv.redxor a;
    [%expect {| (let (($e1 ((_ extract 0 0) a))) (let (($e2 ((_ extract 1 1) a))) (let (($e3 (bvand (bvnot (bvand (bvnot $e1) (bvnot $e2))) (bvnot (bvand $e1 $e2))))) (let (($e4 ((_ extract 2 2) a))) (let (($e5 (bvand (bvnot (bvand (bvnot $e3) (bvnot $e4))) (bvnot (bvand $e3 $e4))))) (let (($e6 ((_ extract 3 3) a))) (let (($e7 (bvand (bvnot (bvand (bvnot $e5) (bvnot $e6))) (bvnot (bvand $e5 $e6))))) (let (($e8 ((_ extract 4 4) a))) (let (($e9 (bvand (bvnot (bvand (bvnot $e7) (bvnot $e8))) (bvnot (bvand $e7 $e8))))) (let (($e10 ((_ extract 5 5) a))) (let (($e11 (bvand (bvnot (bvand (bvnot $e9) (bvnot $e10))) (bvnot (bvand $e9 $e10))))) (let (($e12 ((_ extract 6 6) a))) (let (($e13 (bvand (bvnot (bvand (bvnot $e11) (bvnot $e12))) (bvnot (bvand $e11 $e12))))) (let (($e14 ((_ extract 7 7) a))) (bvand (bvnot (bvand (bvnot $e13) (bvnot $e14))) (bvnot (bvand $e13 $e14))))))))))))))))) |}]

  let%expect_test "Bv.lognot" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" in
    Term.pp std_formatter @@ Term.Bv.lognot a;
    [%expect {| (bvnot a) |}]

  let%expect_test "Bv.shift_left" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.shift_left a b;
    [%expect {| (bvshl a b) |}]

  let%expect_test "Bv.shift_right" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.shift_right a b;
    [%expect {| (ite (= #b1 ((_ extract 7 7) a)) (bvnot (bvlshr (bvnot a) b)) (bvlshr a b)) |}]

  let%expect_test "Bv.shift_right_logical" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.shift_right_logical a b;
    [%expect {| (bvlshr a b) |}]

  let%expect_test "Bv.rotate_left" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.rotate_left a b;
    [%expect {| (let (($e1 (bvurem b #b00001000))) (ite (= $e1 #b00000000) a (bvnot (bvand (bvnot (bvshl a $e1)) (bvnot (bvlshr a (bvadd (bvnot $e1) #b00001001))))))) |}]

  let%expect_test "Bv.rotate_lefti" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" in
    Term.pp std_formatter @@ Term.Bv.rotate_lefti a 3;
    [%expect {| (concat ((_ extract 4 0) a) ((_ extract 7 5) a)) |}]

  let%expect_test "Bv.rotate_right" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.rotate_right a b;
    [%expect {| (let (($e1 (bvurem b #b00001000))) (ite (= $e1 #b00000000) a (bvnot (bvand (bvnot (bvshl a (bvadd (bvnot $e1) #b00001001))) (bvnot (bvlshr a $e1)))))) |}]

  let%expect_test "Bv.rotate_righti" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" in
    Term.pp std_formatter @@ Term.Bv.rotate_righti a 3;
    [%expect {| (concat ((_ extract 2 0) a) ((_ extract 7 3) a)) |}]

  let%expect_test "Bv.zero_extend" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" in
    Term.pp std_formatter @@ Term.Bv.zero_extend 4 a;
    [%expect {| (concat #b0000 a) |}]

  let%expect_test "Bv.sign_extend" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" in
    Term.pp std_formatter @@ Term.Bv.sign_extend 4 a;
    [%expect {| (concat (ite (= #b1 ((_ extract 7 7) a)) #b1111 #b0000) a) |}]

  let%expect_test "Bv.append" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.append a b;
    [%expect {| (concat a b) |}]

  let%expect_test "Bv.concat" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b"
    and c = Term.const bv8 "c" in
    Term.pp std_formatter @@ Term.Bv.concat [| a; b; c |];
    [%expect {| (concat (concat a b) c) |}]

  let%expect_test "Bv.repeat" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" in
    Term.pp std_formatter @@ Term.Bv.repeat 4 a;
    [%expect {| (concat (concat (concat a a) a) a) |}]

  let%expect_test "Bv.extract" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" in
    Term.pp std_formatter @@ Term.Bv.extract ~hi:3 ~lo:2 a;
    [%expect {| ((_ extract 3 2) a) |}]

  let%expect_test "Bv.sge" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.sge a b;
    [%expect {| (not (bvslt a b)) |}]

  let%expect_test "Bv.uge" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.uge a b;
    [%expect {| (not (bvult a b)) |}]

  let%expect_test "Bv.sgt" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.sgt a b;
    [%expect {| (bvslt b a) |}]

  let%expect_test "Bv.ugt" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.ugt a b;
    [%expect {| (bvult b a) |}]


  let%expect_test "Bv.sle" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.sle a b;
    [%expect {| (not (bvslt b a)) |}]

  let%expect_test "Bv.ule" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.ule a b;
    [%expect {| (not (bvult b a)) |}]

  let%expect_test "Bv.slt" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.slt a b;
    [%expect {| (bvslt a b) |}]

  let%expect_test "Bv.ult" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
    Term.pp std_formatter @@ Term.Bv.ult a b;
    [%expect {| (bvult a b) |}]

  let%test "Bv.size" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" in
    Sort.size @@ Term.sort a = 8

  let%expect_test "Fp.pos_zero" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    Term.pp std_formatter @@ Term.Fp.pos_zero fp16;
    [%expect {| (fp #b0 #b00000 #b0000000000) |}]

  let%expect_test "Fp.neg_zero" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    Term.pp std_formatter @@ Term.Fp.neg_zero fp16;
    [%expect {| (fp #b1 #b00000 #b0000000000) |}]

  let%expect_test "Fp.pos_inf" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    Term.pp std_formatter @@ Term.Fp.pos_inf fp16;
    [%expect {| (fp #b0 #b11111 #b0000000000) |}]

  let%expect_test "Fp.neg_inf" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    Term.pp std_formatter @@ Term.Fp.neg_inf fp16;
    [%expect {| (fp #b1 #b11111 #b0000000000) |}]

  let%expect_test "Fp.nan" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    Term.pp std_formatter @@ Term.Fp.nan fp16;
    [%expect {| (fp #b0 #b11111 #b1000000000) |}]

  let%expect_test "Fp.make" =
    let open Once () in
    let bv1 = Sort.bv 1 and bv5 = Sort.bv 5 and bv10 = Sort.bv 10 in
    let sign = Term.Bv.one bv1
    and exponent = Term.Bv.ones bv5 and significand = Term.Bv.zero bv10 in
    Term.pp std_formatter @@ Term.Fp.make ~sign ~exponent significand;
    [%expect {| (fp #b1 #b11111 #b0000000000) |}]

  let%expect_test "Fp.of_real" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    Term.pp std_formatter @@ Term.Fp.of_real fp16 Term.Rm.Rtz "10.1";
    [%expect {| (fp #b0 #b10010 #b0100001100) |}]

  let%expect_test "Fp.of_rational" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    Term.pp std_formatter @@ Term.Fp.of_rational fp16 Term.Rm.Rtz ~num:"101" ~den:"10";
    [%expect {| (fp #b0 #b10010 #b0100001100) |}]

  let%expect_test "Fp.of_float" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    Term.pp std_formatter @@ Term.Fp.of_float fp16 Term.Rm.Rtz 10.1;
    [%expect {| (fp #b0 #b10010 #b0100001100) |}]

  let%expect_test "Fp.assignment" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    print_float @@ Term.Fp.assignment Term.Rm.Rtz
    @@ get_value @@ Term.Fp.of_float fp16 Term.Rm.Rtz 42.;
    [%expect {| 42. |}]

  let%expect_test "Fp.term" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" in
    Term.pp std_formatter @@
    Term.Fp.term Term.Fp.From_sbv ~exponent:5 16 Term.Rm.rtz a;
    [%expect {| ((_ to_fp 5 11) RTZ a) |}]

  let%expect_test "Fp.term" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" in
    Term.pp std_formatter @@
    Term.Fp.term Term.Fp.From_ubv ~exponent:5 16 Term.Rm.rtz a;
    [%expect {| ((_ to_fp_unsigned 5 11) RTZ a) |}]

  let%expect_test "Fp.term" =
    let open Once () in
    let bv16 = Sort.bv 16 in
    let a = Term.const bv16 "a" in
    Term.pp std_formatter @@ Term.Fp.term Term.Fp.From_bv ~exponent:5 16 a;
    [%expect {| ((_ to_fp 5 11) a) |}]

  let%expect_test "Fp.term" =
    let open Once () in
    let fp32 = Sort.fp ~exponent:8 32 in
    let a = Term.const fp32 "a" in
    Term.pp std_formatter @@
    Term.Fp.term Term.Fp.From_fp ~exponent:5 16 Term.Rm.rtz a;
    [%expect {| ((_ to_fp 5 11) RTZ a) |}]

  let%expect_test "Fp.term" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    let a = Term.const fp16 "a" in
    Term.pp std_formatter @@ Term.Fp.term Term.Fp.Abs a;
    [%expect {| (fp.abs a) |}]

  let%expect_test "Fp.term" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    let a = Term.const fp16 "a" in
    Term.pp std_formatter @@ Term.Fp.term Term.Fp.Neg a;
    [%expect {| (fp.neg a) |}]

  let%expect_test "Fp.term" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    let a = Term.const fp16 "a" and b = Term.const fp16 "b" in
    Term.pp std_formatter @@ Term.Fp.term Term.Fp.Add Term.Rm.rtz a b;
    [%expect {| (fp.add RTZ a b) |}]

  let%expect_test "Fp.term" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    let a = Term.const fp16 "a" and b = Term.const fp16 "b" in
    Term.pp std_formatter @@ Term.Fp.term Term.Fp.Sub Term.Rm.rtz a b;
    [%expect {| (fp.add RTZ a (fp.neg b)) |}]

  let%expect_test "Fp.term" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    let a = Term.const fp16 "a" and b = Term.const fp16 "b" in
    Term.pp std_formatter @@ Term.Fp.term Term.Fp.Mul Term.Rm.rtz a b;
    [%expect {| (fp.mul RTZ a b) |}]

  let%expect_test "Fp.term" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    let a = Term.const fp16 "a" and b = Term.const fp16 "b" in
    Term.pp std_formatter @@ Term.Fp.term Term.Fp.Div Term.Rm.rtz a b;
    [%expect {| (fp.div RTZ a b) |}]

  let%expect_test "Fp.term" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    let a = Term.const fp16 "a" and b = Term.const fp16 "b"
    and c = Term.const fp16 "c" in
    Term.pp std_formatter @@ Term.Fp.term Term.Fp.Fma Term.Rm.rtz a b c;
    [%expect {| (fp.fma RTZ a b c) |}]

  let%expect_test "Fp.term" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    let a = Term.const fp16 "a" in
    Term.pp std_formatter @@ Term.Fp.term Term.Fp.Sqrt Term.Rm.rtz a;
    [%expect {| (fp.sqrt RTZ a) |}]

  let%expect_test "Fp.term" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    let a = Term.const fp16 "a" and b = Term.const fp16 "b" in
    Term.pp std_formatter @@ Term.Fp.term Term.Fp.Rem a b;
    [%expect {| (fp.rem a b) |}]

  let%expect_test "Fp.term" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    let a = Term.const fp16 "a" in
    Term.pp std_formatter @@ Term.Fp.term Term.Fp.Rti Term.Rm.rtz a;
    [%expect {| (fp.roundToIntegral RTZ a) |}]

  let%expect_test "Fp.term" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    let a = Term.const fp16 "a" and b = Term.const fp16 "b" in
    Term.pp std_formatter @@ Term.Fp.term Term.Fp.Min a b;
    [%expect {| (fp.min a b) |}]

  let%expect_test "Fp.term" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    let a = Term.const fp16 "a" and b = Term.const fp16 "b" in
    Term.pp std_formatter @@ Term.Fp.term Term.Fp.Max a b;
    [%expect {| (fp.max a b) |}]

  let%expect_test "Fp.term" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    let a = Term.const fp16 "a" and b = Term.const fp16 "b" in
    Term.pp std_formatter @@ Term.Fp.term Term.Fp.Leq a b;
    [%expect {| (fp.leq a b) |}]

  let%expect_test "Fp.term" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    let a = Term.const fp16 "a" and b = Term.const fp16 "b" in
    Term.pp std_formatter @@ Term.Fp.term Term.Fp.Lt a b;
    [%expect {| (fp.lt a b) |}]

  let%expect_test "Fp.term" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    let a = Term.const fp16 "a" and b = Term.const fp16 "b" in
    Term.pp std_formatter @@ Term.Fp.term Term.Fp.Geq a b;
    [%expect {| (fp.leq b a) |}]

  let%expect_test "Fp.term" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    let a = Term.const fp16 "a" and b = Term.const fp16 "b" in
    Term.pp std_formatter @@ Term.Fp.term Term.Fp.Gt a b;
    [%expect {| (fp.lt b a) |}]

  let%expect_test "Fp.term" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    let a = Term.const fp16 "a" and b = Term.const fp16 "b" in
    Term.pp std_formatter @@ Term.Fp.term Term.Fp.Eq a b;
    [%expect {| (and (not (fp.isNaN b)) (not (fp.isNaN a)) (not (and (not (and (fp.isZero b) (fp.isZero a))) (not (= a b))))) |}]

  let%expect_test "Fp.term" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    let a = Term.const fp16 "a" in
    Term.pp std_formatter @@ Term.Fp.term Term.Fp.Is_normal a;
    [%expect {| (fp.isNormal a) |}]

  let%expect_test "Fp.term" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    let a = Term.const fp16 "a" in
    Term.pp std_formatter @@ Term.Fp.term Term.Fp.Is_subnormal a;
    [%expect {| (fp.isSubnormal a) |}]

  let%expect_test "Fp.term" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    let a = Term.const fp16 "a" in
    Term.pp std_formatter @@ Term.Fp.term Term.Fp.Is_zero a;
    [%expect {| (fp.isZero a) |}]

  let%expect_test "Fp.term" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    let a = Term.const fp16 "a" in
    Term.pp std_formatter @@ Term.Fp.term Term.Fp.Is_inf a;
    [%expect {| (fp.isInfinite a) |}]

  let%expect_test "Fp.term" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    let a = Term.const fp16 "a" in
    Term.pp std_formatter @@ Term.Fp.term Term.Fp.Is_nan a;
    [%expect {| (fp.isNaN a) |}]

  let%expect_test "Fp.term" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    let a = Term.const fp16 "a" in
    Term.pp std_formatter @@ Term.Fp.term Term.Fp.Is_neg a;
    [%expect {| (fp.isNeg a) |}]

  let%expect_test "Fp.term" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    let a = Term.const fp16 "a" in
    Term.pp std_formatter @@ Term.Fp.term Term.Fp.Is_pos a;
    [%expect {| (fp.isPositive a) |}]

  let%expect_test "Fp.term" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    let a = Term.const fp16 "a" in
    Term.pp std_formatter @@ Term.Fp.term Term.Fp.To_sbv 8 Term.Rm.rtz a;
    [%expect {| ((_ fp.to_sbv 8) RTZ a) |}]

  let%expect_test "Fp.term" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    let a = Term.const fp16 "a" in
    Term.pp std_formatter @@ Term.Fp.term Term.Fp.To_ubv 8 Term.Rm.rtz a;
    [%expect {| ((_ fp.to_ubv 8) RTZ a) |}]

  let%expect_test "Fp.of_sbv" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" in
    Term.pp std_formatter @@ Term.Fp.of_sbv ~exponent:5 16 Term.Rm.rtz a;
    [%expect {| ((_ to_fp 5 11) RTZ a) |}]

  let%expect_test "Fp.of_ubv" =
    let open Once () in
    let bv8 = Sort.bv 8 in
    let a = Term.const bv8 "a" in
    Term.pp std_formatter @@ Term.Fp.of_ubv ~exponent:5 16 Term.Rm.rtz a;
    [%expect {| ((_ to_fp_unsigned 5 11) RTZ a) |}]

  let%expect_test "Fp.of_bv" =
    let open Once () in
    let bv16 = Sort.bv 16 in
    let a = Term.const bv16 "a" in
    Term.pp std_formatter @@ Term.Fp.of_bv ~exponent:5 16 a;
    [%expect {| ((_ to_fp 5 11) a) |}]

  let%expect_test "Fp.of_fp" =
    let open Once () in
    let fp32 = Sort.fp ~exponent:8 32 in
    let a = Term.const fp32 "a" in
    Term.pp std_formatter @@ Term.Fp.of_fp ~exponent:5 16 Term.Rm.rtz a;
    [%expect {| ((_ to_fp 5 11) RTZ a) |}]

  let%expect_test "Fp.abs" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    let a = Term.const fp16 "a" in
    Term.pp std_formatter @@ Term.Fp.abs a;
    [%expect {| (fp.abs a) |}]

  let%expect_test "Fp.neg" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    let a = Term.const fp16 "a" in
    Term.pp std_formatter @@ Term.Fp.neg a;
    [%expect {| (fp.neg a) |}]

  let%expect_test "Fp.add" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    let a = Term.const fp16 "a" and b = Term.const fp16 "b" in
    Term.pp std_formatter @@ Term.Fp.add Term.Rm.rtz a b;
    [%expect {| (fp.add RTZ a b) |}]

  let%expect_test "Fp.sub" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    let a = Term.const fp16 "a" and b = Term.const fp16 "b" in
    Term.pp std_formatter @@ Term.Fp.sub Term.Rm.rtz a b;
    [%expect {| (fp.add RTZ a (fp.neg b)) |}]

  let%expect_test "Fp.mul" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    let a = Term.const fp16 "a" and b = Term.const fp16 "b" in
    Term.pp std_formatter @@ Term.Fp.mul Term.Rm.rtz a b;
    [%expect {| (fp.mul RTZ a b) |}]

  let%expect_test "Fp.div" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    let a = Term.const fp16 "a" and b = Term.const fp16 "b" in
    Term.pp std_formatter @@ Term.Fp.div Term.Rm.rtz a b;
    [%expect {| (fp.div RTZ a b) |}]

  let%expect_test "Fp.fma" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    let a = Term.const fp16 "a" and b = Term.const fp16 "b"
    and c = Term.const fp16 "c" in
    Term.pp std_formatter @@ Term.Fp.fma Term.Rm.rtz a b c;
    [%expect {| (fp.fma RTZ a b c) |}]

  let%expect_test "Fp.sqrt" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    let a = Term.const fp16 "a" in
    Term.pp std_formatter @@ Term.Fp.sqrt Term.Rm.rtz a;
    [%expect {| (fp.sqrt RTZ a) |}]

  let%expect_test "Fp.rem" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    let a = Term.const fp16 "a" and b = Term.const fp16 "b" in
    Term.pp std_formatter @@ Term.Fp.rem a b;
    [%expect {| (fp.rem a b) |}]

  let%expect_test "Fp.rti" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    let a = Term.const fp16 "a" in
    Term.pp std_formatter @@ Term.Fp.rti Term.Rm.rtz a;
    [%expect {| (fp.roundToIntegral RTZ a) |}]

  let%expect_test "Fp.min" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    let a = Term.const fp16 "a" and b = Term.const fp16 "b" in
    Term.pp std_formatter @@ Term.Fp.min a b;
    [%expect {| (fp.min a b) |}]

  let%expect_test "Fp.max" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    let a = Term.const fp16 "a" and b = Term.const fp16 "b" in
    Term.pp std_formatter @@ Term.Fp.max a b;
    [%expect {| (fp.max a b) |}]

  let%expect_test "Fp.leq" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    let a = Term.const fp16 "a" and b = Term.const fp16 "b" in
    Term.pp std_formatter @@ Term.Fp.leq a b;
    [%expect {| (fp.leq a b) |}]

  let%expect_test "Fp.lt" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    let a = Term.const fp16 "a" and b = Term.const fp16 "b" in
    Term.pp std_formatter @@ Term.Fp.lt a b;
    [%expect {| (fp.lt a b) |}]

  let%expect_test "Fp.geq" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    let a = Term.const fp16 "a" and b = Term.const fp16 "b" in
    Term.pp std_formatter @@ Term.Fp.geq a b;
    [%expect {| (fp.leq b a) |}]

  let%expect_test "Fp.gt" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    let a = Term.const fp16 "a" and b = Term.const fp16 "b" in
    Term.pp std_formatter @@ Term.Fp.gt a b;
    [%expect {| (fp.lt b a) |}]

  let%expect_test "Fp.eq" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    let a = Term.const fp16 "a" and b = Term.const fp16 "b" in
    Term.pp std_formatter @@ Term.Fp.eq a b;
    [%expect {| (and (not (fp.isNaN b)) (not (fp.isNaN a)) (not (and (not (and (fp.isZero b) (fp.isZero a))) (not (= a b))))) |}]

  let%expect_test "Fp.is_normal" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    let a = Term.const fp16 "a" in
    Term.pp std_formatter @@ Term.Fp.is_normal a;
    [%expect {| (fp.isNormal a) |}]

  let%expect_test "Fp.is_subnormal" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    let a = Term.const fp16 "a" in
    Term.pp std_formatter @@ Term.Fp.is_subnormal a;
    [%expect {| (fp.isSubnormal a) |}]

  let%expect_test "Fp.is_zero" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    let a = Term.const fp16 "a" in
    Term.pp std_formatter @@ Term.Fp.is_zero a;
    [%expect {| (fp.isZero a) |}]

  let%expect_test "Fp.is_infinite" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    let a = Term.const fp16 "a" in
    Term.pp std_formatter @@ Term.Fp.is_infinite a;
    [%expect {| (fp.isInfinite a) |}]

  let%expect_test "Fp.is_nan" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    let a = Term.const fp16 "a" in
    Term.pp std_formatter @@ Term.Fp.is_nan a;
    [%expect {| (fp.isNaN a) |}]

  let%expect_test "Fp.is_negative" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    let a = Term.const fp16 "a" in
    Term.pp std_formatter @@ Term.Fp.is_negative a;
    [%expect {| (fp.isNeg a) |}]

  let%expect_test "Fp.is_positive" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    let a = Term.const fp16 "a" in
    Term.pp std_formatter @@ Term.Fp.is_positive a;
    [%expect {| (fp.isPositive a) |}]

  let%expect_test "Fp.to_sbv" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    let a = Term.const fp16 "a" in
    Term.pp std_formatter @@ Term.Fp.to_sbv 8 Term.Rm.rtz a;
    [%expect {| ((_ fp.to_sbv 8) RTZ a) |}]

  let%expect_test "Fp.to_ubv" =
    let open Once () in
    let fp16 = Sort.fp ~exponent:5 16 in
    let a = Term.const fp16 "a" in
    Term.pp std_formatter @@ Term.Fp.to_ubv 8 Term.Rm.rtz a;
    [%expect {| ((_ fp.to_ubv 8) RTZ a) |}]

  let%expect_test "Rm.rne" =
    let open Once () in
    Term.pp std_formatter @@ Term.Rm.rne;
    [%expect {| RNE |}]

  let%expect_test "Rm.rna" =
    let open Once () in
    Term.pp std_formatter @@ Term.Rm.rna;
    [%expect {| RNA |}]

  let%expect_test "Rm.rtn" =
    let open Once () in
    Term.pp std_formatter @@ Term.Rm.rtn;
    [%expect {| RTN |}]

  let%expect_test "Rm.rtp" =
    let open Once () in
    Term.pp std_formatter @@ Term.Rm.rtp;
    [%expect {| RTP |}]

  let%expect_test "Rm.rtz" =
    let open Once () in
    Term.pp std_formatter @@ Term.Rm.rtz;
    [%expect {| RTZ |}]

  let%expect_test "Ar.make" =
    let open Once () in
    let bv8 = Sort.bv 8 and bv32 = Sort.bv 32 in
    let ar32_8 = Sort.ar bv32 bv8 in
    Term.pp std_formatter @@ Term.Ar.make ar32_8 @@ Term.Bv.zero bv8;
    [%expect {| ((as const ((_ BitVec 32)) (_ BitVec 8)) #b00000000) |}]

  let%expect_test "Ar.select" =
    let open Once () in
    let bv8 = Sort.bv 8 and bv32 = Sort.bv 32 in
    let ar32_8 = Sort.ar bv32 bv8 in
    let a = Term.const ar32_8 "a" and b = Term.const bv32 "b" in
    Term.pp std_formatter @@ Term.Ar.select a b;
    [%expect {| (select a b) |}]

  let%expect_test "Ar.store" =
    let open Once () in
    let bv8 = Sort.bv 8 and bv32 = Sort.bv 32 in
    let ar32_8 = Sort.ar bv32 bv8 in
    let a = Term.const ar32_8 "a" and b = Term.const bv32 "b"
    and c = Term.const bv8 "c" in
    Term.pp std_formatter @@ Term.Ar.store a b c;
    [%expect {| (store a b c) |}]

  let%expect_test "Sort.fn" =
    let open Once () in
    let bv1 = Sort.bool and fp16 = Sort.fp ~exponent:5 16
    and rm = Sort.rm and bv8 = Sort.bv 8 in
    Sort.pp std_formatter @@ Sort.fn [ bv1; fp16; rm ] bv8;
    [%expect {| ((_ BitVec 1) (_ FloatingPoint 5 11) RoundingMode) (_ BitVec 8) |}]

  let%expect_test "Uf.lambda" =
    let open Once () in
    let bv1 = Sort.bool and fp16 = Sort.fp ~exponent:5 16 and rm = Sort.rm in
    begin
      Term.pp std_formatter @@
      Term.Uf.lambda [ bv1; fp16; rm ] @@ fun [ sign; fp; rm ] ->
      Term.ite sign (Term.Fp.to_sbv 8 rm fp) (Term.Fp.to_ubv 8 rm fp)
    end;
    [%expect {|
      (define-fun f1 ((p2 (_ BitVec 1)) (p3 (_ FloatingPoint 5 11)) (p4 RoundingMode)) (_ BitVec 8)
      (ite (= #b1 p2) ((_ fp.to_sbv 8) p4 p3) ((_ fp.to_ubv 8) p4 p3))) |}]

  let%expect_test "Uf.apply" =
    let open Once () in
    let bv1 = Sort.bool and fp16 = Sort.fp ~exponent:5 16 and rm = Sort.rm in
    let fn =
      Term.Uf.lambda [ bv1; fp16; rm ] @@ fun [ sign; fp; rm; ] ->
      Term.ite sign (Term.Fp.to_sbv 8 rm fp) (Term.Fp.to_ubv 8 rm fp) in
    let sign = Term.const bv1 "sign" and value = Term.const fp16 "value"
    and rm = Term.const rm "rm" in
    Term.pp std_formatter @@
    Term.Uf.apply fn [ sign; value; rm ];
    [%expect {| (f1 sign value rm) |}]

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let bv8 = Sort.bv 8 in
        let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
        let e = Term.equal a b in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let bv8 = Sort.bv 8 in
        let a = Term.const bv8 "a" in
        let e = Term.Bv.pred a in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let bv8 = Sort.bv 8 in
        let a = Term.const bv8 "a" in
        let e = Term.Bv.succ a in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let bv8 = Sort.bv 8 in
        let a = Term.const bv8 "a" in
        let e = Term.Bv.term Term.Bv.Neg a in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let bv8 = Sort.bv 8 in
        let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
        let e = Term.Bv.term Term.Bv.Add a b in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let bv8 = Sort.bv 8 in
        let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
        let e = Term.Bv.sadd_overflow a b in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let bv8 = Sort.bv 8 in
        let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
        let e = Term.Bv.uadd_overflow a b in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let bv8 = Sort.bv 8 in
        let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
        let e = Term.Bv.term Term.Bv.Sub a b in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let bv8 = Sort.bv 8 in
        let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
        let e = Term.Bv.ssub_overflow a b in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let bv8 = Sort.bv 8 in
        let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
        let e = Term.Bv.usub_overflow a b in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let bv8 = Sort.bv 8 in
        let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
        let e = Term.Bv.term Term.Bv.Mul a b in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let bv8 = Sort.bv 8 in
        let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
        let e = Term.Bv.smul_overflow a b in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let bv8 = Sort.bv 8 in
        let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
        let e = Term.Bv.umul_overflow a b in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let bv8 = Sort.bv 8 in
        let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
        let e = Term.Bv.term Term.Bv.Sdiv a b in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let bv8 = Sort.bv 8 in
        let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
        let e = Term.Bv.sdiv_overflow a b in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let bv8 = Sort.bv 8 in
        let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
        let e = Term.Bv.term Term.Bv.Udiv a b in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let bv8 = Sort.bv 8 in
        let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
        let e = Term.Bv.term Term.Bv.Smod a b in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let bv8 = Sort.bv 8 in
        let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
        let e = Term.Bv.term Term.Bv.Srem a b in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let bv8 = Sort.bv 8 in
        let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
        let e = Term.Bv.term Term.Bv.Urem a b in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let bv8 = Sort.bv 8 in
        let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
        let e = Term.Bv.term Term.Bv.And a b in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let bv8 = Sort.bv 8 in
        let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
        let e = Term.Bv.lognand a b in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let bv8 = Sort.bv 8 in
        let a = Term.const bv8 "a" in
        let e = Term.Bv.redand a in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let bv8 = Sort.bv 8 in
        let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
        let e = Term.Bv.term Term.Bv.Or a b in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let bv8 = Sort.bv 8 in
        let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
        let e = Term.Bv.lognor a b in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let bv8 = Sort.bv 8 in
        let a = Term.const bv8 "a" in
        let e = Term.Bv.redor a in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let bv8 = Sort.bv 8 in
        let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
        let e = Term.Bv.term Term.Bv.Xor a b in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let bv8 = Sort.bv 8 in
        let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
        let e = Term.Bv.logxnor a b in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let bv8 = Sort.bv 8 in
        let a = Term.const bv8 "a" in
        let e = Term.Bv.redxor a in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let bv8 = Sort.bv 8 in
        let a = Term.const bv8 "a" in
        let e = Term.Bv.term Term.Bv.Not a in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let bv8 = Sort.bv 8 in
        let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
        let e = Term.Bv.term Term.Bv.Shl a b in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let bv8 = Sort.bv 8 in
        let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
        let e = Term.Bv.term Term.Bv.Ashr a b in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let bv8 = Sort.bv 8 in
        let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
        let e = Term.Bv.term Term.Bv.Shr a b in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let bv8 = Sort.bv 8 in
        let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
        let e = Term.Bv.term Term.Bv.Rol a b in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let bv8 = Sort.bv 8 in
        let a = Term.const bv8 "a" in
        let e = Term.Bv.rotate_lefti a 3 in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let bv8 = Sort.bv 8 in
        let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
        let e = Term.Bv.term Term.Bv.Ror a b in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let bv8 = Sort.bv 8 in
        let a = Term.const bv8 "a" in
        let e = Term.Bv.rotate_righti a 3 in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let bv8 = Sort.bv 8 in
        let a = Term.const bv8 "a" in
        let e = Term.Bv.zero_extend 4 a in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let bv8 = Sort.bv 8 in
        let a = Term.const bv8 "a" in
        let e = Term.Bv.sign_extend 4 a in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let bv8 = Sort.bv 8 in
        let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
        let e = Term.Bv.term Term.Bv.Concat a b in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let bv8 = Sort.bv 8 in
        let a = Term.const bv8 "a" in
        let e = Term.Bv.repeat 4 a in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let bv8 = Sort.bv 8 in
        let a = Term.const bv8 "a" in
        let e = Term.Bv.term Term.Bv.Extract ~hi:3 ~lo:2 a in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let bv8 = Sort.bv 8 in
        let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
        let e = Term.Bv.term Term.Bv.Sge a b in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let bv8 = Sort.bv 8 in
        let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
        let e = Term.Bv.term Term.Bv.Uge a b in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let bv8 = Sort.bv 8 in
        let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
        let e = Term.Bv.term Term.Bv.Sgt a b in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let bv8 = Sort.bv 8 in
        let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
        let e = Term.Bv.term Term.Bv.Ugt a b in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)


  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let bv8 = Sort.bv 8 in
        let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
        let e = Term.Bv.term Term.Bv.Sle a b in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let bv8 = Sort.bv 8 in
        let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
        let e = Term.Bv.term Term.Bv.Ule a b in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let bv8 = Sort.bv 8 in
        let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
        let e = Term.Bv.term Term.Bv.Slt a b in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let bv8 = Sort.bv 8 in
        let a = Term.const bv8 "a" and b = Term.const bv8 "b" in
        let e = Term.Bv.term Term.Bv.Ult a b in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let bv8 = Sort.bv 8 in
        let a = Term.const bv8 "a" in
        let e = Term.Fp.term Term.Fp.From_sbv ~exponent:5 16 Term.Rm.rtz a in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let bv8 = Sort.bv 8 in
        let a = Term.const bv8 "a" in
        let e = Term.Fp.term Term.Fp.From_ubv ~exponent:5 16 Term.Rm.rtz a in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let bv16 = Sort.bv 16 in
        let a = Term.const bv16 "a" in
        let e =  Term.Fp.term Term.Fp.From_bv ~exponent:5 16 a in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let fp32 = Sort.fp ~exponent:8 32 in
        let a = Term.const fp32 "a" in
        let e = Term.Fp.term Term.Fp.From_fp ~exponent:5 16 Term.Rm.rtz a in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let fp16 = Sort.fp ~exponent:5 16 in
        let a = Term.const fp16 "a" in
        let e =  Term.Fp.term Term.Fp.Abs a in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let fp16 = Sort.fp ~exponent:5 16 in
        let a = Term.const fp16 "a" in
        let e =  Term.Fp.term Term.Fp.Neg a in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let fp16 = Sort.fp ~exponent:5 16 in
        let a = Term.const fp16 "a" and b = Term.const fp16 "b" in
        let e =  Term.Fp.term Term.Fp.Add Term.Rm.rtz a b in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let fp16 = Sort.fp ~exponent:5 16 in
        let a = Term.const fp16 "a" and b = Term.const fp16 "b" in
        let e =  Term.Fp.term Term.Fp.Sub Term.Rm.rtz a b in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let fp16 = Sort.fp ~exponent:5 16 in
        let a = Term.const fp16 "a" and b = Term.const fp16 "b" in
        let e =  Term.Fp.term Term.Fp.Mul Term.Rm.rtz a b in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let fp16 = Sort.fp ~exponent:5 16 in
        let a = Term.const fp16 "a" and b = Term.const fp16 "b" in
        let e =  Term.Fp.term Term.Fp.Div Term.Rm.rtz a b in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let fp16 = Sort.fp ~exponent:5 16 in
        let a = Term.const fp16 "a" and b = Term.const fp16 "b"
        and c = Term.const fp16 "c" in
        let e =  Term.Fp.term Term.Fp.Fma Term.Rm.rtz a b c in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let fp16 = Sort.fp ~exponent:5 16 in
        let a = Term.const fp16 "a" in
        let e =  Term.Fp.term Term.Fp.Sqrt Term.Rm.rtz a in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let fp16 = Sort.fp ~exponent:5 16 in
        let a = Term.const fp16 "a" and b = Term.const fp16 "b" in
        let e =  Term.Fp.term Term.Fp.Rem a b in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let fp16 = Sort.fp ~exponent:5 16 in
        let a = Term.const fp16 "a" in
        let e =  Term.Fp.term Term.Fp.Rti Term.Rm.rtz a in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let fp16 = Sort.fp ~exponent:5 16 in
        let a = Term.const fp16 "a" and b = Term.const fp16 "b" in
        let e =  Term.Fp.term Term.Fp.Min a b in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let fp16 = Sort.fp ~exponent:5 16 in
        let a = Term.const fp16 "a" and b = Term.const fp16 "b" in
        let e =  Term.Fp.term Term.Fp.Max a b in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let fp16 = Sort.fp ~exponent:5 16 in
        let a = Term.const fp16 "a" and b = Term.const fp16 "b" in
        let e =  Term.Fp.term Term.Fp.Leq a b in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let fp16 = Sort.fp ~exponent:5 16 in
        let a = Term.const fp16 "a" and b = Term.const fp16 "b" in
        let e =  Term.Fp.term Term.Fp.Lt a b in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let fp16 = Sort.fp ~exponent:5 16 in
        let a = Term.const fp16 "a" and b = Term.const fp16 "b" in
        let e =  Term.Fp.term Term.Fp.Geq a b in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let fp16 = Sort.fp ~exponent:5 16 in
        let a = Term.const fp16 "a" and b = Term.const fp16 "b" in
        let e =  Term.Fp.term Term.Fp.Gt a b in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let fp16 = Sort.fp ~exponent:5 16 in
        let a = Term.const fp16 "a" and b = Term.const fp16 "b" in
        let e =  Term.Fp.term Term.Fp.Eq a b in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let fp16 = Sort.fp ~exponent:5 16 in
        let a = Term.const fp16 "a" in
        let e =  Term.Fp.term Term.Fp.Is_normal a in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let fp16 = Sort.fp ~exponent:5 16 in
        let a = Term.const fp16 "a" in
        let e =  Term.Fp.term Term.Fp.Is_subnormal a in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let fp16 = Sort.fp ~exponent:5 16 in
        let a = Term.const fp16 "a" in
        let e =  Term.Fp.term Term.Fp.Is_zero a in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let fp16 = Sort.fp ~exponent:5 16 in
        let a = Term.const fp16 "a" in
        let e =  Term.Fp.term Term.Fp.Is_inf a in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let fp16 = Sort.fp ~exponent:5 16 in
        let a = Term.const fp16 "a" in
        let e =  Term.Fp.term Term.Fp.Is_nan a in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let fp16 = Sort.fp ~exponent:5 16 in
        let a = Term.const fp16 "a" in
        let e =  Term.Fp.term Term.Fp.Is_neg a in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let fp16 = Sort.fp ~exponent:5 16 in
        let a = Term.const fp16 "a" in
        let e =  Term.Fp.term Term.Fp.Is_pos a in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let fp16 = Sort.fp ~exponent:5 16 in
        let a = Term.const fp16 "a" in
        let e =  Term.Fp.term Term.Fp.To_sbv 8 Term.Rm.rtz a in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let bv8 = Sort.bv 8 and bv32 = Sort.bv 32 in
        let ar32_8 = Sort.ar bv32 bv8 in
        let e = Term.Ar.make ar32_8 @@ Term.Bv.zero bv8 in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let bv8 = Sort.bv 8 and bv32 = Sort.bv 32 in
        let ar32_8 = Sort.ar bv32 bv8 in
        let a = Term.const ar32_8 "a" and b = Term.const bv32 "b" in
        let e = Term.Ar.select a b in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let bv8 = Sort.bv 8 and bv32 = Sort.bv 32 in
        let ar32_8 = Sort.ar bv32 bv8 in
        let a = Term.const ar32_8 "a" and b = Term.const bv32 "b"
        and c = Term.const bv8 "c" in
        let e = Term.Ar.store a b c in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let fp16 = Sort.fp ~exponent:5 16 in
        let a = Term.const fp16 "a" in
        let e =  Term.Fp.term Term.Fp.To_ubv 8 Term.Rm.rtz a in
        let e' = copy e in
        assert' @@ Term.distinct e e';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let bv1 = Sort.bool and fp16 = Sort.fp ~exponent:5 16
        and rm = Sort.rm in
        let e =
          Term.Uf.lambda [ bv1; fp16; rm ] @@ fun [ sign; fp; rm ] ->
          Term.ite sign (Term.Fp.to_sbv 8 rm fp) (Term.Fp.to_ubv 8 rm fp) in
        let e' = copy e in
        let sign = Term.const bv1 "sign" and value = Term.const fp16 "value"
        and rm = Term.const rm "rm" in
        let a = Term.Uf.apply e [ sign; value; rm ] in
        let a' = Term.Uf.apply e' [ sign; value; rm ] in
        assert' @@ Term.distinct a a';
        check_sat () = Unsat)

  let%test "view" =
    with_debug (fun m ->
        let open (val m : Debug) in
        let bv1 = Sort.bool and bv8 = Sort.bv 8
        and fp16 = Sort.fp ~exponent:5 16 and rm = Sort.rm in
        let fn = Sort.fn [ bv1; fp16; rm ] bv8 in
        let e = Term.const fn "a" in
        let sign = Term.const bv1 "sign" and value = Term.const fp16 "value"
        and rm = Term.const rm "rm" in
        let a = Term.Uf.apply e [ sign; value; rm ] in
        let a' = copy a in
        assert' @@ Term.distinct a a';
        check_sat () = Unsat)

end

let%test "check_sat" =
  let open Once () in
  assert' @@ Term.const Sort.bool "a";
  check_sat () = Sat

let%test "check_sat" =
  let open Once () in
  let a = Term.const Sort.bool "a" in
  assert' @@ Term.equal a @@ Term.Bl.lognot a;
  check_sat () = Unsat

let%test "check_sat" =
  let open Once () in
  let bv64 = Sort.bv 64 and bv128 = Sort.bv 128 in
  let a = Term.const bv64 "a" and b = Term.const bv64 "b" in
  let a128 = Term.Bv.zero_extend 64 a and b128 = Term.Bv.zero_extend 64 b in
  let ab128 = Term.Bv.mul a128 b128 in
  let p = Term.Bv.of_string bv128 "0x87e03acc9f5050086f083d2d5d6b9d47" in
  let one128 = Term.Bv.one bv128 in
  assert' @@ Term.distinct a128 one128;
  assert' @@ Term.distinct b128 one128;
  assert' @@ Term.equal ab128 p;
  (timeout 1. check_sat) () = Unknown

module Incremental = struct

  let%test "check_sat" =
    let open Once () in
    let a = Term.const Sort.bool "a" and b = Term.const Sort.bool "b" in
    assert' @@ Term.Bl.implies a b;
    try
      ignore @@ check_sat ();
      ignore @@ check_sat ();
      false
    with Failure _ -> true

  let%test "check_sat" =
    let open Incremental () in
    let a = Term.const Sort.bool "a" and b = Term.const Sort.bool "b" in
    assert' @@ Term.Bl.implies a b;
    try
      ignore @@ check_sat ();
      ignore @@ check_sat ();
      true
    with Failure _ -> false

  let%test "check_sat_assuming" =
    let open Incremental () in
    let a = Term.const Sort.bool "a" and b = Term.const Sort.bool "b" in
    assert' @@ Term.Bl.implies a b;
    check_sat_assuming [| b |] = Sat

end
