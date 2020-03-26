open OUnit2

(* test vectors from "Appendix C. Test Vectors" of OCRA RFC *)

open Rfc6287
open Rresult

let key k =
  let s = match k with
    | `K20 -> "3132333435363738393031323334353637383930"
    | `K32 -> "313233343536373839303132333435363738393031323" ^
              "3343536373839303132"
    | `K64 -> "313233343536373839303132333435363738393031323" ^
              "334353637383930313233343536373839303132333435" ^
              "36373839303132333435363738393031323334" in
  Hex.to_cstruct (`Hex s)

let pinhash_d = `Digest (Mirage_crypto.Hash.SHA1.digest (Cstruct.of_string "1234"))
let pinhash = `String "1234"

let timestamp = `Int64 0x132d0b6L

let suitestring ctx =
  let open OUnitTest in
  string_of_node (List.hd ctx.path)

let suite ctx =
  R.get_ok (t_of_string (suitestring ctx))

let istr l i =
  let c = char_of_int (i + (int_of_char '0')) in
  String.make l c

let assert_cs_eq_s s cs =
  assert_equal s (Cstruct.to_string cs)


let known_answer =
  let o1 ctx =
    let suite, key = suite ctx, key `K20 in
    let l = ["237653"; "243178"; "653583"; "740991"; "608993"; "388898";
             "816933"; "224598"; "750600"; "294470"] in
    List.iteri (fun i r ->
        let q = istr 8 i in
        let o = R.get_ok (gen suite ~key ~q) in
        assert_cs_eq_s r o) l in

  let o2 ctx =
    let suite, key, p, q = suite ctx, key `K32, pinhash, "12345678" in
    let l = ["65347737"; "86775851"; "78192410"; "71565254"; "10104329";
             "65983500"; "70069104"; "91771096"; "75011558"; "08522129"] in
    List.iteri (fun i r ->
        let c = Int64.of_int i in
        let o = R.get_ok (gen suite ~key ~c ~p ~q) in
        assert_cs_eq_s r o) l in

  let o3 ctx =
    let suite, key, p = suite ctx, key `K32, pinhash in
    let l = ["83238735"; "01501458"; "17957585"; "86776967"; "86807031"] in
    List.iteri (fun i r ->
        let q = istr 8 i in
        let o = R.get_ok (gen suite ~key ~p ~q) in
        assert_cs_eq_s r o) l in

  let o4 ctx =
    let suite, key = suite ctx, key `K64 in
    let l = ["07016083"; "63947962"; "70123924"; "25341727"; "33203315";
             "34205738"; "44343969"; "51946085"; "20403879"; "31409299"] in
    List.iteri (fun i r ->
        let c = Int64.of_int i in
        let q = istr 8 i in
        let o = R.get_ok (gen suite ~key ~c ~q) in
        assert_cs_eq_s r o) l in

  let o5 ctx =
    let suite, key, t = suite ctx, key `K64, timestamp in
    let l =["95209754"; "55907591"; "22048402"; "24218844"; "36209546"] in
    List.iteri (fun i r ->
        let q = istr 8 i in
        let o = R.get_ok (gen suite ~key ~t ~q) in
        assert_cs_eq_s r o) l in

  let s1 ctx =
    let suite, key = suite ctx, key `K32 in
    let l = [("SIG10000","53095496");
             ("SIG11000","04110475");
             ("SIG12000","31331128");
             ("SIG13000","76028668");
             ("SIG14000","46554205")] in
    List.iter (fun (q, r) ->
        let o = R.get_ok (gen suite ~key ~q) in
        assert_cs_eq_s r o) l in

  let s2 ctx =
    let suite, key, t = suite ctx, key `K64, timestamp in
    let l = [("SIG1000000","77537423");
             ("SIG1100000","31970405");
             ("SIG1200000","10235557");
             ("SIG1300000","95213541");
             ("SIG1400000","65360607")] in
    List.iter (fun (q, r) ->
        let o = R.get_ok (gen suite ~key ~t ~q) in
        assert_cs_eq_s r o) l in

  let m1 ctx =
    let suite, key = suite ctx, key `K32 in
    let l = [("CLI22220SRV11110","28247970");
             ("CLI22221SRV11111","01984843");
             ("CLI22222SRV11112","65387857");
             ("CLI22223SRV11113","03351211");
             ("CLI22224SRV11114","83412541");
             ("SRV11110CLI22220","15510767");
             ("SRV11111CLI22221","90175646");
             ("SRV11112CLI22222","33777207");
             ("SRV11113CLI22223","95285278");
             ("SRV11114CLI22224","28934924")] in
    List.iter (fun (q, r) ->
        let o = R.get_ok (gen suite ~key ~q) in
        assert_cs_eq_s r o) l in

  let m2 ctx =
    let suite, key = suite ctx, key `K64 in
    let l = [("CLI22220SRV11110","79496648");
             ("CLI22221SRV11111","76831980");
             ("CLI22222SRV11112","12250499");
             ("CLI22223SRV11113","90856481");
             ("CLI22224SRV11114","12761449")] in
    List.iter (fun (q, r) ->
        let o = R.get_ok (gen suite ~key ~q) in
        assert_cs_eq_s r o) l in

  let m3 ctx =
    let suite, key, p = suite ctx, key `K64, pinhash_d in
    let l = [("SRV11110CLI22220","18806276");
             ("SRV11111CLI22221","70020315");
             ("SRV11112CLI22222","01600026");
             ("SRV11113CLI22223","18951020");
             ("SRV11114CLI22224","32528969")] in
    List.iter (fun (q, r) ->
        let o = R.get_ok (gen suite ~key ~p ~q) in
        assert_cs_eq_s r o) l in
  ["one_way" >::: ["OCRA-1:HOTP-SHA1-6:QN08" >::o1;
                   "OCRA-1:HOTP-SHA256-8:C-QN08-PSHA1" >:: o2;
                   "OCRA-1:HOTP-SHA256-8:QN08-PSHA1" >:: o3;
                   "OCRA-1:HOTP-SHA512-8:C-QN08" >:: o4;
                   "OCRA-1:HOTP-SHA512-8:QN08-T1M" >:: o5;];
   "signature" >::: ["OCRA-1:HOTP-SHA256-8:QA08" >::s1;
                     "OCRA-1:HOTP-SHA512-8:QA10-T1M" >:: s2];
   "mutual" >::: ["OCRA-1:HOTP-SHA256-8:QA08" >:: m1;
                  "OCRA-1:HOTP-SHA512-8:QA08" >:: m2;
                  "OCRA-1:HOTP-SHA512-8:QA08-PSHA1" >:: m3]]


let coverage =
  let invalid_suite ctx =
    assert_equal
      Invalid_suite_string (R.get_error (t_of_string (suitestring ctx))) in

  let string_of_t ctx =
    let ss = suitestring ctx in
    let s = suite ctx in
    assert_equal ss (string_of_t s) in

  let di_of_t ctx =
    let s = suite ctx in
    let di = di_of_t s in
    assert_equal true di.c in

  let challenge0 ctx =
    let s = suite ctx in
    let _ = challenge s in () in

  let gen1 x ctx =
    let suite, key, q = suite ctx, key `K32, "6e6ec0469f5ec369a092" in
    assert_equal (R.get_error (gen suite ~key ~q))
      (DataInput ("suite requires " ^ x)) in

  let gen_no_c  ctx =
    let suite, key, q = suite ctx, key `K32, "6e6ec0469f5ec369a092" in
    let c,p,s,t = Some 0x00L,None,None,None in
    assert_equal (R.get_error (Rfc6287.gen1 suite ~key ~q ~c ~p ~s ~t))
      (DataInput ("no C in suite")) in

  let gen_invalid_q  ctx =
    let suite, key, q = suite ctx, key `K32, "xyz" in
    assert_equal (R.get_error (gen suite ~key ~q))
      (DataInput ("invalid Q")) in

  let gen_no_p  ctx =
    let suite, key, q = suite ctx, key `K32, "6e6ec0469f5ec369a092" in
    assert_equal (R.get_error (gen suite ~key ~q ~p:pinhash))
      (DataInput ("no P in suite")) in

  let gen_conflict_p  ctx =
    let suite, key, q = suite ctx, key `K32, "6e6ec0469f5ec369a092" in
    let p = `Digest (Cstruct.create 0) in
    assert_equal (R.get_error (gen suite ~key ~q ~p))
      (DataInput ("P length conflicts suite")) in

  let gen_no_s  ctx =
    let suite, key, q = suite ctx, key `K32, "6e6ec0469f5ec369a092" in
    assert_equal (R.get_error (gen suite ~key ~q ~s:(Cstruct.create 0)))
      (DataInput ("no S in suite")) in

  let gen_conflict_s ctx =
    let suite, key, q = suite ctx, key `K32, "6e6ec0469f5ec369a092" in
    assert_equal (R.get_error (gen suite ~key ~q ~s:(Cstruct.create 0)))
      (DataInput ("S length conflicts suite")) in

  let gen_no_t  ctx =
    let suite, key, q = suite ctx, key `K32, "6e6ec0469f5ec369a092" in
    assert_equal (R.get_error (gen suite ~key ~q ~t:`Now))
      (DataInput ("no T in suite")) in

  let gen_session_data ctx =
    let suite, key = suite ctx, key `K20 in
    let q = "6e6ec0469f5ec369a092" in
    let s = Cstruct.create 235 in
    Cstruct.memset s 0xa5;
    let _ = gen suite ~key ~s ~q in () in

  let verify1 ctx =
    let suite, key, q, a = suite ctx, key `K20, "aaaaaaaaaa", (Cstruct.create 0) in
    let p,tw,cw,s,c,t = None,None,None,None,None,None in
    assert_equal (R.get_error (verify1 suite ~key ~p ~q ~a ~t ~cw ~tw ~s ~c))
      (DataInput ("suite requires T")) in

  let verify2 ctx =
    let suite, key, q, a = suite ctx, key `K20, "aaaaaaaaaa", (Cstruct.create 0) in
    assert_equal (R.get_error (verify suite ~key ~q ~a ~tw:(-1)))
      (Window "invalid timestamp window or no T in suite") in

  let verify3 ctx =
    let suite, key, q, a = suite ctx, key `K20, "aaaaaaaaaa", (Cstruct.create 0) in
    let time = Int64.of_float (Unix.time ()) in
    assert_equal (R.get_error (verify suite ~time ~key ~q ~a ~t:`Now ~tw:5 ~cw:1 ))
      (Window "invalid counter window or no C in suite") in


  ["t_of_string" >::: ["this_is_not_a_suite_string" >:: invalid_suite;
                       "OCRA-1::QA08" >:: invalid_suite;
                       "OCRA-1:::QA08" >:: invalid_suite;
                       "OCRA-1:HOTP-SHA1-0:QN08-T01X" >:: invalid_suite;
                       "OCRA-1:HOTP-SHA1-0:QN08-T91S" >:: invalid_suite;
                       "OCRA-1:HOTP-SHA1-0:QN08-T91H" >:: invalid_suite;
                       "OCRA-1:HOTP-SHA1-0:QN08-T" >:: invalid_suite;
                       "OCRA-1:HOTP-SHA1-0:QH08-S028---" >:: invalid_suite;
                       "OCRA-1:HOTP-SHA1-0:QX08" >:: invalid_suite;
                       "OCRA-1:HOTP-SHA1-0:C-" >:: invalid_suite;
                       "OCRA-1:HOTP-SHA1-0:C" >:: invalid_suite;
                       "OCRA-1:HOTP-SHA1-0:" >:: invalid_suite;
                       "OCRA-1:HOTP-SHA1-1:QA08" >:: invalid_suite;
                       "OCRA-1:HOTP-SHA-0:QN08" >:: invalid_suite;
                       "OCRA-1:HOTP-SHA1-a0:QN08" >:: invalid_suite;
                       "OCRA-1:HTOP-SHA1-0:QA-08" >:: invalid_suite];
   "string_of_t" >::: ["OCRA-1:HOTP-SHA512-0:C-QH16-T14H" >:: string_of_t];
   "di_of_t" >::: ["OCRA-1:HOTP-SHA512-0:C-QH16-T14H" >:: di_of_t];
   "challenge" >::: ["OCRA-1:HOTP-SHA256-0:QA10" >:: challenge0;
                     "OCRA-1:HOTP-SHA256-0:QN10" >:: challenge0;
                     "OCRA-1:HOTP-SHA256-0:QH10" >:: challenge0];
   "gen" >::: ["OCRA-1:HOTP-SHA256-0:C-QH10" >::  gen1 "C" ;
               "OCRA-1:HOTP-SHA256-0:QH10-PSHA1" >:: gen1 "P";
               "OCRA-1:HOTP-SHA256-0:QH10-T1H" >:: gen1 "T";
               "OCRA-1:HOTP-SHA256-0:QH10-S123" >:: gen1 "S";
               "OCRA-1:HOTP-SHA256-0:QH10" >:: gen_no_c;
               "OCRA-1:HOTP-SHA256-0:QH10" >:: gen_invalid_q;
               "OCRA-1:HOTP-SHA256-0:QH10" >:: gen_no_p;
               "OCRA-1:HOTP-SHA256-0:QH10" >:: gen_no_s;
               "OCRA-1:HOTP-SHA256-0:QH10" >:: gen_no_t;
               "OCRA-1:HOTP-SHA256-0:QH10-PSHA1" >:: gen_conflict_p;
               "OCRA-1:HOTP-SHA256-0:QH10-S999" >:: gen_conflict_s;
               "OCRA-1:HOTP-SHA1-0:QH10-S235" >:: gen_session_data;
               "OCRA-1:HOTP-SHA1-10:QH10-S235" >:: gen_session_data];

   "verify" >::: ["OCRA-1:HOTP-SHA1-0:QH10-T1M" >:: verify1;
                  "OCRA-1:HOTP-SHA1-0:QH10" >:: verify2;
                  "OCRA-1:HOTP-SHA1-0:QH10-T1M" >:: verify3;]

  ]

let verify =
  let v1 ctx =
    let suite, key, c = suite ctx, key `K64, 0xffffffffffffffffL in
    let q = challenge suite in
    let a = R.get_ok (gen ~c suite ~key ~q) in
    let v = verify ~c:(Int64.sub c 23L) ~cw:42 suite ~key ~q ~a in
    assert_equal (R.get_ok v) (true, Some (Int64.add c 1L)) in

  let v2 ctx =
    let suite, key, `Int64 t = suite ctx, key `K64, timestamp in
    let q = challenge suite in
    let a = R.get_ok (gen ~t:(`Int64 t) suite ~key ~q) in
    let v = verify ~t:(`Int64 (Int64.add t 2L)) ~tw:5 suite ~key ~q ~a in
    assert_equal (R.get_ok v) (true, None) in

  let v3 ctx =
    let suite, key, c, t = suite ctx, key `K32, 0x0L, `Now in
    let q = challenge suite in
    let time = Int64.of_float (Unix.time ()) in
    let a = R.get_ok (gen ~time ~c ~t suite ~key ~q) in
    let v = verify ~time ~c:(Int64.sub c 23L) ~cw:42 ~t ~tw:5 suite ~key ~q ~a in
    assert_equal ~printer:(function
        | true , Some i -> "true,Some:" ^ Int64.to_string i
        | false, Some i -> "false,Some:" ^ Int64.to_string i
        | true, None -> "true,None"
        | false, None -> "false,None"
      )
      (R.get_ok v) (true, Some (Int64.add c 1L)) in

  let v4 ctx =
    let suite, key, c, `Int64 t = suite ctx, key `K32, 0x0L, timestamp in
    let q = challenge suite in
    let a = R.get_ok (gen ~c ~t:(`Int64 t) suite ~key ~q) in
    let v = verify ~c:(Int64.sub c 23L) ~cw:42 ~t:(`Int64 (Int64.add t 6L))
        ~tw:5 suite ~key ~q ~a in
    assert_equal (R.get_ok v) (false, None) in

  ["OCRA-1:HOTP-SHA512-10:C-QA64" >:: v1;
   "OCRA-1:HOTP-SHA512-10:QA64-T1M" >:: v2;
   "OCRA-1:HOTP-SHA256-0:C-QN15-T1M" >:: v3;
   "OCRA-1:HOTP-SHA256-0:C-QN15-T1M" >:: v4]

let suite =
  "All" >::: [ "known_answer" >::: known_answer;
               "verify" >::: verify;
               "coverage" >::: coverage]

let () =
  Mirage_crypto_rng_unix.initialize () ;
  run_test_tt_main suite
