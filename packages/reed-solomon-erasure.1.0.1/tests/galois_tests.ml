open OUnit2
open Reed_solomon_erasure__.Galois
open Reed_solomon_erasure__.Ops
open Reed_solomon_erasure__.Tables
module Bigstring = Core_kernel.Bigstring

let backblaze_log_table =
  (String.concat ""
     ["\000\000\001\025\002\050\026\198";
      "\003\223\051\238\027\104\199\075";
      "\004\100\224\014\052\141\239\129";
      "\028\193\105\248\200\008\076\113";
      "\005\138\101\047\225\036\015\033";
      "\053\147\142\218\240\018\130\069";
      "\029\181\194\125\106\039\249\185";
      "\201\154\009\120\077\228\114\166";
      "\006\191\139\098\102\221\048\253";
      "\226\152\037\179\016\145\034\136";
      "\054\208\148\206\143\150\219\189";
      "\241\210\019\092\131\056\070\064";
      "\030\066\182\163\195\072\126\110";
      "\107\058\040\084\250\133\186\061";
      "\202\094\155\159\010\021\121\043";
      "\078\212\229\172\115\243\167\087";
      "\007\112\192\247\140\128\099\013";
      "\103\074\222\237\049\197\254\024";
      "\227\165\153\119\038\184\180\124";
      "\017\068\146\217\035\032\137\046";
      "\055\063\209\091\149\188\207\205";
      "\144\135\151\178\220\252\190\097";
      "\242\086\211\171\020\042\093\158";
      "\132\060\057\083\071\109\065\162";
      "\031\045\067\216\183\123\164\118";
      "\196\023\073\236\127\012\111\246";
      "\108\161\059\082\041\157\085\170";
      "\251\096\134\177\187\204\062\090";
      "\203\089\095\176\156\169\160\081";
      "\011\245\022\235\122\117\044\215";
      "\079\174\213\233\230\231\173\232";
      "\116\214\244\234\168\080\088\175"])

let log_table_same_as_backblaze test_ctxt =
  for i = 0 to (256) - 1 do
    assert_equal log_table.{i} backblaze_log_table.[i]
  done

let test_associativity test_ctxt =
  for a = 0 to (256) - 1 do
    let a = char_of_int a in
    for b = 0 to (256) - 1 do
      let b = char_of_int b in
      for c = 0 to (256) - 1 do
        let c = char_of_int c in
        let x = add a (add b c) in
        let y = add (add b c) a in
        assert_equal x y;
        let x = mul a (mul b c) in
        let y = mul (mul a b) c in
        assert_equal x y;
      done
    done
  done

let qc_add_associativity =
  QCheck_runner.to_ounit2_test
    (QCheck.Test.make ~count:10000 ~name:"qc_add_associativity"
       QCheck.(triple char char char)
       (fun (a,b,c) -> add a (add b c) = add (add a b) c))

let qc_mul_associativity =
  QCheck_runner.to_ounit2_test
    (QCheck.Test.make ~count:10000 ~name:"qc_mul_associativity"
       QCheck.(triple char char char)
       (fun (a,b,c) -> mul a (mul b c) = mul (mul a b) c))

let test_identity test_ctxt =
  let char_0 = char_of_int 0 in
  let char_1 = char_of_int 1 in
  for a = 0 to (256) - 1 do
    let a = char_of_int a in
    let b = sub char_0 a in
    let c = sub a b in
    assert_equal c char_0;
    if a <> char_0 then (
      let b = div char_1 a in
      let c = mul a b in
      assert_equal c char_1;
    )
  done

let qc_additive_identity =
  QCheck_runner.to_ounit2_test
    (QCheck.Test.make ~count:10000 ~name:"qc_additive_identity"
       QCheck.char
       (fun a -> sub a (sub (char_of_int 0) a) = char_of_int 0))

let qc_multiplicative_identity =
  QCheck_runner.to_ounit2_test
    (QCheck.Test.make ~count:10000 ~name:"qc_multiplicative_identity"
       QCheck.char
       (fun a ->
          QCheck.assume (a <> (char_of_int 0));
          mul a (div (char_of_int 1) a) = char_of_int 1))

let test_commutativity test_ctxt =
  for a = 0 to (256) - 1 do
    let a = char_of_int a in
    for b = 0 to (256) -1 do
      let b = char_of_int b in
      let x = add a b in
      let y = add b a in
      assert_equal x y;
      let x = mul a b in
      let y = mul b a in
      assert_equal x y;
    done
  done

let qc_add_commutativity =
  QCheck_runner.to_ounit2_test
    (QCheck.Test.make ~count:10000 ~name:"qc_add_commutativity"
       QCheck.(triple char char char)
       (fun (a,b,_) ->
          add a b = add b a))

let qc_mul_commutativity =
  QCheck_runner.to_ounit2_test
    (QCheck.Test.make ~count:10000 ~name:"qc_mul_commutativity"
       QCheck.(triple char char char)
       (fun (a,b,_) ->
          mul a b = mul b a))

let test_distributivity test_ctxt =
  for a = 0 to (256) - 1 do
    let a = char_of_int a in
    for b = 0 to (256) - 1 do
      let b = char_of_int b in
      for c = 0 to (256) - 1 do
        let c = char_of_int c in
        let x = mul a (add b c) in
        let y = add (mul a b) (mul a c) in
        assert_equal x y;
      done
    done
  done

let qc_add_distributivity =
  QCheck_runner.to_ounit2_test
    (QCheck.Test.make ~count:10000 ~name:"qc_add_distributivity"
       QCheck.(triple char char char)
       (fun (a,b,c) ->
          mul a (add b c) = add (mul a b) (mul a c)))

let test_exp test_ctxt =
  for a = 0 to (256) - 1 do
    let a = char_of_int a in
    let power = ref 1 in
    for j = 0 to (256) - 1 do
      let x = exp a j in
      assert_equal x (char_of_int !power);
      power := int_of_char (mul (char_of_int !power) a);
    done
  done

let test_galois test_ctxt =
  assert_equal (mul (char_of_int 3) (char_of_int 4)) (char_of_int 12);
  assert_equal (mul (char_of_int 7) (char_of_int 7)) (char_of_int 21);
  assert_equal (mul (char_of_int 23) (char_of_int 45)) (char_of_int 41);

  let input = Bigstring.of_string "\000\001\002\003\004\005\006\010\050\100\150\174\201\255\099\032\067\085\200\199\198\197\196\195\194\193\192\191\190\189\188\187\186\185" in
  let output1 = Bigstring.create (Bigstring.length input) in
  let output2 = Bigstring.create (Bigstring.length input) in
  mul_slice (char_of_int 25) input output1;
  let expect = "\x00\x19\x32\x2b\x64\x7d\x56\xfa\xb8\x6d\xc7\x85\xc3\x1f\x22\x07\x25\xfe\xda\x5d\x44\x6f\x76\x39\x20\x0b\x12\x11\x08\x23\x3a\x75\x6c\x47" in
  for i = 0 to (Bigstring.length input) - 1 do
    assert_equal expect.[i] output1.{i}
  done;
  mul_slice_pure_ocaml (char_of_int 25) input output2;
  for i = 0 to (Bigstring.length input) - 1 do
    assert_equal expect.[i] output2.{i}
  done;

  let expect_xor = "\x00\x2d\x5a\x77\xb4\x99\xee\x2f\x79\xf2\x07\x51\xd4\x19\x31\xc9\xf8\xfc\xf9\x4f\x62\x15\x38\xfb\xd6\xa1\x8c\x96\xbb\xcc\xe1\x22\x0f\x78" in
  mul_slice_xor (char_of_int 52) input output1;
  for i = 0 to (Bigstring.length input) - 1 do
    assert_equal expect_xor.[i] output1.{i}
  done;
  mul_slice_xor_pure_ocaml (char_of_int 52) input output2;
  for i = 0 to (Bigstring.length input) - 1 do
    assert_equal expect_xor.[i] output2.{i}
  done;

  let expect = "\x00\xb1\x7f\xce\xfe\x4f\x81\x9e\x03\x06\xe8\x75\xbd\x40\x36\xa3\x95\xcb\x0c\xdd\x6c\xa2\x13\x23\x92\x5c\xed\x1b\xaa\x64\xd5\xe5\x54\x9a" in
  mul_slice (char_of_int 177) input output1;
  for i = 0 to (Bigstring.length input) - 1 do
    assert_equal expect.[i] output1.{i}
  done;
  mul_slice_pure_ocaml (char_of_int 177) input output2;
  for i = 0 to (Bigstring.length input) - 1 do
    assert_equal expect.[i] output2.{i}
  done;

  let expect_xor = "\x00\xc4\x95\x51\x37\xf3\xa2\xfb\xec\xc5\xd0\xc7\x53\x88\xa3\xa5\x06\x78\x97\x9f\x5b\x0a\xce\xa8\x6c\x3d\xf9\xdf\x1b\x4a\x8e\xe8\x2c\x7d" in
  mul_slice_xor (char_of_int 117) input output1;
  for i = 0 to (Bigstring.length input) - 1 do
    assert_equal expect_xor.[i] output1.{i}
  done;
  mul_slice_xor_pure_ocaml (char_of_int 117) input output2;
  for i = 0 to (Bigstring.length input) - 1 do
    assert_equal expect_xor.[i] output2.{i}
  done;

  assert_equal (exp (char_of_int 2) 2) (char_of_int 4);
  assert_equal (exp (char_of_int 5) 20) (char_of_int 235);
  assert_equal (exp (char_of_int 13) 7) (char_of_int 43)

let test_slice_add test_ctxt =
  List.iter
    (fun len ->
       let input = Bigstring.of_bytes (Bytes.init len (fun _ -> char_of_int (Random.int 256))) in
       let output = Bigstring.init len (fun _ -> char_of_int (Random.int 256)) in
       let expect = Bigstring.create len in
       for i = 0 to (len) - 1 do
         expect.{i} <- char_of_int ((int_of_char input.{i}) lxor (int_of_char output.{i}));
       done;
       slice_xor input output;
       for i = 0 to (len) - 1 do
         assert_equal expect.{i} output.{i};
       done;
       let output = Bigstring.init len (fun _ -> char_of_int (Random.int 256)) in
       for i = 0 to (len) - 1 do
         expect.{i} <- char_of_int ((int_of_char input.{i}) lxor (int_of_char output.{i}));
       done;
       slice_xor input output;
       for i = 0 to (len) - 1 do
         assert_equal expect.{i} output.{i};
       done;
    )
    [16; 32; 34]

let test_div_a_is_0 test_ctxt =
  assert_equal (char_of_int 0) (div (char_of_int 0) (char_of_int 100))

let test_div_b_is_0 test_ctxt =
  try
    div (char_of_int 1) (char_of_int 0) |> ignore;
    assert_failure "Missing exception"
  with
  | Failure _ -> ()

let test_pure_ocaml_same_as_maybe_ffi test_ctxt =
  let len = 10_003 in
  for _ = 0 to (5000) - 1 do
    let c = char_of_int (Random.int 256) in
    let input = Bigstring.init len (fun _ -> char_of_int (Random.int 256)) in
    begin
      let output      = Bigstring.init len (fun _ -> char_of_int (Random.int 256)) in
      let output_copy = Reed_solomon_erasure.RS_Shard_utils.copy_bigstr output in

      mul_slice            c input output;
      mul_slice_pure_ocaml c input output_copy;

      assert_equal output output_copy;
    end;
    begin
      let output = Bigstring.init len (fun _ -> char_of_int (Random.int 256)) in
      let output_copy = Reed_solomon_erasure.RS_Shard_utils.copy_bigstr output in

      mul_slice_xor            c input output;
      mul_slice_xor_pure_ocaml c input output_copy;

      assert_equal output output_copy;
    end
  done

let suite =
  "galois_tests">:::
  ["log_table_same_as_backblaze">::       log_table_same_as_backblaze;
   "test_associativity">::                test_associativity;
   qc_add_associativity;
   qc_mul_associativity;
   "test_identity">::                     test_identity;
   qc_additive_identity;
   qc_multiplicative_identity;
   "test_commutativity">::                test_commutativity;
   qc_add_associativity;
   qc_mul_associativity;
   "test_distributivity">::               test_distributivity;
   qc_add_distributivity;
   "test_exp">::                          test_exp;
   "test_galois">::                       test_galois;
   "test_slice_add">::                    test_slice_add;
   "test_div_a_is_0">::                   test_div_a_is_0;
   "test_div_b_is_0">::                   test_div_b_is_0;
   "test_pure_ocaml_same_as_maybe_ffi">:: test_pure_ocaml_same_as_maybe_ffi;
  ]
