open OUnit2
open MUTF8

let pp_str s =
  BatPrintf.sprintf "%S" s

let pp_u16s s =
  BatString.concat
    " "
    (List.map (fun c ->
         if c >= 0x20 && c <= 0x7F && c != 34 && c != 39 && c != 92 then
           BatString.of_list [ '"'; BatChar.chr c; '"' ]
         else
           BatPrintf.sprintf "%04X" c)
    s)

let count_seq s =
  Seq.fold_left (fun i _ -> succ i) 0 s

(* Verify that simple strings don't get re-encoded unnecessarily. *)
let phys_roundtrip inp _ =
  assert_bool "utf8-bytes"  (inp == (inp |> of_utf8  |> to_bytes));
  assert_bool "utf8-utf8"   (inp == (inp |> of_utf8  |> to_utf8));
  assert_bool "bytes-utf8"  (inp == (inp |> of_bytes |> to_utf8));
  assert_bool "bytes-bytes" (inp == (inp |> of_bytes |> to_bytes))
let phys_roundtrip_tests = "phys_roundtrip" >::: [
      "simple" >:: phys_roundtrip "Simple string";
      "latin"  >:: phys_roundtrip "Simple\xC3\x97string";
      "empty"  >:: phys_roundtrip "";
    ]

(* Given a MUTF8.t value v, assert various properties *)
let ptest v mutf utf u16len u32len u16codes ctxt =
  assert_equal ~ctxt:ctxt ~printer:pp_str ~msg:"to_bytes"
    mutf (to_bytes v);
  assert_equal ~ctxt:ctxt ~printer:pp_str ~msg:"to_utf8"
    utf (to_utf8 v);
  assert_equal ~ctxt:ctxt ~printer:string_of_int ~msg:"utf16_length"
    u16len (utf16_length v);
  assert_equal ~ctxt:ctxt ~printer:string_of_int ~msg:"unicode_length"
    u32len (unicode_length v);
  assert_equal ~ctxt:ctxt ~printer:pp_u16s ~msg:"to_utf16_seq"
    u16codes (List.of_seq @@ to_utf16_seq v);
  assert_equal ~ctxt:ctxt ~printer:pp_u16s ~msg:"to_utf16_enum"
    u16codes (BatList.of_enum @@ to_utf16_enum v)

(* Check that various representations of the same string convert to
   each other *)
let crosstest mutf utf u16codes u32len =
  let s1 = of_bytes mutf in
  let s2 = of_utf8 utf in
  let s3 = of_utf16_seq (List.to_seq u16codes) in
  let u16len = List.length u16codes in
  [
    "via_mutf"  >:: ptest s1 mutf utf u16len u32len u16codes;
    "via_utf8"  >:: ptest s2 mutf utf u16len u32len u16codes;
    "via_utf16" >:: ptest s3 mutf utf u16len u32len u16codes;
    "equiv"     >:: (fun c ->
      (* debugdump s1; debugdump s2; debugdump s3; *)
      assert_equal ~ctxt:c ~msg:"bytes-utf8"  s1 s2;
      assert_equal ~ctxt:c ~msg:"bytes-utf16" s1 s3;
      assert_equal ~ctxt:c ~msg:"utf8-utf16"  s2 s3
    )
  ]
let value_crosstests = "value-crosstest" >::: [
      "simple" >::: crosstest
                      "Simple string"
                      "Simple string"
                      [83;105;109;112;108;101;32;115;116;114;105;110;103]
                      13;
      "empty" >::: crosstest
                     "" "" [] 0;
      "latin" >::: crosstest
                     "A\xC3\x97B"
                     "A\xC3\x97B"
                     [65;215;66] 3;
      "nul" >::: crosstest
                   "nul:\xC0\x80"
                   "nul:\x00"
                   [110; 117; 108; 58; 0] 5;
      "midbmp" >::: crosstest
                      "\xE1\x80\x80chars\xE5\x9C\xAA"
                      "\xE1\x80\x80chars\xE5\x9C\xAA"
                      [0x1000; 99; 104; 97; 114; 115; 0x572A]
                      7;
      "supp" >::: crosstest
                    "hi\xED\xA0\x80\xED\xBD\x88"
                    "hi\xF0\x90\x8D\x88"
                    [104; 105; 0xD800; 0xDF48]
                    3;
      "high" >::: crosstest
                    "\xC3\x97\xED\xAF\xBF\xED\xBF\xBFz"
                    "\xC3\x97\xF4\x8F\xBF\xBFz"
                    [215; 0xDBFF; 0xDFFF; 122] 3;
    ]

(* Check that of_bytes rejects malformed MUTF8 *)
let invalidities1 =
  "invalid-mutf8" >:::
    let malf mutf _ =
      assert_raises ~msg:"should reject malformed MUTF8"
        BatUTF8.Malformed_code
        (fun () -> of_bytes mutf)
    in [
        "trunc0" >:: malf "foo\xC0";
        "trunc0" >:: malf "foo\xE1";
        "trunc1" >:: malf "foo\xE1\x9F";
        "sp0"    >:: malf "\xC0bar";
        "sp0"    >:: malf "\xE2bar";
        "sp1"    >:: malf "\xE2\xA0bar";
        "embednul" >:: malf "\x00foo";
        "embednul" >:: malf "bar\x00";
        "supp_plane" >:: malf "hi\xF0\x90\x8D\x88";
      ]

(* Check that of_utf16_seq rejects out-of-range UTF16 *)
let invalidities2 =
  "invalid-utf16" >:::
  let malf u16 _ =
    assert_raises ~msg:"should reject invalid UTF-16"
      BatUChar.Out_of_range
      (fun () -> of_utf16_seq (List.to_seq u16))
  in [
      "large" >:: malf [1; 2; 65536; 3];
      "large" >:: malf [1; 2; 0xFFFFFFFF; 3];
      "small" >:: malf [3; -1; 2]
    ]

(* Verify that MUTF8.compare produces the correct ordering *)
let orderings =
  let values = Array.map (List.to_seq) [|
                   [];
                   [0];
                   [0; 1;];
                   [0; 1; 10000];
                   [1];
                   [1; 0];
                   [1; 0; 10000];
                   [1; 1];
                   [1; 1; 99];
                   [1; 1; 999];
                   [1; 1; 10000];
                   [1; 128; 1];
                   [128];
                   [128; 0];
                   [128; 0; 0];
                   [128; 0; 1];
                 |] in
  "orderings" >:::
    let c = BatList.range 0 `To (pred @@ Array.length values) in
    List.map (fun (l,r) ->
        let res = MUTF8.compare
                    (of_utf16_seq @@ Array.get values l)
                    (of_utf16_seq @@ Array.get values r) in
        let rval, rop = if l < r then
                          (-1, '<')
                        else if l > r then
                          (1, '>')
                        else
                          (0, '=') in
        (BatPrintf.sprintf "%d%c%d" l rop r) >:: (fun ctxt ->
          assert_equal ~ctxt:ctxt ~printer:string_of_int ~msg:"MUTF8.compare" rval res))
      (BatList.cartesian_product c c)


(* Check that we handle unpaired surrogates. These are valid in MUTF8, but
   not valid in Unicode or UTF-8. However, we pretend that they would
   be valid if they were converted to Unicode. *)
let check16 mutf u16 ?u32 ctxt =
  let u32' = match u32 with None -> u16 | Some v -> v in
  let v1 = of_bytes mutf in
  let v2 = of_utf16_seq @@ List.to_seq u16 in
  assert_equal ~ctxt:ctxt ~printer:pp_u16s
    ~msg:"bytes->utf16" u16  (List.of_seq @@ to_utf16_seq v1);
  assert_equal ~ctxt:ctxt ~printer:pp_u16s
    ~msg:"utf16->utf16" u16  (List.of_seq @@ to_utf16_seq v2);
  assert_equal ~ctxt:ctxt ~printer:pp_str
    ~msg:"bytes->bytes" mutf (to_bytes v1);
  assert_equal ~ctxt:ctxt ~printer:pp_str
    ~msg:"utf16->bytes" mutf (to_bytes v2);
  assert_equal ~ctxt:ctxt ~printer:string_of_int
    ~msg:"bytes->u16len" (List.length u16) (utf16_length v1);
  assert_equal ~ctxt:ctxt ~printer:string_of_int
    ~msg:"utf16->u16len" (List.length u16) (utf16_length v2);
  assert_equal ~ctxt:ctxt ~printer:string_of_int
    ~msg:"bytes->u32len" (List.length u32') (unicode_length v1);
  assert_equal ~ctxt:ctxt ~printer:string_of_int
    ~msg:"utf16->u32len" (List.length u32') (unicode_length v2);
  let useq = to_utf16_seq v1 in
  assert_raises
    BatUChar.Out_of_range
    (fun () -> count_seq (strict_to_ucs32 useq));
  assert_equal ~ctxt:ctxt ~printer:pp_u16s
    ~msg:"wobbly-u32" u32' (List.of_seq @@ wobbly_to_ucs32 useq)

let unpaired =
  "unpaired" >::: [
      "trailhi" >:: check16 "hi\xED\xA0\x80" [104; 105; 0xD800];
      "traillo" >:: check16 "hi\xED\xBD\x88" [104; 105; 0xDF48];
      "swapped" >:: check16 "hi\xED\xBD\x88\xED\xA0\x80j"
                      [104; 105; 0xDF48; 0xD800; 106];
      "aab" >:: check16 "a\xED\xA1\x82\xED\xA3\x84\xED\xBD\x88"
                  [97; 0xD842; 0xD8C4; 0xDF48]
                  ~u32:[97; 0xD842; 0x41348];
      "bab" >:: check16 "a\xED\xBD\x88\xED\xA0\x80\xED\xBD\x88"
                  [97; 0xDF48; 0xD800; 0xDF48]
                  ~u32:[97; 0xDF48; 0x10348];
    ]


let () =
  run_test_tt_main (
      "mutf8" >::: [
        phys_roundtrip_tests;
        value_crosstests;
        invalidities1; invalidities2;
        orderings;
        unpaired
      ]
    )

