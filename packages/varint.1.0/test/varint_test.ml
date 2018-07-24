open Varint
open OUnit2

let reset b =
  
  let off = Mstruct.offset b in
  let pos = (0 - off) in
  Mstruct.shift b pos;
  b
  

       
let test_varint32 ctx =
  let i = Random.int32 1000000000l in
  let got = VarInt32.to_cstruct i |> VarInt32.of_cstruct in

  
  assert_equal i got;

  let mbuf = Mstruct.create 400 in
  VarInt32.write_varint mbuf i;


  let got_1 = reset mbuf |> VarInt32.read_varint in
  assert_equal i got_1
               


let test_varint64 ctx =
  let i = Random.int64 8446744073709551615L in
  let got = VarInt64.to_cstruct i |> VarInt64.of_cstruct in

  assert_equal i got;
  let mbuf = Mstruct.create 400 in
  VarInt64.write_varint mbuf i;

  let got_1 = reset mbuf |> VarInt64.read_varint in
  assert_equal i got_1




               
let test_lfp ctx =
  let module LFP = LengthFieldPrefixing(VarInt32) in
  let hello = "hello friend, this world is an ugly place." in


  let msg = Cstruct.of_string hello in 
  let buf = LFP.encode msg |> Mstruct.of_cstruct in
  
  let got = LFP.decode buf |> Cstruct.to_string in
  assert_equal got hello

               

let suite =
  "VarInt Suite" >:::
    [
      "testing VarInt32" >:: test_varint32;
      "testing VarInt64" >:: test_varint64;
      "testing length field prefixing" >:: test_lfp
    ]
               
let () =
  run_test_tt_main suite
  
