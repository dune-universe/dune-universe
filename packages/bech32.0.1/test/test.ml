open Alcotest
open Bech32

let random_string n = String.init n (fun _ -> Char.chr @@ Random.int 256)

let test_changebase_simple () =
  for _ = 0 to 100 do
    let rand = random_string 100 in
    match convertbits ~pad:true ~frombits:8 ~tobits:5 rand with
    | Error msg -> failwith (Printf.sprintf "from %d to %d: %s" 8 5 msg)
    | Ok rand' -> (
        match convertbits ~pad:false ~frombits:5 ~tobits:8 rand' with
        | Error msg -> failwith (Printf.sprintf "from %d to %d: %s" 5 8 msg)
        | Ok rand'' -> check string "changebase_simple" rand rand'' )
  done

let test_changebase = [ ("random_simple", `Quick, test_changebase_simple) ]

let valid_vectors_mainnet =
  [
    ( "0014751e76e8199196d454941c45d1b3a323f1433bd6",
      "BC1QW508D6QEJXTDG4Y5R3ZARVARY0C5XW7KV8F3T4" );
    ("6002751e", "BC1SW50QA3JX3S");
    ( "5210751e76e8199196d454941c45d1b3a323",
      "bc1zw508d6qejxtdg4y5r3zarvaryvg6kdaj" );
  ]

let valid_vectors_testnet =
  [
    ( "00201863143c14c5166804bd19203356da136c985678cd4d27a1b8c6329604903262",
      "tb1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3q0sl5k7" );
    ( "0020000000c4a5cad46221b2a187905e5266362b99d5e91c6ce24d165dab93e86433",
      "tb1qqqqqp399et2xygdj5xreqhjjvcmzhxw4aywxecjdzew6hylgvsesrxh6hy" );
  ]

let decode_check_valid :
    type a.
    (module Segwit.NETWORK with type t = a) -> string -> string -> unit -> unit
    =
 fun m hex v () ->
  match Segwit.(decode m v) with
  | Error msg -> failwith msg
  | Ok t -> (
      let module N = (val m : Segwit.NETWORK with type t = a) in
      let script = Segwit.scriptPubKey t in
      let (`Hex decoded) = Hex.of_string script in
      check string "decode_check_valid_decoded" hex decoded;
      match Segwit.encode t with
      | Error msg -> failwith msg
      | Ok addr ->
          check string "decode_check_valid_encoded" (String.lowercase_ascii v)
            addr )

let decode_check_valid_mainnet =
  ListLabels.map valid_vectors_mainnet ~f:(fun (hex, v) ->
      (v, `Quick, decode_check_valid (module Segwit.Btc) hex v))

let decode_check_valid_testnet =
  ListLabels.map valid_vectors_testnet ~f:(fun (hex, v) ->
      (v, `Quick, decode_check_valid (module Segwit.Tbtc) hex v))

let invalid_vectors_mainnet =
  [
    "bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t5";
    "BC13W508D6QEJXTDG4Y5R3ZARVARY0C5XW7KN40WF2";
    "bc1rw5uspcuh";
    "bc10w508d6qejxtdg4y5r3zarvary0c5xw7kw508d6qejxtdg4y5r3zarvary0c5xw7kw5rljs90";
    "BC1QR508D6QEJXTDG4Y5R3ZARVARYV98GJ9P";
    "bc1zw508d6qejxtdg4y5r3zarvaryvqyzf3du";
    "bc1gmk9yu";
  ]

let invalid_vectors_testnet =
  [
    "tc1qw508d6qejxtdg4y5r3zarvary0c5xw7kg3g4ty";
    "tb1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3q0sL5k7";
    "tb1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3pjxtptv";
  ]

let decode_invalid :
    type a. (module Segwit.NETWORK with type t = a) -> string -> unit -> unit =
 fun m v () ->
  match Segwit.decode m v with
  | Error _ -> ()
  | Ok { prog; _ } -> failwith (Printf.sprintf "prog=[%S]" prog)

let decode_check_invalid_mainnet =
  ListLabels.map invalid_vectors_mainnet ~f:(fun v ->
      (v, `Quick, decode_invalid (module Segwit.Btc) v))

let decode_check_invalid_testnet =
  ListLabels.map invalid_vectors_testnet ~f:(fun v ->
      (v, `Quick, decode_invalid (module Segwit.Tbtc) v))

let valid_vectors =
  [
    "A12UEL5L";
    "a12uel5l";
    "an83characterlonghumanreadablepartthatcontainsthenumber1andtheexcludedcharactersbio1tt5tgs";
    "abcdef1qpzry9x8gf2tvdw0s3jn54khce6mua7lmqqqxw";
    "11qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqc8247j";
    "split1checkupstagehandshakeupstreamerranterredcaperred2y9e3w";
    "?1ezyfcl";
  ]

let invalid_vectors =
  [
    "\x201nwldj5";
    "\x7f1axkwrx";
    "\x801eym55h";
    "an84characterslonghumanreadablepartthatcontainsthenumber1andtheexcludedcharactersbio1569pvx";
    "pzry9x0s0muk";
    "1pzry9x0s0muk";
    "x1b4n0q5v";
    "li1dgmt3";
    "de1lg7wt\xff";
    "A1G7SGD8";
    "10a06t8";
    "1qzzfhee";
  ]

let valid_encoding s () =
  match decode s with Error msg -> failwith msg | Ok _ -> ()

let invalid_encoding s () =
  match decode s with Error _ -> () | Ok _ -> failwith ""

let decode_valid =
  ListLabels.map valid_vectors ~f:(fun v -> (v, `Quick, valid_encoding v))

let decode_invalid =
  ListLabels.map invalid_vectors ~f:(fun v -> (v, `Quick, invalid_encoding v))

let () =
  Alcotest.run "bech32"
    [
      ("changebase", test_changebase);
      ("segwit_decode_valid_mainnet", decode_check_valid_mainnet);
      ("segwit_decode_valid_testnet", decode_check_valid_testnet);
      ("segwit_decode_invalid_mainnet", decode_check_invalid_mainnet);
      ("segwit_decode_invalid_testnet", decode_check_invalid_testnet);
      ("decode_valid", decode_valid);
      ("decode_invalid", decode_invalid);
    ]
