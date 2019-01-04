(* The tests *)
let increment_base32_1 () = 
  Alcotest.(check string) "increments correctly" "A109D" (Ulid.increment_base_32 "A109C")

let increment_base32_2 () =
  Alcotest.(check string) "carries correctly" "A1Z00" (Ulid.increment_base_32 "A1YZZ")
  
let increment_base32_3 () =
  Alcotest.(check string) "double increments correctly" "A1Z01" (Ulid.increment_base_32 (Ulid.increment_base_32 "A1YZZ"))

let test_set_1 = [
  "base 32 increment increments correctly", `Quick, increment_base32_1;
  "base 32 increment carries correctly", `Quick, increment_base32_2;
  "base 32 increment double increments correctly", `Quick, increment_base32_3;
]

let encode_time_1 () =
  Alcotest.(check string) "encodes properly" "01ARYZ6S41" (Ulid.encode_time 1469918176385 10)

let encode_time_2 () =
  Alcotest.(check string) "changes length properly" "0001AS99AA60" (Ulid.encode_time 1470264322240 12)

let encode_time_3 () =
  Alcotest.(check string) "truncates length properly" "AS4Y1E11" (Ulid.encode_time 1470118279201 8)

let test_set_2 = [
  "encode time encodes time", `Quick, encode_time_1;
  "encode time changes length", `Quick, encode_time_2;
  "encode time truncates length", `Quick, encode_time_3;
]

let encode_random_1 () =
  Alcotest.(check int) "returns current length" 12 (String.length (Ulid.encode_random 12 (Ulid.get_nocrypto_rng ())))

let test_set_3 = [
  "encode random returns correct length", `Quick, encode_random_1;
]

let ulid_1 () =
  Alcotest.(check int) "returns current length" 26 (String.length (Ulid.ulid ()))

let ulid_2 () =
  let ulid = (Ulid.ulid ~seed_time:1469918176385 ()) in
  let time = (String.sub ulid 0 10) in
  Alcotest.(check string) "returns expected time encoded" "01ARYZ6S41" time

(* let ulid_3 () =
  let rec gen counter =
    if counter = 0 then ()
    else (
      print_string (Ulid.ulid ());
      print_newline ();
      gen (counter - 1)
    ) in
  gen 10;
  Alcotest.(check int) "help" 0 1 *)

let test_set_4 = [
  "ulid returns correct length", `Quick, ulid_1;
  "ulid correctly encodes time", `Quick, ulid_2;
  (* "ulid looks good", `Quick, ulid_3; *)
]

let stubbed_rng n = (n * 0) + 30;;
let monotonic_ulid = Ulid.monotonic_factory ~prng:stubbed_rng ();;

let monotonic_1 () =
  Alcotest.(check string) "first call" "01ARYZ6S41YYYYYYYYYYYYYYYY" (monotonic_ulid ~seed_time:1469918176385 ())

let monotonic_2 () =
  Alcotest.(check string) "second call same time" "01ARYZ6S41YYYYYYYYYYYYYYYZ" (monotonic_ulid ~seed_time:1469918176385 ())

let monotonic_3 () =
  Alcotest.(check string) "third call smaller time" "01ARYZ6S41YYYYYYYYYYYYYYZ0" (monotonic_ulid ~seed_time:100000000 ())

let monotonic_4 () =
  Alcotest.(check string) "fourth call even smaller time" "01ARYZ6S41YYYYYYYYYYYYYYZ1" (monotonic_ulid ~seed_time:10000 ())

let monotonic_5 () =
  Alcotest.(check string) "fifth call 1 greater time" "01ARYZ6S42YYYYYYYYYYYYYYYY" (monotonic_ulid ~seed_time:1469918176386 ())

let test_set_5 = [
  "monotonic first call", `Quick, monotonic_1;
  "monotonic second call", `Quick, monotonic_2;
  "monotonic third call", `Quick, monotonic_3;
  "monotonic fourth call", `Quick, monotonic_4;
  "monotonic fifth call", `Quick, monotonic_5;
]

(* Run it *)
let () =
  Alcotest.run "ULID tests" [
    "base 32 increment", test_set_1;
    "encode time", test_set_2;
    "encode random", test_set_3;
    "ulid", test_set_4;
    "monotonic", test_set_5;
  ]