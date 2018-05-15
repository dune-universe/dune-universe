module Radix = Radis.Make(String)

let test_add ~keylen =
  let seed = Random.int 1000 in

  Random.init seed;

  let random_string len =
    let res = Bytes.create len in
    for i = 0 to len - 1
    do let n = Random.int (10 + 26 + 26) in
       match n with
       | n when n < 26 -> Bytes.unsafe_set res i (Char.chr (n + 65))
       | n when n < 26 + 26 -> Bytes.unsafe_set res i (Char.chr (n + 97 - 26))
       | n -> Bytes.unsafe_set res i (Char.chr (n + 48 - 52))
    done;
    Bytes.unsafe_to_string res in

  let radix =
    let rec go radix i =
      if i = 1000
      then radix
      else go (Radix.add (random_string keylen) true radix) (succ i) in
    go Radix.empty 0 in

  Alcotest.test_case "add" `Quick
    (fun () ->
      Random.init seed;
        for _ = 0 to 999
        do let key = random_string keylen in
           if not (Radix.mem key radix) then raise Not_found done)

let () =
  Alcotest.run "radis"
    [ "add", [ test_add ~keylen:40 ] ]
