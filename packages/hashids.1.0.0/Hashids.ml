(* Copyright 2016-2017 Vincent Jacques <vincent@vincent-jacques.net> *)

open General.Abbr
open IntRef.O


let shuffle x ~salt =
  if x = "" || salt = "" then x else
  let salt =
    salt
    |> Str.to_list
    |> Li.map ~f:Ch.to_int
    |> Li.to_array
  in
  (* Shuffling is based on swapping characters so we need a copy.
  Maybe we could find an implementation where each char can ben computed
  functionally and avoid mutations? *)
  let x = By.of_string x in
  let swap i j =
    let xi = By.get x i
    and xj = By.get x j
    in begin
      x.[j] <- xi;
      x.[i] <- xj
    end
  and last = By.size x - 1
  and salt_size = Ar.size salt
  and p = ref 0 in
  for i = last downto 1 do
    let v = (last - i) mod salt_size in
    p =+ salt.(v);
    swap i ((salt.(v) + v + !p) mod i)
  done;
  By.to_string x

module ShuffleTests = struct
  open Tst

  let test = "shuffle" >:: (
    let make salt x expected =
      ~: "salt:%S x:%S -> %S" salt x expected (lazy (check_string ~expected (shuffle ~salt x)))
    in [
      make "" "" "";
      make "" "abcdefghij" "abcdefghij";
      make "xyz" "" "";
      make "xyz" "a" "a";
      make "x" "abcdefghij" "bcdhfjieag";
      make "y" "abcdefghij" "eagcjhfbdi";
      make "xx" "abcdefghij" "cdhfjaiebg";
      make "yy" "abcdefghij" "fdjcghabei";
      make "xyz" "abcdefghij" "hjfacbiedg";
    ]
  )
end


let hash n ~alphabet =
  let base = Str.size alphabet in
  let current n = alphabet.[n mod base]
  and next n = n / base in
  let rec loop ret = function
    | 0 -> ret
    | n -> step ret n
  and step ret n =
    loop ((current n)::ret) (next n)
  in
  step [] n
  |> Str.of_list

let unhash s ~alphabet =
  let base = Str.size alphabet
  and digits =
    alphabet
    |> Str.to_list
    |> Li.map_i ~f:(fun ~i c -> (c, i))
    |> ChSoMap.of_list_last
  in
  s
  |> Str.to_list
  |> Li.reverse
  |> Li.fold_acc ~acc:1 ~init:0 ~f:(fun ~acc:mult value k ->
    let value = value + mult * ChSoMap.get digits ~k
    and mult = base * mult in
    (mult, value)
  )

module HashTests = struct
  open Tst

  let test = "hash/unhash" >:: (
    let check alphabet n hashed =
      check_string ~expected:hashed (hash ~alphabet n);
      check_int ~expected:n (unhash ~alphabet hashed);
    in
    let make n =
      (* hash/unhash are just encoding/decoding of integer in some base when alphabet is 012...(base-1),
      so we reuse the octal, decimal and hexadecimal conversions of the Format module. *)
      ~: "%i" n (lazy (
        check "01234567" n (Frmt.apply "%o" n);
        check "0123456789" n (Frmt.apply "%i" n);
        check "0123456789abcdef" n (Frmt.apply "%x" n);
      ))
    in Li.map ~f:make [0; 1; 2; 5; 7; 8; 9; 10; 11; 15; 16; 20; 31; 32; 39; 40; 63; 64]
  )
end


let box ~min_length ~alphabet ~guards ~seed =
  let guards_size = Str.size guards in
  let enough hashid =
    Str.size hashid >= min_length
  and guard hashid index =
    let index = (seed + Ch.to_int hashid.[index]) mod guards_size in
    Str.of_char guards.[index]
  in
  (* @todo Can we avoid many string concatenations? We're often adding a single char *)
  let guard_front hashid = guard hashid 0 ^ hashid
  and guard_back hashid = hashid ^ guard hashid 2
  and pad =
    let half = Str.size alphabet / 2 in
    let rec loop alphabet hashid =
      let alphabet = shuffle ~salt:alphabet alphabet in
      let hashid =
        (Str.drop_prefix' alphabet ~len:half) ^ hashid ^ (Str.prefix alphabet ~len:half)
      in
      if enough hashid then
        hashid
      else
        loop alphabet hashid
    in
    loop alphabet
  and trim hashid =
    let excess = Str.size hashid - min_length in
    if excess > 0 then
      let start_pos = excess / 2 in
      Str.substring hashid ~pos:start_pos ~len:min_length
    else
      hashid
  and if_not_enough f hashid =
    if enough hashid then
      hashid
    else
      f hashid
  in
  fun hashid ->
    hashid
    |> if_not_enough guard_front
    |> if_not_enough guard_back
    |> if_not_enough (fun hashid -> hashid |> pad |> trim)

let unbox ~guards =
  let guards = Str.to_list guards in
  function hashid ->
    match Str.split' ~seps:guards hashid with
      | _::hashid::_ | hashid::_ -> hashid
      | [] -> Exn.failure "Str.split' returned empty list" (*BISECT-IGNORE*)

module BoxTests = struct
  open Tst

  let test = "box/unbox" >:: (
    let make min_length alphabet guards seed unboxed boxed =
      ~: "%i %S %S %i %S -> %S" min_length alphabet guards seed unboxed boxed (lazy (
        check_string ~expected:boxed (box ~min_length ~alphabet ~guards ~seed unboxed);
        check_string ~expected:unboxed (unbox ~guards boxed);
      ))
    in [
      make 0 "abcd" "hij" 42 "" "";
      make 0 "abcd" "hij" 42 "vwxyz" "vwxyz";
      make 2 "abcd" "hij" 42 "vwxyz" "vwxyz";
      make 5 "abcd" "hij" 42 "vwxyz" "vwxyz";
      make 6 "abcd" "hij" 40 "vwxyz" "jvwxyz";
      make 6 "abcd" "hij" 41 "vwxyz" "hvwxyz";
      make 6 "abcd" "hij" 42 "vwxyz" "ivwxyz";
      make 7 "abcd" "hij" 40 "vwxyz" "jvwxyzh";
      make 7 "abcd" "hij" 41 "vwxyz" "hvwxyzi";
      make 7 "abcd" "hij" 42 "vwxyz" "ivwxyzj";
      make 8 "abcd" "hij" 42 "vwxyz" "civwxyzj";
      make 9 "abcd" "hij" 42 "vwxyz" "civwxyzjb";
      make 10 "abcd" "hij" 42 "vwxyz" "acivwxyzjb";
      make 11 "abcd" "hij" 42 "vwxyz" "acivwxyzjbd";
      make 12 "abcd" "hij" 42 "vwxyz" "dacivwxyzjbd";
      make 13 "abcd" "hij" 42 "vwxyz" "dacivwxyzjbda";
      make 14 "abcd" "hij" 42 "vwxyz" "cdacivwxyzjbda";
    ]
  )
end


let encode ~salt ~alphabet ~seps ~guards ~min_length =
  let alphabet_size = Str.size alphabet in
  function
    | [] -> ""
    | xs ->
      let seed = Li.fold_i ~init:0 ~f:(fun ~i seed x ->
        if x < 0 then Exn.invalid_argument "negative integer (Hashids can encode only positive integers)";
        seed + x mod (i + 100)) xs
      in
      let lottery = Str.of_char alphabet.[seed mod alphabet_size] in
      let (hashid, alphabet) =
        xs
        |> Li.fold_i ~init:(lottery, alphabet) ~f:(fun ~i (hashid, alphabet) x ->
          let salt = Str.prefix (lottery ^ salt ^ alphabet) ~len:alphabet_size in
          let alphabet = shuffle ~salt alphabet in
          let hashed = hash x ~alphabet in
          let sep = seps.[x mod (Ch.to_int hashed.[0] + i) mod (Str.size seps)] in
          let hashid = hashid ^ hashed ^ (Str.of_char sep) in
          (hashid, alphabet)
        )
      in
      Str.drop_suffix' hashid ~len:1
      |> box ~min_length ~alphabet ~guards ~seed

let decode ~salt ~alphabet ~seps ~guards =
  let seps = Str.to_list seps
  and alphabet_size = Str.size alphabet
  and cut s ~i = (Str.prefix s ~len:i, Str.drop_prefix' s ~len:i) in
  function
    | "" -> []
    | hashid ->
      let (lottery, hashid) =
        hashid
        |> unbox ~guards
        |> cut ~i:1
      in
      hashid
      |> Str.split' ~seps
      |> Li.fold_acc ~acc:alphabet ~init:[] ~f:(fun ~acc:alphabet xs hashed ->
        let salt = Str.prefix (lottery ^ salt ^ alphabet) ~len:alphabet_size in
        let alphabet = shuffle ~salt alphabet in
        let x = unhash hashed ~alphabet in
        let xs = x::xs in
        (alphabet, xs)
      )
      |> Li.reverse

module EncodeTests = struct
  open Tst

  let test = "encode/decode" >:: (
    let make salt alphabet seps guards min_length xs hashid =
      let encode = encode ~salt ~alphabet ~seps ~guards ~min_length
      and decode = decode ~salt ~alphabet ~seps ~guards in
      ~: "%S %S %S %S %i -> %S" salt alphabet seps guards min_length hashid (lazy (
        check_string ~expected:hashid (encode xs);
        check_int_list ~expected:xs (decode hashid);
      ))
    in [
      "negative" >: (lazy (
        let encode = encode ~salt:"salt" ~alphabet:"abcde" ~seps:"hijk" ~guards:"xyz" ~min_length:0 in
        expect_exception
          ~expected:(Invalid_argument "negative integer (Hashids can encode only positive integers)")
          (lazy (encode [-1]))
      ));
      make "" "abcde" "hijk" "xyz" 0 [] "";
      make "" "abcde" "hijk" "xyz" 4 [] "";
      make "" "abcde" "hijk" "xyz" 0 [42] "cead";
      make "" "abcde" "hijk" "xyz" 12 [42] "daebxceadzdc";
      make "0123" "abcde" "hijk" "xyz" 0 [42] "cabd";
      make "0123" "abcde" "hijk" "xyz" 12 [42] "eecdxcabdyab";
      make "" "abcde" "hijk" "xyz" 0 [42; 57] "edeajebe";
      make "" "abcde" "hijk" "xyz" 12 [42; 57] "aezedeajebey";
      make "0123" "abcde" "hijk" "xyz" 0 [42; 57] "edabjdad";
      make "0123" "abcde" "hijk" "xyz" 12 [42; 57] "cdzedabjdady";
      make "" "abcde" "hijk" "xyz" 0 [42; 57; 72; 25] "becbjacaibebhecc";
      make "" "abcde" "hijk" "xyz" 25 [42; 57; 72; 25] "bccadxbecbjacaibebheccxeb";
      make "0123" "abcde" "hijk" "xyz" 0 [42; 57; 72; 25] "bcedjedeiacahaee";
      make "0123" "abcde" "hijk" "xyz" 25 [42; 57; 72; 25] "beedbxbcedjedeiacahaeeyac";
    ]
  )
end


let preprocess =
  let all_seps = "cfhistuCFHISTU"
  and length_of_ratio ratio ~alphabet =
    Int.of_float (Fl.ceil (Fl.of_int (Str.size alphabet) /. ratio))
  in
  let split_seps alphabet =
    (* Multiple responsibility (Hum, it's weird but still feels appropriate to do it in one single loop):
      - detect spaces
      - partition in ("not in all_seps", "in all_seps")
      - keep each char only once
      - detect if not enough chars *)
    let (alphabet, seen_seps, seen) =
      let all_seps = all_seps |> Str.to_list |> ChSoSet.of_list in
      Str.fold alphabet ~init:([], ChSoSet.empty, ChSoSet.empty) ~f:(fun (alphabet, seps, seen) v ->
        if v = ' ' then Exn.invalid_argument "alphabet contains space (Hashids cannot contains spaces)";
        if ChSoSet.contains seen ~v then (alphabet, seps, seen) else
        let seen = ChSoSet.replace seen ~v in
        if ChSoSet.contains all_seps ~v then
          (alphabet, ChSoSet.replace seps ~v, seen)
        else
          (v::alphabet, seps, seen)
      )
    in
    if ChSoSet.size seen < 16 then
      Exn.invalid_argument "alphabet too short (Hashids requires at least 16 distinct characters)"
    else
      let alphabet =
        alphabet
        |> Li.reverse
        |> Str.of_list
      and seps = Str.filter all_seps ~f:(fun v -> ChSoSet.contains seen_seps ~v) in
      (alphabet, seps)
  and complete_seps alphabet seps =
    let seps_min_length = length_of_ratio ~alphabet 3.5
    and seps_length = Str.size seps in
    if seps_length < seps_min_length then
      (* In the Python and Java implementations, they ensure that seps_min_length >= 2
      but to have seps_min_length = 1, we need length alphabet <= 3 and since we've
      already checked that length alphabet + length seps >= 16, then we must have
      length seps >= 13 and it's impossible to be in this branch where length seps < 1. *)
      let diff = seps_min_length - seps_length in
      let seps = seps ^ (Str.prefix alphabet ~len:diff)
      and alphabet = Str.drop_prefix' alphabet ~len:diff in
      (alphabet, seps)
    else
      (alphabet, seps)
  and make_guards alphabet seps =
    let guards_size = length_of_ratio ~alphabet 12. in
    if Str.size alphabet < 3 then
      let guards = Str.prefix seps ~len:guards_size
      and seps = Str.drop_prefix' seps ~len:guards_size
      in (alphabet, seps, guards)
    else
      let guards = Str.prefix alphabet ~len:guards_size
      and alphabet = Str.drop_prefix' alphabet ~len:guards_size
      in (alphabet, seps, guards)
  in
  fun ~salt ~alphabet ->
    let (alphabet, seps) = split_seps alphabet in
    let seps = shuffle seps ~salt in
    let (alphabet, seps) = complete_seps alphabet seps in
    let alphabet = shuffle alphabet ~salt in
    make_guards alphabet seps

module PreprocessTests = struct
  open Tst

  let test = "preprocess" >:: (
    let make salt alphabet expected_alphabet expected_seps expected_guards =
      ~: "%S %S -> %S %S %S" salt alphabet expected_alphabet expected_seps expected_guards (lazy (
        let (alphabet, seps, guards) = preprocess ~salt ~alphabet in
        check_string ~expected:expected_alphabet alphabet;
        check_string ~expected:expected_seps seps;
        check_string ~expected:expected_guards guards;
      ))
    in [
      "too short" >: (lazy (expect_exception
        ~expected:(Invalid_argument "alphabet too short (Hashids requires at least 16 distinct characters)")
        (lazy (preprocess ~salt:"" ~alphabet:"0123456789abcde"))));
      "duplicate => too short" >: (lazy (expect_exception
        ~expected:(Invalid_argument "alphabet too short (Hashids requires at least 16 distinct characters)")
        (lazy (preprocess ~salt:"" ~alphabet:"0123456789abcdee"))));
      "space" >: (lazy (expect_exception
        ~expected:(Invalid_argument "alphabet contains space (Hashids cannot contains spaces)")
        (lazy (preprocess ~salt:"" ~alphabet:"0123456789 abcdef"))));
      make "" "0123456789abcdef" "3456789abde" "cf01" "2";
      make "" "0123456789abcdef0123456789abcdef" "3456789abde" "cf01" "2";
      make "salt" "0123456789abcdef" "8e7ab43592d" "fc01" "6";
      make "other salt" "0123456789abcdef" "93aed582764" "fc01" "b";
      "alphabet without standard separators" >:: (let alphabet = "abdegjklmnopqrvwxyzABDEGJKLMNOPQRVWXYZ1234567890" in [
        make "" alphabet "yzABDEGJKLMNOPQRVWXYZ1234567890" "abdegjklmnopqr" "vwx";
        make "salt" alphabet "zJWKwLQM7PX3Z120G6AVNB8yxR5O4Y9" "abdegjklmnopqr" "EvD";
        make "other salt" alphabet "VLXDvO2YMEw985RGB6xPQKNJz473y01" "abdegjklmnopqr" "ZAW";
      ]);
      "alphabet with one standard separator" >:: (let alphabet = "abcdegjklmnopqrvwxyzABDEGJKLMNOPQRVWXYZ1234567890" in [
        make "" alphabet "xyzABDEGJKLMNOPQRVWXYZ1234567890" "cabdegjklmnopq" "rvw";
        make "salt" alphabet "EzOJr73X9Z1wyP8K5QYVv6BGR0WA4ML2" "cabdegjklmnopq" "xND";
        make "other salt" alphabet "O7N23RL4Yz5VDZ6G10P8vK9rxwAyEWMQ" "cabdegjklmnopq" "BXJ";
      ]);
      "alphabet with almost only standard separators" >:: (let alphabet = "cfhistuCFHISTU01" in [
        make "" alphabet "01" "fhistuCFHISTU" "c";
        make "salt" alphabet "10" "iuUCFSThctfIH" "s";
        make "other salt" alphabet "10" "StIischHCuTFf" "U";
      ]);
      "alphabet with standard separators in reverse order" >:: (let alphabet = "!\"#%&',-/0123456789:;<=>ABCDEFGHIJKLMNOPQRSTUVWXYZ_`abcdefghijklmnopqrstuvwxyz~" in [
        make "" alphabet "123456789:;<=>ABDEGJKLMNOPQRVWXYZ_`abdegjklmnopqrvwxyz~" "cfhistuCFHISTU!\"#%&" "',-/0";
        make "salt" alphabet "Kj0<vLwZ'Ebo9kJe3V:Y~OaGPlMq6-7mdzxA,_`;n4WXNRy/>g2pQBr" "siuUCFSThctfIH!\"#%&" "D8=51";
        make "other salt" alphabet "Q-n8NPoWdJX'1Yzg/B;52OZLy=aqwKk4G<MVr>E,9xb:7`~6AD0R_vj" "UStIischHCuTFf!\"#%&" "lpm3e";
      ]);
    ]
  )
end


type t = {
  salt: string;
  alphabet: string;
  seps: string;
  min_length: int;
  guards: string;
}

let make ?(salt="") ?(min_length=0) ?(alphabet="abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890") () =
  let min_length = Int.max 0 min_length
  and (alphabet, seps, guards) = preprocess ~salt ~alphabet in
  {salt; alphabet; seps; min_length; guards}

let encode {salt; alphabet; seps; min_length; guards} xs =
  encode ~salt ~alphabet ~seps ~guards ~min_length xs

let decode {salt; alphabet; seps; min_length=_; guards} xs =
  decode ~salt ~alphabet ~seps ~guards xs

module PublicInterfaceTests = struct
  open Tst

  let test = "public interface" >:: [
    "alphabet too short" >: (lazy (expect_exception
      ~expected:(Invalid_argument "alphabet too short (Hashids requires at least 16 distinct characters)")
      (lazy (make ~alphabet:"abcdefghijklmno" ()))
    ));
    "negative" >: (
      let config = make () in
      lazy (expect_exception
        ~expected:(Invalid_argument "negative integer (Hashids can encode only positive integers)")
        (lazy (encode config [-1]))
      )
    );
    "encode/decode" >:: (
      let success name ?salt ?min_length ?alphabet xs encoded =
        let config = make ?salt ?min_length ?alphabet () in
        ~: "%s: %s" name encoded (lazy (
          check_string ~expected:encoded (encode config xs);
          check_int_list ~expected:xs (decode config encoded);
        ))
      in [
        success "empty" [] "";
        success "default" [0] "gY";
        success "default" [1] "jR";
        success "default" [22] "Lw";
        success "default" [333] "Z0E";
        success "default" [9999] "w0rR";
        success "default" [12345] "j0gW";
        success "default" [1; 2; 3] "o2fXhV";
        success "default" [683; 94108; 123; 5] "vJvi7On9cXGtD";
        success "default" [2; 4; 6] "xGhmsW";
        success "default" [99; 25] "3lKfD";
        success "default" [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20; 21; 22; 23; 24] "3RcpfqhKi5s0t0u6CQFKHRIqS0TPU2cofrhRi8sjtLuzCGFoHE";
        success "salt" ~salt:"this is my salt" [1] "NV";
        success "salt" ~salt:"this is my salt" [2] "6m";
        success "salt" ~salt:"this is my salt" [3] "yD";
        success "salt" ~salt:"this is my salt" [4] "2l";
        success "salt" ~salt:"this is my salt" [5] "rD";
        success "salt" ~salt:"this is my salt" [12345] "NkK9";
        success "salt" ~salt:"this is my salt" [9007199254740992] "262mm1m4J9Z";
        success "salt" ~salt:"this is my salt" [683; 94108; 123; 5] "aBMswoO2UB3Sj";
        success "salt" ~salt:"Arbitrary string" [683; 94108; 123; 5] "QWyf8yboH7KT2";
        success "salt" ~salt:"Arbitrary string" [1; 2; 3] "neHrCa";
        success "salt" ~salt:"Arbitrary string" [2; 4; 6] "LRCgf2";
        success "salt" ~salt:"Arbitrary string" [99; 25] "JOMh1";
        success "all" ~salt:"this is my salt" ~min_length:0 ~alphabet:"0123456789abcdef" [1234567] "b332db5";
        success "alphabet" ~alphabet:"!\"#%&',-/0123456789:;<=>ABCDEFGHIJKLMNOPQRSTUVWXYZ_`abcdefghijklmnopqrstuvwxyz~" [2839; 12; 32; 5] "_nJUNTVU3";
        success "alphabet" ~alphabet:"!\"#%&',-/0123456789:;<=>ABCDEFGHIJKLMNOPQRSTUVWXYZ_`abcdefghijklmnopqrstuvwxyz~" [1; 2; 3] "7xfYh2";
        success "alphabet" ~alphabet:"!\"#%&',-/0123456789:;<=>ABCDEFGHIJKLMNOPQRSTUVWXYZ_`abcdefghijklmnopqrstuvwxyz~" [23832] "Z6R>";
        success "alphabet" ~alphabet:"!\"#%&',-/0123456789:;<=>ABCDEFGHIJKLMNOPQRSTUVWXYZ_`abcdefghijklmnopqrstuvwxyz~" [99; 25] "AYyIB";
        success "min_length" ~min_length:25 [] "";
        success "min_length" ~min_length:25 [0] "r9JOyLkQWjnegYbwZ1p0GDXNm";
        success "min_length" ~min_length:25 [1] "kL39J4q2VolejRejNmGQBW71g";
        success "min_length" ~min_length:25 [2] "2Pr1gO3GWpmbk5ezJn4KRjLMv";
        success "min_length" ~min_length:25 [7452; 2967; 21401] "pO3K69b86jzc6krI416enr2B5";
        success "min_length" ~min_length:25 [1; 2; 3] "gyOwl4B97bo2fXhVaDR0Znjrq";
        success "min_length" ~min_length:25 [6097] "Nz7x3VXyMYerRmWeOBQn6LlRG";
        success "min_length" ~min_length:25 [99; 25] "k91nqP3RBe3lKfDaLJrvy8XjV";
        success "all" ~salt:"arbitrary salt" ~min_length:16 ~alphabet:"abcdefghijklmnopqrstuvwxyz" [7452; 2967; 21401] "wygqxeunkatjgkrw";
        success "all" ~salt:"arbitrary salt" ~min_length:16 ~alphabet:"abcdefghijklmnopqrstuvwxyz" [1; 2; 3] "pnovxlaxuriowydb";
        success "all" ~salt:"arbitrary salt" ~min_length:16 ~alphabet:"abcdefghijklmnopqrstuvwxyz" [60125] "jkbgxljrjxmlaonp";
        success "all" ~salt:"arbitrary salt" ~min_length:16 ~alphabet:"abcdefghijklmnopqrstuvwxyz" [99; 25] "erdjpwrgouoxlvbx";
        success "alphabet without standard separators" ~alphabet:"abdegjklmnopqrvwxyzABDEGJKLMNOPQRVWXYZ1234567890" [7452; 2967; 21401] "X50Yg6VPoAO4";
        success "alphabet without standard separators" ~alphabet:"abdegjklmnopqrvwxyzABDEGJKLMNOPQRVWXYZ1234567890" [1; 2; 3] "GAbDdR";
        success "alphabet without standard separators" ~alphabet:"abdegjklmnopqrvwxyzABDEGJKLMNOPQRVWXYZ1234567890" [60125] "5NMPD";
        success "alphabet without standard separators" ~alphabet:"abdegjklmnopqrvwxyzABDEGJKLMNOPQRVWXYZ1234567890" [99; 25] "yGya5";
        success "alphabet without standard separators plus salt" ~salt:"salt" ~alphabet:"abdegjklmnopqrvwxyzABDEGJKLMNOPQRVWXYZ1234567890" [99; 25] "zZRnx";
        success "alphabet with standard separators" ~alphabet:"abdegjklmnopqrvwxyzABDEGJKLMNOPQRVWXYZ1234567890uC" [7452; 2967; 21401] "GJNNmKYzbPBw";
        success "alphabet with standard separators" ~alphabet:"abdegjklmnopqrvwxyzABDEGJKLMNOPQRVWXYZ1234567890uC" [1; 2; 3] "DQCXa4";
        success "alphabet with standard separators" ~alphabet:"abdegjklmnopqrvwxyzABDEGJKLMNOPQRVWXYZ1234567890uC" [60125] "38V1D";
        success "alphabet with standard separators" ~alphabet:"abdegjklmnopqrvwxyzABDEGJKLMNOPQRVWXYZ1234567890uC" [99; 25] "373az";
      ]
    );
  ];
end


module Tests = struct
  open Tst

  let test = "Hashids" >:: [
    ShuffleTests.test;
    HashTests.test;
    BoxTests.test;
    EncodeTests.test;
    PreprocessTests.test;
    PublicInterfaceTests.test;
  ]
end
