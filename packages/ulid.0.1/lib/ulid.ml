(*
  ULID is [48 bits timestamp] + [80 bits random]
  encoded in Crockford's base32
*)
let encoding =
  [| '0'
   ; '1'
   ; '2'
   ; '3'
   ; '4'
   ; '5'
   ; '6'
   ; '7'
   ; '8'
   ; '9'
   ; 'A'
   ; 'B'
   ; 'C'
   ; 'D'
   ; 'E'
   ; 'F'
   ; 'G'
   ; 'H'
   ; 'J'
   ; 'K'
   ; 'M'
   ; 'N'
   ; 'P'
   ; 'Q'
   ; 'R'
   ; 'S'
   ; 'T'
   ; 'V'
   ; 'W'
   ; 'X'
   ; 'Y'
   ; 'Z' |]

let encoding_len = Array.length encoding

let time_len = 10

let random_len = 16

let rng_init = ref false

let get_nocrypto_rng () =
  let () =
    if !rng_init = false then (
      Nocrypto_entropy_unix.initialize () ;
      rng_init := true )
  in
  let rng int = Nocrypto.Rng.Int.gen int in
  rng

let random_char prng () =
  let idx = prng encoding_len in
  encoding.(idx)

let rec find_in_array a x n =
  if n >= Array.length a then -1
  else if a.(n) = x then n
  else find_in_array a x (n + 1)

let encode_time now len =
  let str = Buffer.create len in
  let rec enc time length s =
    if length = 0 then Buffer.contents s
    else
      let m = time mod encoding_len in
      Buffer.add_char s encoding.(m) ;
      enc ((time - m) / encoding_len) (length - 1) s
  in
  let s = enc now len str in
  String.init len (fun i -> s.[len - 1 - i])

let encode_random len prng =
  let str = Buffer.create len in
  let rec enc length s =
    if length = 0 then Buffer.contents s
    else (
      Buffer.add_char s (random_char prng ()) ;
      enc (length - 1) s )
  in
  let s = enc len str in
  String.init len (fun i -> s.[len - 1 - i])

let replace_char_at str idx c =
  if idx > Bytes.length str - 1 then str
  else
    let () = Bytes.set str idx c in
    str

let increment_base_32 str =
  let initial = Bytes.of_string str in
  let index = String.length str - 1 in
  let max_char_index = encoding_len - 1 in
  let rec incr finished curr_idx s f =
    if finished || curr_idx < 0 then Bytes.to_string f
    else
      let char = Bytes.get s curr_idx in
      let char_index = find_in_array encoding char 0 in
      if char_index = max_char_index then
        let new_s = replace_char_at s curr_idx encoding.(0) in
        incr false (curr_idx - 1) new_s f
      else
        let new_f = replace_char_at s curr_idx encoding.(char_index + 1) in
        incr true (curr_idx - 1) s new_f
  in
  incr false index initial initial

let get_now () = int_of_float (1000. *. Unix.gettimeofday ())

let ulid ?(seed_time = get_now ()) () =
  let s = Buffer.create (time_len + random_len) in
  Buffer.add_string s (encode_time seed_time time_len) ;
  Buffer.add_string s (encode_random random_len (get_nocrypto_rng ())) ;
  Buffer.contents s

let ulid_factory ?(prng = get_nocrypto_rng ()) () ?(seed_time = get_now ()) ()
    =
  let s = Buffer.create (time_len + random_len) in
  Buffer.add_string s (encode_time seed_time time_len) ;
  Buffer.add_string s (encode_random random_len prng) ;
  Buffer.contents s

let monotonic_factory ?(prng = get_nocrypto_rng ()) () =
  let last_time = ref 0 in
  let last_random = ref "" in
  fun ?(seed_time = get_now ()) () ->
    let s = Buffer.create (time_len + random_len) in
    let () =
      if seed_time <= !last_time then
        last_random := increment_base_32 !last_random
      else (
        last_time := seed_time ;
        last_random := encode_random random_len prng )
    in
    Buffer.add_string s (encode_time !last_time time_len) ;
    Buffer.add_string s !last_random ;
    Buffer.contents s
