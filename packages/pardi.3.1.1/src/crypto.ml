
module Ht = BatHashtbl
module Log = Dolog.Log

(* create fresh nonces / check their freshness *)
module Nonce_store = struct

  let ht = Ht.create 100_003

  let fresh (counter: int ref): string =
    if !counter = -1 then failwith "Nonce_store.fresh: counter has looped";
    Ht.add ht !counter ();
    let nonce = string_of_int !counter in
    incr counter;
    nonce

  let is_fresh (nonce: string): bool =
    let i = int_of_string nonce in
    if Ht.mem ht i then
      false
    else
      let () = Ht.add ht i () in
      true
end

let compression_flag = ref true

let compress (msg: string): string =
  if !compression_flag then
    Lz4.compress msg
  else
    msg

let uncompress (msg: string): string =
  if !compression_flag then
    Lz4.decompress msg
  else
    msg

(* signature of the message: length 20B = 160bits *)
let sign (sign_key: string) (msg: string): string =
  let signer = Cryptokit.MAC.hmac_ripemd160 sign_key in
  signer#add_string msg;
  let signature = signer#result in
  signer#wipe;
  assert(String.length sign_key >= 20 && String.length signature = 20);
  signature

let ignore_fst _a b =
  b

(* optionally return the message without its prefix signature, or None
   if the signature is incorrect or anything strange was found *)
let check_sign (sign_key: string) (msg: string): string option =
  let n = String.length msg in
  if n <= 21 then (* 20B signature plus at least 1B message *)
    ignore_fst (Log.error "Crypto.check_sign: message too short: %d" n) None
  else
    let prev_sign = String.sub msg 0 20 in
    let signless = String.sub msg 20 (n - 20) in
    let curr_sign = sign sign_key signless in
    if curr_sign <> prev_sign then
      ignore_fst (Log.error "Crypto.check_sign: bad signature") None
    else
      Some signless

(* with chacha20, encryption is the same operation as decryption (xoring with
   a pseudo random stream) *)
let transform (cipher_key: string) (stream_offs: int64 ref) (msg: string): string =
  let n = String.length msg in
  assert(n > 0 && String.length cipher_key = 16 && !stream_offs >= Int64.zero);
  let src_dst = Bytes.of_string msg in
  let chacha20 = new Cryptokit.Stream.chacha20 ~ctr:!stream_offs cipher_key in
  chacha20#transform src_dst 0 src_dst 0 n;
  chacha20#wipe;
  (* update stream offset *)
  stream_offs := Int64.add !stream_offs (Int64.of_int n);
  Bytes.unsafe_to_string src_dst

(* encrypt-then-sign scheme *)
let encode (sign_key: string) (cipher_key: string) (stream_offs: int64 ref)
    (rng: Cryptokit.Random.rng)
    (counter: int ref)
    (m: 'a): string =
  let plain_text = Marshal.to_string m [Marshal.No_sharing] in
  let maybe_compressed = compress plain_text in
  let salt = Bytes.make 8 '0' in (* 64 bits salt *)
  rng#random_bytes salt 0 8;
  (* let salt_hex = Utils.convert `To_hexa salt in *)
  (* Log.debug "enc. salt = %s" salt_hex; *)
  let nonce = Nonce_store.fresh counter in
  (* Log.debug "enc. nonce = %s" nonce; *)
  let s_n_m = (Bytes.unsafe_to_string salt) ^ nonce ^ "|" ^ maybe_compressed in
  let encrypted = transform cipher_key stream_offs s_n_m in
  (sign sign_key encrypted) ^ encrypted

(* check-sign-then-decrypt scheme *)
let decode (sign_key: string) (cipher_key: string) (stream_offs: int64 ref)
    (s: string): 'a option =
  match check_sign sign_key s with
  | None -> None
  | Some encrypted ->
    let str = transform cipher_key stream_offs encrypted in
    (* leading salt (8 first bytes) is ignored *)
    (* let salt = String.sub str 0 8 in
       * let salt_hex = Utils.convert `To_hexa salt in
       * Log.debug "dec. salt = %s" salt_hex; *)
    let n = String.length str in
    let nonce_end = String.index_from str 8 '|' in
    assert(nonce_end > 8 && nonce_end < n);
    let nonce = String.sub str 8 (nonce_end - 8) in
    (* Log.debug "dec. nonce = %s" nonce; *)
    if Nonce_store.is_fresh nonce then
      let compressed = BatString.lchop ~n:(nonce_end + 1) str in
      let u = uncompress compressed in
      Some (Marshal.from_string u 0: 'a)
    else
      ignore_fst (Log.error "Crypto.decode: nonce already seen: %s" nonce) None
