module String = Core.String
module AES = Nocrypto.Cipher_block.AES.CBC

let __kdf key =
  let aes_salt = Entropy.min_bits (32 * 8) in
  let mac_salt = Entropy.max_bits (32 * 8) in
  let aes_key = Hardening.kdf ~size:32l ~salt:aes_salt key in
  let mac_key = Hardening.kdf ~size:32l ~salt:mac_salt key in
  (aes_key, mac_key)


let hash data = Cstruct.of_hex @@ Hashing.hash @@ Cstruct.to_string data

let mac ~key data =
  let key', data' = (Cstruct.to_string key, Cstruct.to_string data) in
  Cstruct.of_hex @@ Hashing.mac ~key:key' data'


let encrypt ~key ~iv ~metadata ~message:msg =
  let aes_key, mac_key = __kdf key in
  let aes_key' = AES.of_secret aes_key in
  let plaintext = Helpers.pad ~basis:16 @@ Cstruct.to_string msg in
  let ciphertext =
    AES.encrypt ~iv ~key:aes_key' @@ Cstruct.of_string plaintext
  in
  let secret = hash mac_key in
  let payload = Cstruct.concat [ metadata; iv; ciphertext ] in
  let tag = mac ~key:secret payload in
  (ciphertext, tag)


let decrypt ~reason ~key ~iv ~metadata ~cipher ~tag =
  let aes_key, mac_key = __kdf key in
  let secret = hash mac_key in
  let payload = Cstruct.concat [ metadata; iv; cipher ] in
  let tag' = mac ~key:secret payload in
  if Cstruct.equal tag tag'
  then
    let aes_key' = AES.of_secret aes_key in
    let plaintext = AES.decrypt ~iv ~key:aes_key' cipher in
    Cstruct.of_string @@ Helpers.unpad @@ Cstruct.to_string plaintext
  else raise reason
