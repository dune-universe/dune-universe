module String = Core.String
module AES = Nocrypto.Cipher_block.AES.GCM

let encrypt ~key ~iv ~message:msg =
  let key = AES.of_secret key in
  let plaintext = Helpers.pad ~basis:16 msg in
  let result = AES.encrypt ~iv ~key @@ Cstruct.of_string plaintext in
  (result.AES.message, result.AES.tag)

let decrypt ~reason ~key ~iv ~cipher ~tag =
  let key = AES.of_secret key in
  let result = AES.decrypt ~iv ~key cipher in
  if Cstruct.equal tag result.AES.tag then
    Helpers.unpad @@ Cstruct.to_string result.AES.message
  else raise reason
