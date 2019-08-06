module Reasons = Exceptions
module String = Core.String
module List = Core.List

let __concat_on separator left right = left ^ separator ^ right

let __join ~on list =
  list
  |> List.map ~f:Encoding.encode_blob
  |> List.reduce_exn ~f:(__concat_on on)


let commit payload =
  let key = Entropy.key () in
  let iv = Entropy.iv () in
  let metadata = Cstruct.of_string @@ Fingerprint.id () in
  let message = Cstruct.of_string @@ Encoding.encode payload in
  let cipher, tag = Encryption.encrypt ~key ~iv ~metadata ~message in
  let commitment = __join ~on:"@" [ metadata; iv; cipher; tag ] in
  let opening = Encoding.encode_blob key in
  (commitment, opening)


let __decode ~reason data =
  try Encoding.decode_as_blob data with _ -> raise reason


let __split ~reason ~on data =
  match String.split data ~on with
  | [ metadata; iv; cipher; tag ] ->
      let metadata' = __decode ~reason metadata in
      let iv' = __decode ~reason iv in
      let cipher' = __decode ~reason cipher in
      let tag' = __decode ~reason tag in
      (metadata', iv', cipher', tag')
  | _ ->
      raise reason


let reveal ~commitment ~opening =
  let open Reasons in
  let key = __decode ~reason:InvalidOpening opening in
  let metadata, iv, cipher, tag =
    __split ~reason:InvalidCommitment ~on:'@' commitment
  in
  let payload =
    Encryption.decrypt ~reason:BindingFailure ~key ~iv ~metadata ~cipher ~tag
  in
  Encoding.decode @@ Cstruct.to_string payload
