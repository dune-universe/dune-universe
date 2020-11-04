let key_length_bytes = 32

type priv = [ `Checked of Cstruct.t ]

let checked_buffer (`Checked cs) = Cstruct.to_bigarray cs

let priv cs =
  if Cstruct.len cs = key_length_bytes then `Checked cs
  else invalid_arg "Invalid length"

let encode_priv (`Checked cs) = cs

external s_to_p : Cstruct.buffer -> Cstruct.buffer -> unit
  = "ml_Hacl_Ed25519_secret_to_public"
  [@@noalloc]

let priv_to_public priv =
  let pub = Cstruct.create key_length_bytes in
  s_to_p (Cstruct.to_bigarray pub) (checked_buffer priv);
  pub

external ed25519_sign :
  Cstruct.buffer -> Cstruct.buffer -> Cstruct.buffer -> int -> unit
  = "ml_Hacl_Ed25519_sign"
  [@@noalloc]

external ed25519_verify :
  Cstruct.buffer -> Cstruct.buffer -> int -> Cstruct.buffer -> bool
  = "ml_Hacl_Ed25519_verify"
  [@@noalloc]

let sign priv msg =
  let signature = Cstruct.create 64 in
  (* SHA512 = 512 bit = 64 byte *)
  ed25519_sign
    (Cstruct.to_bigarray signature)
    (checked_buffer priv) (Cstruct.to_bigarray msg) (Cstruct.len msg);
  signature

let verify ~pub ~msg ~signature =
  ed25519_verify (Cstruct.to_bigarray pub) (Cstruct.to_bigarray msg)
    (Cstruct.len msg)
    (Cstruct.to_bigarray signature)
