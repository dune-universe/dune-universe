module Cs = Cstruct

type t = {
  mutable state: Cs.t;
  mutable buffer: Cs.t;
  hash: Cs.t -> Cs.t;
}

let expand key nonce =
  if Cs.length nonce <> 8 then invalid_arg "nonce must be 8 byte long"
  else
    let (s, k0, k1) = match Cs.length key with
        32 -> let (k0, k1) = Cs.split key 16 in ("expand 32-byte k", k0, k1)
      | 16 -> ("expand 16-byte k", key, key)
      | _ -> invalid_arg "key must be either 32 (recommended) or 16 byte long)" in
    let state = Cs.create 64 in
    Cs.blit_from_string s 0 state 0 4;
    Cs.blit k0 0 state 4 16;
    Cs.blit_from_string s 4 state 20 4;
    Cs.blit nonce 0 state 24 8;
    Cs.LE.set_uint64 state 32 Int64.zero;
    Cs.blit_from_string s 8 state 40 4;
    Cs.blit k1 0 state 44 16;
    Cs.blit_from_string s 12 state 60 4;
    state

let create ?(hash=Salsa20_core.salsa20_20_core) key nonce =
  let state = expand key nonce in
  let buffer = Cs.create 0 in
  {state; buffer; hash}

let hash state =
  state.buffer <- state.hash state.state;
  let nonce = Int32.add (Cs.LE.get_uint32 state.state 32) Int32.one in
  Cs.LE.set_uint32 state.state 32 nonce;
  if nonce = Int32.zero then
    let nonce = Int32.add (Cs.LE.get_uint32 state.state 36) Int32.one in
    Cs.LE.set_uint32 state.state 36 nonce

let encrypt input state =
  let l = Cs.length input in
  let output = Cs.create l in
  let i = ref 0 in
  while !i < l do
    if Cs.length state.buffer = 0 then hash state;
    let count = min (Cs.length state.buffer) (l - !i) in
    let buffer = Cs.create count in
    Cs.blit input !i buffer 0 count;
    Mirage_crypto.Uncommon.Cs.xor_into state.buffer buffer count;
    Cs.blit buffer 0 output !i count;
    i := !i + count;
    state.buffer <- Cs.shift state.buffer count
  done;
  output

let decrypt = encrypt
