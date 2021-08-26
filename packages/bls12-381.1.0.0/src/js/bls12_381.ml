(*****************************************************************************)
(*                                                                           *)
(* Copyright (c) 2020-2021 Danny Willems <be.danny.willems@gmail.com>        *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

module Fr = Fr
module Fq12 = Fq12
module G1 = G1
module G2 = G2
module Pairing = Pairing

module Signature = struct
  type sk = Bytes.t

  type pk = Bytes.t

  type signature = Bytes.t

  let sk_of_bytes_exn _sk = failwith "Not implemented"

  let sk_to_bytes _b = failwith "Not implemented"

  let unsafe_pk_of_bytes _ = failwith "Not implemented"

  let pk_of_bytes_exn _ = failwith "Not implemented"

  let pk_of_bytes_opt _ = failwith "Not implemented"

  let pk_to_bytes _ = failwith "Not implemented"

  let generate_sk ?key_info ikm =
    ignore key_info ;
    ignore ikm ;
    failwith "Not implemented"

  let derive_pk _sk = failwith "Not implemented"

  let aggregate_signature_opt _ = failwith "Not implemented"

  module Basic = struct
    let sign _sk _message = failwith "Not implemented"

    let verify _pk _msg _signature = failwith "Not implemented"

    let aggregate_verify _ _ = failwith "Not implemented"
  end

  module Aug = struct
    let sign _sk _message = failwith "Not implemented"

    let verify _pk _msg _signature = failwith "Not implemented"

    let aggregate_verify _ _ = failwith "Not implemented"
  end

  module Pop = struct
    type proof = Bytes.t

    let sign _sk _message = failwith "Not implemented"

    let verify _pk _msg _signature = failwith "Not implemented"

    let pop_prove _sk = failwith "Not implemented"

    let pop_verify _pk _signature = failwith "Not implemented"

    let aggregate_verify _ _ _ = failwith "Not implemented"
  end
end
