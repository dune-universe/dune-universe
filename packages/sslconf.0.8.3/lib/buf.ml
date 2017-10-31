(*---------------------------------------------------------------------------
   Copyright (c) 2017 Tony Wuersch. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   sslconf 0.8.3
  ---------------------------------------------------------------------------*)

(* Buffer procedures.
   Allocation for one buffer is limited to 2**[nbits]-1 bytes.

   This code should mimic openssl crypto/buffer/buffer.c
*)

open Rresult

(* nbits, length, data *)
type t = int * int * Bytes.t

(* an empty buffer (length 0, max 0) *)
let empty nbits = nbits, 0, Bytes.empty

(* to get a Bytes.buffer from a Buf.t *)
let data (_, _, data) = data

(* grow buffer to allow for a given length, and set to null characters *)
let extend (nbits, length, data) len =
  if len <= length
  then
    Ok (nbits, len, data)
  else
    let allocated = Bytes.length data in
    let result =
      if len <= allocated
      then Ok data
      else if nbits < 3
      then Error ("Invalid_argument: too few bits", nbits, len, 0)
      else
        (*
          standard expansion is (len+3)/3*4.
          limit_before_expansion is max n such that (n+3)/3*4 < 2**31.
         *)
        let limit = (1 lsl (nbits - 2)) * 3 - 4 in
        if len > limit
        then
          let msg = "len_exceeds_limit" in
          Error (msg, nbits, len, limit)
        else
          let to_allocate = (len + 3) / 3 * 4 in
          try
            Ok (Bytes.extend data 0 (to_allocate - allocated))
          with
            Invalid_argument s ->                    (*BISECT-IGNORE*)
            let msg = "invalid_argument: " ^ s in  (*BISECT-IGNORE*)
            Error (msg, nbits, len, limit)         (*BISECT-IGNORE*)
    in
    match result with
      Ok data ->
      Bytes.fill data length (len - length) '\x00';
      Ok (nbits, len, data)
    | Error (msg, nbits, len, limit) -> Error (msg, nbits, len, limit)

(*---------------------------------------------------------------------------
   Copyright (c) 2017 Tony Wuersch

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
