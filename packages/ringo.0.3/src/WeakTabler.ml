(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module Make (H: Hashtbl.HashedType) : Sigs.HS with type key = H.t = struct

  type wkey = nativeint
  module WH = struct
    (* NOTE: So that it is scanned by the GC, the type [t] must be an
       allocated/boxed type. *)
    type t = wkey

    (* NOTE: the [nativeint] fed into this hash functions are the result of
       [Nativeint.of_int @@ M.hash v] (for arbitrary [v]s). We assume [M.hash]
       already has the expected properties of a good hash, most importantly that
       it avoids collisions, and we assert that [Nativeint.of_int] does not
       degrade those properties. *)
    let hash a = Nativeint.to_int a

    let equal = Nativeint.equal
  end

  module Table = Ephemeron.K1.Make (WH)

  type key = H.t
  type 'a t = 'a Table.t
  let create i = Table.create i
  let remove t k = Table.remove t (Nativeint.of_int @@ H.hash k)
  let find_opt t k = Table.find_opt t (Nativeint.of_int @@ H.hash k)
  let replace t k v = Table.replace t (Nativeint.of_int @@ H.hash k) v
  let length t = Table.length t
  let clear t = Table.clear t

end
