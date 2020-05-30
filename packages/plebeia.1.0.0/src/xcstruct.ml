(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019,2020 DaiLambda, Inc. <contact@dailambda.jp>            *)
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
open Stdint

include Cstruct
include Cstruct.LE (* Intel friendly *)

(* The original [uint32] functions of Cstruct returns **[int32]**.
   Very confusing, so we patch them here. *)
let get_uint32 buf x = Uint32.of_int32 @@ Cstruct.LE.get_uint32 buf x
let set_uint32 buf x v = Cstruct.LE.set_uint32 buf x @@ Uint32.to_int32 v
let get_index buf x = Index.of_uint32 @@ get_uint32 buf x
let set_index buf x v = set_uint32 buf x @@ Index.to_uint32 v
let get_hash buf pos = Hash.of_string @@ copy buf pos (pos+28)

(* Cstruct.blit_from_string, but make sure all the string contents are written *)
let write_string s buf off len =
  let slen = String.length s in
  if slen <> len then begin Log.fatal "write_string: %d <> %d" slen len; assert false end;
  blit_from_string s 0 buf off len


