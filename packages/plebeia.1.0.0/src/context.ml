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
module Int64 = Stdint.Int64
  
(* XXX Problem in 32bit arch
     
   * [Cstruct.of_bigarray] only takes [char] indexed [Bigstring.t].
   * Offset must be [int] in [Cstruct].
   
   The current simple implementation to map the entire file to one [Bigstring.t]
   restricts the maximum file size in 32bit arch to [1_073_741_823], which is roughly just 1GB.
*)

type t = 
  { storage : Storage.t
  ; hashcons : Hashcons.t (* Hashcons tbl *)
  ; stat : Stat.t (* Statistics *)
  }

let mode t = Storage.mode t.storage

let create ?(pos=0L) ?length ?hashcons fn =
  let storage = Storage.create ~pos ?length fn in
  let hashcons = Hashcons.create (Option.default Hashcons.default_config hashcons) in
  { storage
  ; hashcons
  ; stat = Stat.create ()
  }

let ref_load_leaf_value : (t -> Index.t -> Value.t option) ref = ref (fun _ -> assert false)

let open_ ?(pos=0L) ?hashcons ~mode fn =
  let storage = Storage.open_ ~pos ~mode fn in
  let hashcons = Hashcons.create (Option.default Hashcons.default_config hashcons) in
  { storage
  ; hashcons
  ; stat = Stat.create () 
  }

let close { storage ; _ } = Storage.close storage
  
