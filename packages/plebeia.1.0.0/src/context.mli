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
(** { 1 Merkle Patricia tree storage } *)

(** { 2 Types } *)

type t = 
  { storage : Storage.t
  ; hashcons : Hashcons.t (* Hashcons tbl *)
  ; stat : Stat.t         (* Statistics *)
  }

val create : 
  ?pos:int64
  -> ?length:int 
  -> ?hashcons: Hashcons.config
  -> string (* path *)
  -> t
(** Create a new context storage.  
    Note that if the file already exists, [create] fails.

    The context is created in Writer mode.

    pos: the start position in the file
    length: initial size of the file in bytes
*) 

val open_ : 
  ?pos:int64 
  -> ?hashcons: Hashcons.config
  -> mode:Storage.mode
  -> string (* path *)
  -> t
(** Open an existing context storage.

    pos: The start position in the file
*)

val close : t -> unit
(** Closes the context.  
                         
    If program exits or crashes without closing a context, some data 
    may be lost, even if they are written on the disk.
*)    

val mode : t -> Storage.mode
(** Returns writing mode *)

val ref_load_leaf_value : (t -> Index.t -> Value.t option) ref
(** Forward declaration to load a leaf value from context *)
