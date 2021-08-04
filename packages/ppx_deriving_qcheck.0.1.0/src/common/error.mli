(*****************************************************************************)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Valentin Chaboche                                      *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** Error management *)
open Ppxlib

(** Default location *)
val default_loc : location ref

(** Set default location *)
val set_loc : location -> unit

(** Internal error without message *)
val internal_error : ?loc:location -> unit -> 'a

(** Syntax error on parsing *)
val syntax_error : ?loc:location -> err:char -> unit -> 'a

(** Internal error when a pattern matching case is not supported *)
val case_unsupported : ?loc:location -> case:string -> unit -> 'a

(** Error when a pbt.property is not supported *)
val property_unsupported : ?loc:location -> property:string -> unit -> 'a

(** Error when a pbt.property misses generators *)
val property_gen_missing :
  ?loc:location -> property:string -> required:int -> actual:int -> unit -> 'a

(** Error when a pbt.property misses args *)
val property_arg_missing :
  ?loc:location -> property:string -> required:int -> actual:int -> unit -> 'a

(** Error on location with message *)
val location_error : ?loc:location -> msg:string -> unit -> 'a
