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
(** { 1 Plebeia }
                 
    Implementation of space-efficient binary Patricia trees in OCaml.
    The implementation is geared for used in Tezos, though it is rather
    generic. A stop-and-copy GC is provided. This implementation aims
    to maximize correctness and cares second about efficiency. *)

(** { 2 Internal implementation }
                                 
    For testing, debugging, and advance uses.
*)
module Internal = struct

  (** { 3 Base } *)
  module Error            = Error
  module Utils            = Utils
  module Option           = Option
  module Result           = Result
  module Monad            = Monad
  module Log              = Log

  (** { 3 Core } *)
  module Value            = Value
  module Index            = Index
  module Segment          = Segment
  module Context          = Context
  module Node             = Node
  module Node_tools       = Node_tools
  module Cursor           = Cursor

  (** { 3 Hash } *)
  module Hash             = Hash
  module Node_hash        = Node_hash
  module Cursor_hash      = Cursor_hash

  (** { 3 Storage } *)
  module Lock             = Lock
  module Storage          = Storage
  module Node_storage     = Node_storage
  module Cursor_storage   = Cursor_storage
  module Hashcons         = Hashcons
  module Bud_cache        = Bud_cache

  (** { 3 High level } *)
  module Deep             = Deep

  (** { 3 Version control } *)
  module Roots            = Roots
  module Vc               = Vc

  (** { 3 Helper } *)
  module Stat             = Stat
  module Debug            = Debug

  (** { 3 Experimental } *)
  module Traverse         = Traverse
  module Diff             = Diff
  module Deep_stat        = Deep_stat
  module Merkle_proof     = Merkle_proof
  module Cursor_tools     = Cursor_tools

  (** { 3 Deprecated } *)
  module Search           = Search
  module Ediff            = Ediff

  (** { 3 Test } *)
  module Rand = Rand
  module Gen = Gen
  module Test_utils = Test_utils

end

include (Internal : Plebeia_intf.S)
(** { 2 Standard APIs, module interfaces are restricted. } *)
