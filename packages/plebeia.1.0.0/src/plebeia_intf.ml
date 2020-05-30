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
module type S = sig

  (** {2 Base} *)

  module Error : module type of struct include Error end

  module Result : module type of struct include Result end

  module Value : module type of struct include Value end

  (** {2 Core} *)

  (** A segment represents a path from the root of a tree to a leaf or
      to the root of a sub-tree. *)
  module Segment : sig
    type side = Segment.side = Left | Right
    type t = Segment.t

    val to_string : t -> string
    (** LLRRLL *)

    val of_string : string -> t option
    (** LLRRLL *)

    val to_sides : t -> side list
    val of_sides : side list -> t
  end

  (** {2 Hash } *)

  module Hash : module type of Hash

  (** {2 High level} *)

  module Hashcons : sig
    type config = Hashcons.config =
      { max_leaf_size : int 
        (* Maximum size of leaf value stored *)
      ; max_words : int
      }

    val default_config : config
  end

  module Vc : module type of struct include Vc end

  module Roots : module type of struct include Roots end

  (** {2 Helper} *)

  module Stat : sig
    type t = Stat.t
    val create : unit -> t
    val pp : Format.formatter -> t -> unit
  end

  (** {2 Experimental} *)

  (** Calculate Merckle Proofs *)
  module Merkle_proof : module type of struct include Merkle_proof end

end
