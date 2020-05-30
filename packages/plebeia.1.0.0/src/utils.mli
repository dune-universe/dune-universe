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
module Exn : sig
  val catch : ('a -> 'b) -> 'a -> ('b, [> `Exn of exn ]) result
  val protect : (unit -> 'a) -> (unit -> unit) -> 'a
end

module Format : sig
  include module type of struct include Format end

  type 'a t = formatter -> 'a -> unit
  val string : string t
  val list : (unit, formatter, unit) format -> 'a t -> 'a list t
  val option : 'a t -> 'a option t
end

module String : sig
  include module type of struct include String end

  val find_char : (char -> bool) -> string -> int -> int option
  val split_by_char : (char -> bool) -> string -> string list
  val for_all : (char -> bool) -> string -> bool
end

module Hashtbl : sig
  include module type of struct include Hashtbl end

  val alter : ('a, 'b) Hashtbl.t -> ('b option -> 'b option) -> 'a -> unit
end

module List : sig
  include module type of struct include List end

  val ( @ ) : 'a list -> 'a list -> 'a list
  (** Tail recursive *)
      
  val rev_concat : 'a list list -> 'a list
  (** Reversed versio of [concat].  Tail recursive *)

  val concat : 'a list list -> 'a list
  (** Tail recursive *)

  val map : ('a -> 'b) -> 'a list -> 'b list
  (** Tail recursive *)

  val split_at : int -> 'a list -> 'a list * 'a list
  val find_map_opt : ('a -> 'b option) -> 'a list -> 'b option
  val rev_filter_map : ('a -> 'b option) -> 'a list -> 'b list
  val filter_map : ('a -> 'b option) -> 'a list -> 'b list
  val rev_concat_map : ('a -> 'b list) -> 'a list -> 'b list
  val concat_map : ('a -> 'b list) -> 'a list -> 'b list
  val is_prefix : 'a list -> 'a list -> 'a list option
  val rev_group_by : ('a -> 'a -> bool) -> 'a list -> 'a list list
  val group_by : ('a -> 'a -> bool) -> 'a list -> 'a list list
  val rev_take : int -> 'a list -> 'a list
  val take_while : ('a -> bool) -> 'a list -> 'a list * 'a list
  val take_while_map : ('a -> 'b option) -> 'a list -> 'b list * 'a list
                                                         
end

(** Recommended to open this module to use the values without module paths *)
module Open : sig
  val from_Some : 'a option -> 'a

  val to_file : file: string -> string -> unit
  (** Create a file with the given string *)

  val ( ^/ ) : string -> string -> string
  (** [Filename.concat] *)

  val with_time : (unit -> 'a) -> 'a * float
  (** Time the function in seconds *)

  val failwithf : ('a, unit, string, 'b) format4 -> 'a
  (** failwith with printf interface *)
end
