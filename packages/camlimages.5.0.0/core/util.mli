(***********************************************************************)
(*                                                                     *)
(*                             CamlImages                              *)
(*                                                                     *)
(*            Fran輟is Pessaux, projet Cristal, INRIA Rocquencourt     *)
(*            Pierre Weis, projet Cristal, INRIA Rocquencourt          *)
(*            Jun Furuse, projet Cristal, INRIA Rocquencourt           *)
(*                                                                     *)
(*  Copyright 1999-2014,                                               *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(** Bytes and Strings 

    In 4.02.0, [s.[i] <- c] becomes deprecated and this is 
    very frustrating for CamlImages.

    We introduce [(<<)] and replace them by [s << i & c].
*)

external ( & ) : ('a -> 'b) -> 'a -> 'b = "%apply"

val (<<) : bytes -> int -> char -> unit
(** equivalent with [Bytes.set] *)

val (<<!) : bytes -> int -> char -> unit
(** equivalent with [Bytes.unsafe_set] *)

val range_check : bytes -> int -> int -> unit
(** [range_check bs from to_] raises [Invalid_argument "index out of bounds"]
    when [from] and [to_] are invalid range for [bs].
*)

val (>@!) : 'a array -> int -> 'a
(** equivalent with [Array.unsafe.get] *)

val (@%) : bytes -> int -> int
(** s @% p = Char.code @@ Bytes.get s p *)
