(********************************************************************************)
(*  Prelude.mli
    Copyright (c) 2018 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the ISC license.
    See LICENSE file for full license text.
*)
(********************************************************************************)


(********************************************************************************)
(** {1 Signatures}                                                              *)
(********************************************************************************)

module type TESTABLE =
sig
    type t

    val pp: Format.formatter -> t -> unit
    val equal: t -> t -> bool
end


(********************************************************************************)
(** {1 Modules}                                                                 *)
(********************************************************************************)

module Array:
sig
    include module type of Array

    val binary_search: ('a -> 'a -> int) -> 'a -> 'a array -> int option
    (** [binary_search cmp x arr] performs a binary search for element [x]
     *  in array [arr] using [cmp] as the comparison function. It assumes
     *  the array is sorted!
     *)
end

module String:
sig
    include module type of String

    val edit_distance: string -> string -> int
    (** Compute the Damerau-Levenshtein distance between two strings (more precisely
     *  the Optimal String Alignment Distance) using the algorithm described in the
     *  Wikipedia page: https://en.wikipedia.org/wiki/Damerau-Levenshtein_distance
     *  Note that we assume an ASCII encoding of the strings (won't work for UTF8).
     *)
end
