(********************************************************************************)
(*  Phrase.mli
    Copyright (c) 2018 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the ISC license.
    See LICENSE file for full license text.
*)
(********************************************************************************)

open Prelude
open Word


(********************************************************************************)
(** {1 Definitions about the internal representation of phrases}                *)
(********************************************************************************)

module Internal:
sig
    type r32 =
        {
        adj: Adjective.t;
        noun: Noun.t;
        loc: Location.t;
        }

    type r64 =
        {
        subj_adj: Adjective.t;
        subj_noun: Noun.t;
        verb: Verb.t;
        obj_adj: Adjective.t;
        obj_noun: Noun.t;
        obj_loc: Location.t;
        }

    type t =
        | R32 of r32
        | R64 of r64

    include TESTABLE with type t := t
end


(********************************************************************************)
(** {1 Definitions about the external (user-visible) representation of phrases} *)
(********************************************************************************)

module type EXTERNAL =
sig
    type t
    type of_string_error

    val of_internal: Internal.t -> t
    val to_internal: t -> Internal.t
    val of_string: string -> (t, of_string_error) result
    val to_string: t -> string
end

module Hexa:
sig
    type of_string_error = [ `Invalid32 | `Invalid64 | `Invalid_length of int ]
    type of_bytes_error = [ `Invalid_length of int ]

    include EXTERNAL with type of_string_error := of_string_error

    val of_bytes: bytes -> (t, of_bytes_error) result
    val to_bytes: t -> bytes
end

module Text:
sig
    type unknown_word =
        {
        name: string;
        word: string;
        same_prefix: string option;
        suggestions: string list array;
        }

    type unknown_abbr =
        {
        name: string;
        word: string;
        }

    type of_string_error = [ `Unknown_words of unknown_word list | `Parsing_error of string ]
    type of_abbr_error = [ `Unknown_abbrs of unknown_abbr list | `Parsing_error of string ]

    include EXTERNAL with type of_string_error := of_string_error

    val of_abbr_string: string -> (t, of_abbr_error) result
    val to_abbr_string: t -> string
end
