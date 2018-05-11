(********************************************************************************)
(*  Word.ml
    Copyright (c) 2018 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the ISC license.
    See LICENSE file for full license text.
*)
(********************************************************************************)

open Prelude


(********************************************************************************)
(** {1 Module types}                                                            *)
(********************************************************************************)

module type SOURCE =
sig
    val name: string
    val count: int
    val data: string
    val transform: string -> string
end

module type S =
sig
    type t

    val name: string
    val count: int

    val of_int: int -> t
    val to_int: t -> int

    val of_string: string -> t option
    val to_string: t -> string

    val of_abbr_string: string -> t option
    val to_abbr_string: t -> string

    val suggest: max_distance:int -> string -> string option * string list array

    include TESTABLE with type t := t
end


(********************************************************************************)
(** {1 Functors}                                                                *)
(********************************************************************************)

module Make (Source: SOURCE): S =
struct
    type t = int

    let name = Source.name
    let count = Source.count

    let prefix_len = 4
    
    let wordvec =
        let len = String.length Source.data in
        let vec = Array.make Source.count "" in
        let rec loop idx start =
            if start > len
            then
                assert (idx = Source.count)
            else
                let finish = match String.index_from_opt Source.data start '\n' with
                    | Some x -> x
                    | None   -> len in
                let word = String.sub Source.data start (finish - start) in
                let idx' =
                    if String.length word > 0 && word.[0] <> '#'
                    then begin
                        (if idx < Source.count then vec.(idx) <- Source.transform word);
                        idx + 1
                    end
                    else
                        idx in
                loop idx' (finish + 1) in
        loop 0 0;
        Array.sort String.compare vec;
        vec

    let prefix_compare a b =
        let len_a = String.length a in
        let len_b = String.length b in
        let rec loop i =
            if i >= prefix_len
            then 0
            else match (i < len_a, i < len_b) with
                | (true, true) ->
                    let cmp = Char.compare a.[i] b.[i] in
                    if cmp = 0 then loop (i + 1) else cmp
                | (true, false) ->
                    1
                | (false, true) ->
                    -1
                | (false, false) ->
                    0 in
        loop 0

    let of_int x = x

    let to_int x = x

    let of_string str = Array.binary_search String.compare str wordvec

    let to_string x = wordvec.(x)

    let of_abbr_string str =
        if String.length str > prefix_len
        then None
        else Array.binary_search prefix_compare str wordvec

    let to_abbr_string x =
        let str = wordvec.(x) in
        if String.length str > prefix_len
        then String.sub str 0 prefix_len
        else str

    let suggest ~max_distance str =
        let len = String.length str in
        let str_abbr = if len > prefix_len then String.sub str 0 prefix_len else str in
        let same_prefix = match of_abbr_string str_abbr with
            | Some x -> Some (to_string x)
            | None   -> None in
        let suggestions = Array.make (max_distance + 1) [] in
        let f word =
            let distance = String.edit_distance str word in
            if distance <= max_distance
            then suggestions.(distance) <- word :: suggestions.(distance) in
        Array.iter f wordvec;
        (same_prefix, suggestions)

    let pp = Format.pp_print_int

    let equal = (=)
end


(********************************************************************************)
(** {1 Modules encapsulating word lists}                                        *)
(********************************************************************************)

module Adjective = Make
(struct
    let name = "adjective"
    let count = 2048
    let data = [%blob "../resources/adjectives.txt"]
    let transform x = x
end)

module Location = Make
(struct
    let name = "location"
    let count = 1024
    let data = [%blob "../resources/locations.txt"]
    let transform x = x
end)

module Noun = Make
(struct
    let name = "noun"
    let count = 2048
    let data = [%blob "../resources/nouns.txt"]
    let transform x = x
end)

module Verb = Make
(struct
    let name = "verb"
    let count = 1024
    let data = [%blob "../resources/verbs.txt"]

    let transform x =
        let is_vowel = function 'a' | 'e' | 'i' | 'o' | 'u' -> true | _ -> false in
        let len = String.length x in
        let penult = x.[len - 2] in
        let ult = x.[len - 1] in
        match (penult, ult) with
            | ('s', 's') | (_, 'x') | (_, 'z') | ('s', 'h') | ('c', 'h') -> x ^ "es"
            | (v, 's') when is_vowel v                                   -> x ^ "ses"
            | (c, 'o') when not (is_vowel c)                             -> x ^ "es"
            | (c, 'y') when not (is_vowel c)                             -> String.sub x 0 (len - 1) ^ "ies"
            | _                                                          -> x ^ "s"
end)
