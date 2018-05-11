(********************************************************************************)
(*  Phrase.ml
    Copyright (c) 2018 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the ISC license.
    See LICENSE file for full license text.
*)
(********************************************************************************)

open Prelude
open Word


(********************************************************************************)
(** {1 Signatures}                                                              *)
(********************************************************************************)

module type INTEGER =
sig
    type t

    val zero: t
    val one: t
    val of_int: int -> t
    val to_int: t -> int
    val add: t -> t -> t
    val sub: t -> t -> t
    val logand: t -> t -> t
    val logor: t -> t -> t
    val shift_left: t -> int -> t
    val shift_right_logical: t -> int -> t
end


(********************************************************************************)
(** {1 Definitions about the internal representation of phrases}                *)
(********************************************************************************)

module Internal =
struct
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

    let pp fmt x = ()
    
    let equal = (=)
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

module Hexa =
struct
    open Internal
    
    type t = Int32 of int32 | Int64 of int64

    type of_string_error = [ `Invalid32 | `Invalid64 | `Invalid_length of int ]

    type of_bytes_error = [ `Invalid_length of int ]

    let of_internal internal =
        let aux (type u) (module I: INTEGER with type t = u) to_hexa xs =
            let rec loop acc = function
                | [] ->
                    to_hexa acc
                | (i, shift) :: tl ->
                    let x = I.shift_left (I.of_int i) shift in
                    let acc = I.add acc x in
                    loop acc tl in
            loop I.zero xs in
        match internal with
            | R32 x -> aux (module Int32) (fun x -> Int32 x)
                [
                (Adjective.to_int x.adj, 0);
                (Noun.to_int x.noun, 11);
                (Location.to_int x.loc, 22);
                ]
            | R64 x -> aux (module Int64) (fun x -> Int64 x)
                [
                (Adjective.to_int x.subj_adj, 0);
                (Noun.to_int x.subj_noun, 11);
                (Verb.to_int x.verb, 22);
                (Adjective.to_int x.obj_adj, 32);
                (Noun.to_int x.obj_noun, 43);
                (Location.to_int x.obj_loc, 54);
                ]

    let to_internal hexa =
        let get (type u) (module I: INTEGER with type t = u) x first len =
            let mask = I.(shift_left (sub (shift_left one len) one) first) in
            let v = I.(shift_right_logical (logand mask x) first) in
            I.to_int v in
        match hexa with
            | Int32 x ->
                let get = get (module Int32) in
                let adj = get x 0 11 |> Adjective.of_int in
                let noun = get x 11 11 |> Noun.of_int in
                let loc = get x 22 10 |> Location.of_int in
                R32 {adj; noun; loc}
            | Int64 x ->
                let get = get (module Int64) in
                let subj_adj = get x 0 11 |> Adjective.of_int in
                let subj_noun = get x 11 11 |> Noun.of_int in
                let verb = get x 22 10 |> Verb.of_int in
                let obj_adj = get x 32 11 |> Adjective.of_int in
                let obj_noun = get x 43 11 |> Noun.of_int in
                let obj_loc = get x 54 10 |> Location.of_int in
                R64 {subj_adj; subj_noun; verb; obj_adj; obj_noun; obj_loc}

    let of_string str = match String.length str with
        | 8  ->
            begin match Int32.of_string_opt ("0x" ^ str) with
                | Some x -> Ok (Int32 x)
                | None   -> Error `Invalid32
            end
        | 16 ->
            begin match Int64.of_string_opt ("0x" ^ str) with
                | Some x -> Ok (Int64 x)
                | None   -> Error `Invalid64
            end
        | x ->
            Error (`Invalid_length x)

    let to_string = function
        | Int32 x -> Printf.sprintf "%08lx" x
        | Int64 x -> Printf.sprintf "%016Lx" x

    let of_bytes buf =
        let aux (type u) (module I: INTEGER with type t = u) len =
            let rec loop acc i =
                if i < len
                then
                    let v = Bytes.get buf (len - i - 1) |> Char.code |> I.of_int in
                    let mask = I.shift_left v (8 * i) in
                    let acc = I.logor acc mask in
                    loop acc (i + 1)
                else
                    acc in
            loop I.zero 0 in
        match Bytes.length buf with
            | 4 -> Ok (Int32 (aux (module Int32) 4))
            | 8 -> Ok (Int64 (aux (module Int64) 8))
            | x -> Error (`Invalid_length x)

    let to_bytes x =
        let aux (type u) (module I: INTEGER with type t = u) len x =
            let bytes = Bytes.create len in
            let mask = I.of_int 255 in
            for i = 0 to len - 1 do
                let v = I.(shift_right_logical x (8 * i) |> logand mask |> to_int) in
                let b = Char.chr v in
                Bytes.set bytes (len - i - 1) b
            done;
            bytes in
        match x with
            | Int32 x -> aux (module Int32) 4 x
            | Int64 x -> aux (module Int64) 8 x
        
        
end

module Text =
struct
    open Internal
    
    type t =
        {
        text: string;
        internal: Internal.t;
        }

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

    let sentence32 a b c =
        Printf.sprintf "%s %s from %s" (String.capitalize_ascii a) b (String.capitalize_ascii c)

    let sentence64 a b c d e f =
        Printf.sprintf "%s %s %s %s %s from %s" (String.capitalize_ascii a) b c d e (String.capitalize_ascii f)

    let of_internal internal = match internal with
        | R32 x ->
            let a = Adjective.to_string x.adj in
            let b = Noun.to_string x.noun in
            let c = Location.to_string x.loc in
            let text = sentence32 a b c in
            {text; internal}
        | R64 x ->
            let a = Adjective.to_string x.subj_adj in
            let b = Noun.to_string x.subj_noun in
            let c = Verb.to_string x.verb in
            let d = Adjective.to_string x.obj_adj in
            let e = Noun.to_string x.obj_noun in
            let f = Location.to_string x.obj_loc in
            let text = sentence64 a b c d e f in
            {text; internal}

    let to_internal x = x.internal

    let split_words str =
        str |> String.lowercase_ascii |> String.split_on_char ' ' |> List.filter ((<>) "")

    let of_string str =
        let process (type u) (module W: Word.S with type t = u) errors word = match W.of_string word with
            | Some x ->
                (Some x, errors)
            | None ->
                let (same_prefix, suggestions) = W.suggest ~max_distance:2 word in
                let error = {name = W.name; word; same_prefix; suggestions} in
                (None, error :: errors) in
        let rec extract acc actions words = match (actions, words) with
            | ([], [])                             -> Some acc
            | ([], _)                              -> None
            | (`Word :: _, [])                     -> None
            | (`Word :: xtl, y :: ytl)             -> extract (y :: acc) xtl ytl
            | (`Opt _ :: xtl, [])                  -> extract acc xtl []
            | (`Opt x :: xtl, y :: ytl) when x = y -> extract acc xtl ytl
            | (`Opt x :: xtl, ys)                  -> extract acc xtl ys in
        let words = split_words str in
        if List.length words <= 5
        then match extract [] [`Opt "the"; `Word; `Word; `Opt "from"; `Word] words with
            | Some [c; b; a] ->
                let errors = [] in
                let (a', errors) = process (module Adjective) errors a in
                let (b', errors) = process (module Noun) errors b in
                let (c', errors) = process (module Location) errors c in
                begin match (a', b', c', errors) with
                    | (Some adj, Some noun, Some loc, _) ->
                        let internal = R32 {adj; noun; loc} in
                        let text = sentence32 a b c in
                        Ok {internal; text}
                    | (_, _, _, errors) ->
                        Error (`Unknown_words errors)
                end
            | _ ->
                Error (`Parsing_error str)
        else match extract [] [`Opt "the"; `Word; `Word; `Word; `Opt "the"; `Word; `Word; `Opt "from"; `Word] words with
            | Some [f; e; d; c; b; a] ->
                let errors = [] in
                let (a', errors) = process (module Adjective) errors a in
                let (b', errors) = process (module Noun) errors b in
                let (c', errors) = process (module Verb) errors c in
                let (d', errors) = process (module Adjective) errors d in
                let (e', errors) = process (module Noun) errors e in
                let (f', errors) = process (module Location) errors f in
                begin match (a', b', c', d', e', f', errors) with
                    | (Some subj_adj, Some subj_noun, Some verb, Some obj_adj, Some obj_noun, Some obj_loc, _) ->
                        let internal = R64 {subj_adj; subj_noun; verb; obj_adj; obj_noun; obj_loc} in
                        let text = sentence64 a b c d e f in
                        Ok {internal; text}
                    | (_, _, _, _, _, _, errors) ->
                        Error (`Unknown_words errors)
                end
            | _ ->
                Error (`Parsing_error str)

    let to_string x = x.text

    let of_abbr_string str =
        let process (type u) (module W: Word.S with type t = u) errors word = match W.of_abbr_string word with
            | Some x -> (Some x, errors)
            | None   -> (None, {name = W.name; word} :: errors) in
        match split_words str with
            | [a; b; c] ->
                let errors = [] in
                let (a', errors) = process (module Adjective) errors a in
                let (b', errors) = process (module Noun) errors b in
                let (c', errors) = process (module Location) errors c in
                begin match (a', b', c', errors) with
                    | (Some adj, Some noun, Some loc, _) ->
                        let internal = R32 {adj; noun; loc} in
                        let adj' = Adjective.to_string adj in
                        let noun' = Noun.to_string noun in
                        let loc' = Location.to_string loc in
                        let text = sentence32 adj' noun' loc' in
                        Ok {internal; text}
                    | (_, _, _, errors) ->
                        Error (`Unknown_abbrs errors)
                end
            | [a; b; c; d; e; f] ->
                let errors = [] in
                let (a', errors) = process (module Adjective) errors a in
                let (b', errors) = process (module Noun) errors b in
                let (c', errors) = process (module Verb) errors c in
                let (d', errors) = process (module Adjective) errors d in
                let (e', errors) = process (module Noun) errors e in
                let (f', errors) = process (module Location) errors f in
                begin match (a', b', c', d', e', f', errors) with
                    | (Some subj_adj, Some subj_noun, Some verb, Some obj_adj, Some obj_noun, Some obj_loc, _) ->
                        let internal = R64 {subj_adj; subj_noun; verb; obj_adj; obj_noun; obj_loc} in
                        let subj_adj' = Adjective.to_string subj_adj in
                        let subj_noun' = Noun.to_string subj_noun in
                        let verb' = Verb.to_string verb in
                        let obj_adj' = Adjective.to_string obj_adj in
                        let obj_noun' = Noun.to_string obj_noun in
                        let obj_loc' = Location.to_string obj_loc in
                        let text = sentence64 subj_adj' subj_noun' verb' obj_adj' obj_noun' obj_loc' in
                        Ok {internal; text}
                    | (_, _, _, _, _, _, errors) ->
                        Error (`Unknown_abbrs errors)
                end
            | _ ->
                Error (`Parsing_error str)

    let to_abbr_string x = match x.internal with
        | R32 x ->
            let a = Adjective.to_abbr_string x.adj in
            let b = Noun.to_abbr_string x.noun in
            let c = Location.to_abbr_string x.loc in
            Printf.sprintf "%s %s %s" a b c
        | R64 x ->
            let a = Adjective.to_abbr_string x.subj_adj in
            let b = Noun.to_abbr_string x.subj_noun in
            let c = Verb.to_abbr_string x.verb in
            let d = Adjective.to_abbr_string x.obj_adj in
            let e = Noun.to_abbr_string x.obj_noun in
            let f = Location.to_abbr_string x.obj_loc in
            Printf.sprintf "%s %s %s %s %s %s" a b c d e f
end
