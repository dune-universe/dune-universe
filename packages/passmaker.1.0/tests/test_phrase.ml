(********************************************************************************)
(*  Test_phrase.ml
    Copyright (c) 2018 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the ISC license.
    See LICENSE file for full license text.
*)
(********************************************************************************)

open Passmaker
open Word
open Phrase


(********************************************************************************)
(** {1 Common definitions}                                                      *)
(********************************************************************************)

let num_tests = 1000

let random_word (type u) (module W: Word.S with type t = u) =
    W.of_int (Random.int W.count)

let random_word_string (type u) (module W: Word.S with type t = u) =
    random_word (module W) |> W.to_string

let random_word_abbr (type u) (module W: Word.S with type t = u) =
    let str = random_word_string (module W) in
    let len = String.length str in
    if len > 4
    then String.sub str 0 4
    else str

let random_internal32 () =
    let adj = random_word (module Adjective) in
    let noun = random_word (module Noun) in
    let loc = random_word (module Location) in
    Internal.(R32 {adj; noun; loc})

let random_internal64 () =
    let subj_adj = random_word (module Adjective) in
    let subj_noun = random_word (module Noun) in
    let verb = random_word (module Verb) in
    let obj_adj = random_word (module Adjective) in
    let obj_noun = random_word (module Noun) in
    let obj_loc = random_word (module Location) in
    Internal.(R64 {subj_adj; subj_noun; verb; obj_adj; obj_noun; obj_loc})

let testable_internal = Alcotest.testable Internal.pp Internal.equal

let test_internal_conv of_internal to_internal () =
    let run prefix i internal =
        let desc = Printf.sprintf "%s:%04d" prefix i in
        Alcotest.(check testable_internal desc internal (internal |> of_internal |> to_internal)) in
    for i = 1 to num_tests do
        run "r32" i @@ random_internal32 ();
        run "r64" i @@ random_internal64 ()
    done


(********************************************************************************)
(** {1 Tests for Hexa module}                                                   *)
(********************************************************************************)

module Test_Hexa =
struct
    type of_string_error = [ `Invalid32 | `Invalid64 | `Invalid_length of int ] [@@deriving show, eq]

    type of_bytes_error = [ `Invalid_length of int ] [@@deriving show, eq]

    let random_bytes len =
        let buf = Bytes.create len in
        let chan = open_in_bin "/dev/urandom" in
        really_input chan buf 0 len;
        close_in chan; 
        buf

    let testable_bytes =
        let pp fmt x =
            let x' = x |> Bytes.to_string |> Hex.of_string |> Hex.show in
            Format.pp_print_string fmt x' in
        let equal = Bytes.equal in
        Alcotest.testable pp equal

    let testable_of_string_error = Alcotest.testable pp_of_string_error equal_of_string_error

    let testable_of_bytes_error = Alcotest.testable pp_of_bytes_error equal_of_bytes_error

    let test_string_conv () =
        let run prefix i x =
            let str = x |> Bytes.to_string |> Hex.of_string |> Hex.show in
            let desc = Printf.sprintf "%s:%04d" prefix i in
            Alcotest.(check (result string testable_of_string_error) desc (Ok str) Rresult.(str |> Hexa.of_string >>| Hexa.to_string)) in
        for i = 1 to num_tests do
            run "r32" i @@ random_bytes 4;
            run "r64" i @@ random_bytes 8
        done

    let test_bytes_conv () =
        let run prefix i x =
            let desc = Printf.sprintf "%s:%04d" prefix i in
            Alcotest.(check (result testable_bytes testable_of_bytes_error) desc (Ok x) Rresult.(x |> Hexa.of_bytes >>| Hexa.to_bytes)) in
        for i = 1 to num_tests do
            run "r32" i @@ random_bytes 4;
            run "r64" i @@ random_bytes 8
        done

    let testset =
        [
        ("internal_conv", `Quick, test_internal_conv Hexa.of_internal Hexa.to_internal);
        ("string_conv", `Quick, test_string_conv);
        ("bytes_conv", `Quick, test_bytes_conv);
        ]
end


(********************************************************************************)
(** {1 Tests for Text module}                                                   *)
(********************************************************************************)

module Test_Text =
struct
    type unknown_word = Text.unknown_word =
        {
        name: string;
        word: string;
        same_prefix: string option;
        suggestions: string list array;
        } [@@deriving show, eq]

    type unknown_abbr = Text.unknown_abbr =
        {
        name: string;
        word: string;
        } [@@deriving show, eq]

    type of_string_error = [ `Unknown_words of unknown_word list | `Parsing_error of string ] [@@deriving show, eq]

    type of_abbr_error = [ `Unknown_abbrs of unknown_abbr list | `Parsing_error of string ] [@@deriving show, eq]

    let random_text32 () =
        let article = if Random.bool () then "the " else "" in
        let ablative = if Random.bool () then "from " else "" in
        let adj = random_word_string (module Adjective) in
        let noun = random_word_string (module Noun) in
        let loc = random_word_string (module Location) in
        let canonical = Printf.sprintf "%s %s from %s"
            (String.capitalize_ascii adj) noun (String.capitalize_ascii loc) in
        let elaborate = Printf.sprintf "%s%s %s %s%s"
            article adj noun ablative loc in
        (canonical, elaborate)
    
    let random_abbr32 () =
        let adj = random_word_abbr (module Adjective) in
        let noun = random_word_abbr (module Noun) in
        let loc = random_word_abbr (module Location) in
        Printf.sprintf "%s %s %s" adj noun loc
    
    let random_text64 () =
        let article1 = if Random.bool () then "the " else "" in
        let article2 = if Random.bool () then "the " else "" in
        let ablative = if Random.bool () then "from " else "" in
        let subj_adj = random_word_string (module Adjective) in
        let subj_noun = random_word_string (module Noun) in
        let verb = random_word_string (module Verb) in
        let obj_adj = random_word_string (module Adjective) in
        let obj_noun = random_word_string (module Noun) in
        let obj_loc = random_word_string (module Location) in
        let canonical = Printf.sprintf "%s %s %s %s %s from %s"
            (String.capitalize_ascii subj_adj) subj_noun verb obj_adj obj_noun (String.capitalize_ascii obj_loc) in
        let elaborate = Printf.sprintf "%s%s %s %s %s%s %s %s%s"
            article1 subj_adj subj_noun verb article2 obj_adj obj_noun ablative obj_loc in
        (canonical, elaborate)

    let random_abbr64 () =
        let subj_adj = random_word_abbr (module Adjective) in
        let subj_noun = random_word_abbr (module Noun) in
        let verb = random_word_abbr (module Verb) in
        let obj_adj = random_word_abbr (module Adjective) in
        let obj_noun = random_word_abbr (module Noun) in
        let obj_loc = random_word_abbr (module Location) in
        Printf.sprintf "%s %s %s %s %s %s" subj_adj subj_noun verb obj_adj obj_noun obj_loc

    let testable_of_string_error = Alcotest.testable pp_of_string_error equal_of_string_error

    let testable_of_abbr_error = Alcotest.testable pp_of_abbr_error equal_of_abbr_error

    let test_string_conv () =
        let run prefix i (canonical, elaborate) =
            let desc = Printf.sprintf "%s:%04d" prefix i in
            Alcotest.(check (result string testable_of_string_error) desc (Ok canonical) Rresult.(elaborate |> Text.of_string >>| Text.to_string)) in
        for i = 1 to num_tests do
            run "r32" i @@ random_text32 ();
            run "r64" i @@ random_text64 ()
        done

    let test_abbr_conv () =
        let run prefix i abbr =
            let desc = Printf.sprintf "%s:%04d" prefix i in
            Alcotest.(check (result string testable_of_abbr_error) desc (Ok abbr) Rresult.(abbr |> Text.of_abbr_string >>| Text.to_abbr_string)) in
        for i = 1 to num_tests do
            run "r32" i @@ random_abbr32 ();
            run "r64" i @@ random_abbr64 ()
        done

    let testset =
        [
        ("internal_conv", `Quick, test_internal_conv Text.of_internal Text.to_internal);
        ("string_conv", `Quick, test_string_conv);
        ("abbr_conv", `Quick, test_abbr_conv);
        ]
end


(********************************************************************************)
(** {1 Tests for both modules}                                                  *)
(********************************************************************************)

module Test_Both =
struct
    let test_well_known () =
        let run (hexa, text) =
            let text' = Rresult.(hexa |> Hexa.of_string |> R.get_ok |> Hexa.to_internal |> Text.of_internal |> Text.to_string) in
            Alcotest.(check string hexa text text') in
        List.iter run
            [
            ("00000000", "Abandoned aardvark from Aachen");
            ("ffffffff", "Zesty zucchini from Zurich");
            ("0000000000000000", "Abandoned aardvark abandons abandoned aardvark from Aachen");
            ("ffffffffffffffff", "Zesty zucchini wrestles zesty zucchini from Zurich");
            ]

    let testset =
        [
        ("well-known", `Quick, test_well_known);
        ]
end


(********************************************************************************)
(** {1 Main}                                                                    *)
(********************************************************************************)

let () = Alcotest.run "Phrase module"
    [
    ("Hexa", Test_Hexa.testset);
    ("Text", Test_Text.testset);
    ("Both", Test_Both.testset);
    ]
