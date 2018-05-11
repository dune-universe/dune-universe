(********************************************************************************)
(*  Test_word.ml
    Copyright (c) 2018 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the ISC license.
    See LICENSE file for full license text.
*)
(********************************************************************************)

open Passmaker
open Prelude
open Word


(********************************************************************************)
(** {1 Functors}                                                                *)
(********************************************************************************)

module type SOURCE =
sig
    include Word.S

    val mappings: (int * string) list
    val suggestions: (string * int * (string option * string list array)) list
end

module Make_test (Source: SOURCE) =
struct
    let m = Alcotest.testable Source.pp Source.equal

    let abbr str =
        if String.length str > 4
        then String.sub str 0 4
        else str

    let test_word_list () =
        let rec is_valid prev_word prev_abbr i =
            if i < Source.count
            then
                let this_word = Source.(i |> of_int |> to_string) in
                let this_abbr = abbr this_word in
                match (prev_word, this_word) with
                    | (Some prev, this) when String.compare prev this >= 0 ->
                        Some (Printf.sprintf "%s '%s' is not lexicographically smaller than '%s'" (String.capitalize_ascii Source.name) prev this)
                    | _ -> match (prev_abbr, this_abbr) with
                        | (Some prev, this) when String.compare prev this >= 0 ->
                            Some (Printf.sprintf "Abbreviation '%s' is not lexicographically smaller than '%s'" prev this)
                        | _ ->
                            if String.length this_word <= 10
                            then is_valid (Some this_word) (Some this_abbr) (i + 1)
                            else Some (Printf.sprintf "%s '%s' has more than 10 characters" (String.capitalize_ascii Source.name) this_word)
            else
                None in
        Alcotest.(check (option string) "word_list" None @@ is_valid None None 0)

    let test_levenshtein () =
        let rec outer i =
            if i < Source.count
            then
                let word1 = Source.(i |> of_int |> to_string) in
                let rec inner j =
                    if j < Source.count
                    then
                        let word2 = Source.(j |> of_int |> to_string) in
                        let dist = String.edit_distance word1 word2 in
                        if dist <= 1
                        then Some (Printf.sprintf "Edit distance between %ss '%s' and '%s' is %d" Source.name word1 word2 dist)
                        else inner (j + 1)
                    else
                        None in
                match inner (i + 1) with
                    | Some x -> Some x
                    | None   -> outer (i + 1)
            else
                None in
        Alcotest.(check (option string) "levenshtein" None @@ outer 0)

    let test_of_string () =
        let run (idx, str) = Alcotest.(check (option m) str (Some Source.(of_int idx)) Source.(of_string str)) in
        List.iter run Source.mappings

    let test_to_string () =
        let run (idx, str) = Alcotest.(check string str str Source.(idx |> of_int |> to_string)) in
        List.iter run Source.mappings

    let test_of_abbr_string () =
        let run (idx, str) =
            let str = abbr str in
            Alcotest.(check (option m) str (Some Source.(of_int idx)) Source.(of_abbr_string str)) in
        List.iter run Source.mappings

    let test_to_abbr_string () =
        let run (idx, str) =
            let str = abbr str in
            Alcotest.(check string str str Source.(idx |> of_int |> to_abbr_string)) in
        List.iter run Source.mappings

    let test_suggest () =
        let run (str, max_distance, expected) = Alcotest.(check (pair (option string) (array (list string))) str expected Source.(suggest ~max_distance str)) in
        List.iter run Source.suggestions

    let testset =
        [
        ("word_list", `Quick, test_word_list);
        ("levenshtein", `Quick, test_levenshtein);
        ("of_string", `Quick, test_of_string);
        ("to_string", `Quick, test_to_string);
        ("of_abbr_string", `Quick, test_of_abbr_string);
        ("to_abbr_string", `Quick, test_to_abbr_string);
        ("suggest", `Quick, test_suggest);
        ]
end


(********************************************************************************)
(** {1 Tests for modules satisfying signature SOURCE}                           *)
(********************************************************************************)

module Test_Adjective = Make_test
(struct
    include Adjective

    let mappings =
        [
        (0, "abandoned");
        (2, "able");
        ]

    let suggestions =
        [
        ("affordable", 2, (Some "affordable", [| ["affordable"]; []; [] |]));
        ("afordable", 2, (None, [| []; ["affordable"]; ["adorable"] |]));
        ("allured", 2, (Some "alluring", [| []; []; ["altered"; "allied"; "alleged"] |]));
        ]
end)

module Test_Location = Make_test
(struct
    include Location

    let mappings =
        [
        (0, "aachen");
        (4, "abuja");
        ]
    
    let suggestions =
        [
        ("azerbeijan", 0, (Some "azerbaijan", [| []; |]));
        ("azerbeijan", 1, (Some "azerbaijan", [| []; ["azerbaijan"] |]));
        ("azerbeijan", 2, (Some "azerbaijan", [| []; ["azerbaijan"]; [] |]));
        ("glouster", 2, (Some "gloucester", [| []; []; ["gloucester"] |]));
        ("goah", 2, (None, [| []; ["goa"]; ["utah"; "guam"; "graz"; "doha"] |]));
        ]
end)

module Test_Noun = Make_test
(struct
    include Noun

    let mappings =
        [
        (0, "aardvark");
        (4, "abdomen");
        ]
    
    let suggestions =
        [
        ("ardvark", 2, (None, [| []; ["aardvark"]; [] |]));
        ]
end)

module Test_Verb = Make_test
(struct
    include Verb

    let mappings =
        [
        (0, "abandons");
        (4, "absolves");
        ]
    
    let suggestions =
        [
        ("customises", 2, (Some "customizes", [| []; ["customizes"]; [] |]));
        ]
end)


(********************************************************************************)
(** {1 Main}                                                                    *)
(********************************************************************************)

let () = Alcotest.run "Word module"
    [
    ("Adjective", Test_Adjective.testset);
    ("Location", Test_Location.testset);
    ("Noun", Test_Noun.testset);
    ("Verb", Test_Verb.testset);
    ]
