(********************************************************************************)
(*  Passmakercmd.ml
    Copyright (c) 2018 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the ISC license.
    See LICENSE file for full license text.
*)
(********************************************************************************)

open Cmdliner
open Passmaker
open Phrase


(********************************************************************************)
(** {1 Auxiliary modules}                                                       *)
(********************************************************************************)

module Size:
sig
    type t = S32 | S64

    val of_string: string -> (t, [ `Msg of string ]) result
    val pp: Format.formatter -> t -> unit
    val conv: t Arg.conv
end =
struct
    type t = S32 | S64

    let of_string = function
        | "32" -> Ok S32
        | "64" -> Ok S64
        | _    -> Error (`Msg "Not a valid size")

    let pp fmt size = Format.pp_print_string fmt (match size with S32 -> "32" | S64 -> "64")

    let conv = Arg.conv (of_string, pp)
end


(********************************************************************************)
(*  {1 Miscelaneous utility functions}                                          *)
(********************************************************************************)

let string_or_stdin = function
    | Some str -> str
    | None     -> read_line ()

let sprint_unknown_word ({name; word; same_prefix; suggestions} : Text.unknown_word) =
    let msg = Printf.sprintf "Unknown %s '%s'.\n" name word in
    let msg = match same_prefix with
        | None     -> msg
        | Some str -> msg ^ Printf.sprintf "  There is one %s with the same 4-letter prefix: %s.\n" name str in
    let msg =
        if suggestions.(1) = []
        then msg
        else msg ^ Printf.sprintf "  List of very similar %ss: %s.\n" name (String.concat ", " suggestions.(1)) in
    let msg =
        if suggestions.(2) = []
        then msg
        else msg ^ Printf.sprintf "  List of roughly similar %ss: %s.\n" name (String.concat ", " suggestions.(2)) in
    msg

let sprint_unknown_abbr ({name; word} : Text.unknown_abbr) =
    Printf.sprintf "Unknown abbreviated %s '%s'.\n" name word

let sprint_error = function
    | `Invalid32 | `Invalid64 ->
        "Input contains invalid hexadecimal characters."
    | `Invalid_length x ->
        "Input contains an invalid number of characters."
    | `Unknown_words xs ->
        "Unknown words were found:\n\n" ^
        String.concat "\n" (List.map sprint_unknown_word xs)
    | `Unknown_abbrs xs ->
        "Unknown abbreviations were found:\n\n" ^
        String.concat "\n" (List.map sprint_unknown_abbr xs)
    | `Parsing_error _ ->
        "Unable to parse input."

let transform_result = function
    | Ok msg    -> Ok msg
    | Error err -> Error (sprint_error err)

let random_bytes size =
    let len = match size with Size.S32 -> 4 | Size.S64 -> 8 in
    let buf = Bytes.create len in
    let chan = open_in_bin "/dev/urandom" in
    really_input chan buf 0 len;
    close_in chan;
    buf

let common_doc = "If the command line parameter is not present, the value is read from stdin instead."


(********************************************************************************)
(*  {1 Commands}                                                                *)
(********************************************************************************)

let text_of_hex_cmd =
    let text_of_hex hexa =
        let open Rresult in
        string_or_stdin hexa |>
        Hexa.of_string >>|
        Hexa.to_internal >>|
        Text.of_internal >>|
        Text.to_string |>
        transform_result in
    let term =
        let hexa =
            let doc = "Hexadecimal representation of passphrase (either 8 or 16 characters long). " ^ common_doc in
            Arg.(value @@ pos 0 (some string) None @@ info [] ~docv:"HEX" ~doc) in
        Term.(const text_of_hex $ hexa) in
    let info =
        let doc = "Convert hexadecimal representation to text passphrase." in
        Term.info ~doc "text-of-hex" in
    (term, info)

let hex_of_text_cmd =
    let hex_of_text text =
        let open Rresult in
        string_or_stdin text |>
        Text.of_string >>|
        Text.to_internal >>|
        Hexa.of_internal >>|
        Hexa.to_string |>
        transform_result in
    let term =
        let text =
            let doc = "Text passphrase (either 32-bit or 64-bit variant). " ^ common_doc in
            Arg.(value @@ pos 0 (some string) None @@ info [] ~docv:"PHRASE" ~doc) in
        Term.(const hex_of_text $ text) in
    let info =
        let doc = "Convert text phrase to hexadecimal representation." in
        Term.info ~doc "hex-of-text" in
    (term, info)

let generate_cmd =
    let generate size =
        let open Rresult in
        random_bytes size |>
        Hexa.of_bytes >>|
        Hexa.to_internal >>|
        Text.of_internal >>|
        Text.to_string |>
        transform_result in
    let term =
        let size =
            let doc = "Entropy length in bits of the generated passphrase. Either 32 or 64." in
            Arg.(value @@ opt Size.conv Size.S64 @@ info ["s"; "size"] ~docv:"SIZE" ~doc) in
        Term.(const generate $ size) in
    let info =
        let doc = "Generate a fresh text passphrase." in
        Term.info ~doc "generate" in
    (term, info)

let abbreviate_cmd =
    let abbreviate text =
        let open Rresult in
        string_or_stdin text |>
        Text.of_string >>|
        Text.to_abbr_string |>
        transform_result in
    let term =
        let text =
            let doc = "Text passphrase (either 32-bit or 64-bit variant). " ^ common_doc in
            Arg.(value @@ pos 0 (some string) None @@ info [] ~docv:"TEXT" ~doc) in
        Term.(const abbreviate $ text) in
    let info =
        let doc = "Abbreviate text passphrase." in
        Term.info ~doc "abbreviate" in
    (term, info)

let expand_cmd =
    let expand text =
        let open Rresult in
        string_or_stdin text |>
        Text.of_abbr_string >>|
        Text.to_string |>
        transform_result in
    let term =
        let text =
            let doc = "Abbreviated passphrase (either 32-bit or 64-bit variant). " ^ common_doc in
            Arg.(value @@ pos 0 (some string) None @@ info [] ~docv:"ABBR" ~doc) in
        Term.(const expand $ text) in
    let info =
        let doc = "Expand abbreviated phrase into text passphrase." in
        Term.info ~doc "expand" in
    (term, info)

let default_cmd =
    let term = Term.(ret (pure (`Help (`Pager, None)))) in
    let info =
        let doc = "Utility for generating memorable passphrases." in
        Term.info ~version:"1.0" ~doc "passmakercmd" in
    (term, info)

let cmds =
    [
    text_of_hex_cmd;
    hex_of_text_cmd;
    generate_cmd;
    abbreviate_cmd;
    expand_cmd;
    ]

let () = match Term.eval_choice default_cmd cmds with
    | `Version | `Help -> exit 0
    | `Ok Ok msg       -> print_endline msg; exit 0
    | `Ok Error msg    -> prerr_endline msg; exit 1
    | `Error `Parse    -> exit 2
    | `Error `Term     -> exit 3
    | `Error `Exn      -> exit 4
