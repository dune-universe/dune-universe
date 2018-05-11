(********************************************************************************)
(*  Gen.ml
    Copyright (c) 2018 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the ISC license.
    See LICENSE file for full license text.
*)
(********************************************************************************)

open Passmaker
open Phrase

let random_bytes () =
    let len = 8 in
    let buf = Bytes.create len in
    let chan = open_in_bin "/dev/urandom" in
    really_input chan buf 0 len;
    close_in chan;
    buf

let () = match Hexa.of_bytes @@ random_bytes () with
    | Ok hexa ->
        let text = hexa |> Hexa.to_internal |> Text.of_internal in
        Printf.printf "Text passphrase:            %s\n" (Text.to_string text);
        Printf.printf "Abbreviated passphrase:     %s\n" (Text.to_abbr_string text);
        Printf.printf "Hexadecimal representation: %s\n" (Hexa.to_string hexa);
        exit 0
    | Error _ ->
        Printf.eprintf "An internal error occurred.";
        exit 1
