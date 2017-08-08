(******************************************************************************)
(* Copyright (c) 2016 Skylable Ltd. <info-copyright@skylable.com>             *)
(*                                                                            *)
(* Permission to use, copy, modify, and/or distribute this software for any   *)
(* purpose with or without fee is hereby granted, provided that the above     *)
(* copyright notice and this permission notice appear in all copies.          *)
(*                                                                            *)
(* THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES   *)
(* WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF           *)
(* MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR    *)
(* ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES     *)
(* WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN      *)
(* ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF    *)
(* OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.             *)
(******************************************************************************)

(* This code is in the public domain *)
open Lwt
let show state = Printf.printf "%.3f\n" state; return state
let add a state = return (state +. a)

let commands = Cmdtui.(commands ~help:Cmdtui_lambda_term.display_help [
    "show", ("prints the state (floating point)", (const show));
    "add", ("adds a number to the current state", (const add $ float));
    "bindings", ("prints all bindings", const Cmdtui_lambda_term.show_bindings)
  ])
    
let () =
  Lwt_main.run (
    [
      "show";
      "add 1.";
      "add 2.";
      "bindings"
    ] |> Lwt_stream.of_list |>
    Cmdtui_lambda_term.run_lines ~prompt:"test" ~commands 0. >>= fun result ->
    assert (result -. 3. < 1e-9);
    Lwt.return_unit)
