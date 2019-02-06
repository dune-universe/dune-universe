#! /usr/bin/env ocamlscript
Ocaml.packs := [ "unix"; "stringext"; "lwt.unix" ]
--

open Lwt.Infix

let rec loop () =
  Lwt_io.read_line_opt Lwt_io.stdin
  >>= function
  | None -> Lwt.return ()
  | Some x ->
    begin match x |> String.trim |> Stringext.split ~on:' ' |> List.filter (fun x -> x <> "") with
    | [ "|"; name ] ->
      Printf.printf "| %s -> \"%s\"\n" name name
    | _ ->
      ()
    end;
    loop ()

let _ = Lwt_main.run (loop ())
