(*
 * Copyright (c) 2014-2015 Magnus Skjegstad <magnus@skjegstad.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)
open Lwt

module OS = Os_xen

type t = { cmd_line : string;
           parameters : (string * string) list }

exception Parameter_not_found of string

let get_cmd_line () =
  (* Originally based on mirage-skeleton/xen/static_website+ip code for reading
   * boot parameters, but we now read from xenstore for better ARM
   * compatibility.  *)
  OS.Xs.make () >>= fun client ->
  Lwt.catch (fun () ->
    OS.Xs.(immediate client (fun x -> read x "vm")) >>= fun vm ->
    OS.Xs.(immediate client (fun x -> read x (vm^"/image/cmdline"))))
    (fun _ ->
       let cmdline = (OS.Start_info.get ()).OS.Start_info.cmd_line in
       Lwt.return cmdline)

let create () =
  get_cmd_line () >>= fun cmd_line_raw ->
  (* Strip leading whitespace *)
  let filter_map fn l =
    List.fold_left (fun acc x ->
        match fn x with Some y -> y::acc | None -> acc) [] l
  in
  let entries = Parse_argv.parse cmd_line_raw in
  match entries with
  | Error s -> fail_with s
  | Ok l ->
    let parameters =
      filter_map (fun x ->
        match Astring.String.cut ~sep:"=" x with
        | Some (a,b) ->
          Some (a,b)
        | _ ->
          Printf.printf "Ignoring malformed parameter: %s\n" x; None
      ) l
    in
    Lwt.return { cmd_line=cmd_line_raw ; parameters}

let get_exn t parameter =
  try
    List.assoc parameter t.parameters
  with
    Not_found -> raise (Parameter_not_found parameter)

let get t parameter =
  try
    Some (List.assoc parameter t.parameters)
  with
    Not_found -> None

let parameters x = x.parameters

let to_argv ~filter x =
  let x = List.filter filter x in
  let argv = Array.make (1 + List.length x) "" in
  let f i (k,v) =
    let dash = if String.length k = 1 then "-" else "--" in
    argv.(i + 1) <- Printf.sprintf "%s%s=%s" dash k v
  in
  List.iteri f x ;
  argv

let argv ?(filter=(fun _ -> true)) () =
  create () >>= fun t -> return (to_argv ~filter @@ parameters t)
