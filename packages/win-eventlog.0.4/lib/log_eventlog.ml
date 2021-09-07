(*
 * Copyright (c) 2016 Docker Inc
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

let ppf, flush =
  let b = Buffer.create 255 in
  let flush () =
    let s = Buffer.contents b in
    Buffer.clear b;
    s
  in
  (Format.formatter_of_buffer b, flush)

let reporter ~eventlog ?(category = 0) ?(event = 0) () =
  let report _src level ~over k msgf =
    let ty =
      match level with
      | Logs.App -> `Success
      | Logs.Error -> `Error
      | Logs.Warning -> `Warning
      | Logs.Info -> `Information
      | Logs.Debug -> `Success
    in
    let k _ =
      Eventlog.report eventlog ty category event [| flush () |];
      over ();
      k ()
    in
    msgf @@ fun ?header ?tags:_ fmt ->
    match header with
    | None -> Format.kfprintf k ppf ("@[" ^^ fmt ^^ "@]@.")
    | Some h -> Format.kfprintf k ppf ("[%s] @[" ^^ fmt ^^ "@]@.") h
  in
  { Logs.report }
