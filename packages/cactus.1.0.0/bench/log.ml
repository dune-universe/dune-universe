(*
 * Copyright (c) 2021 Tarides <contact@tarides.com>
 * Copyright (c) 2021 Gabriel Belouze <gabriel.belouze@ens.psl.eu>
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
 *)

let nop_fmt = Format.make_formatter (fun _ _ _ -> ()) (fun () -> ())

let app_reporter =
  let counter = Mtime_clock.counter () in
  let report _src level ~over k msgf =
    let dt = Mtime_clock.count counter |> Mtime.Span.to_ms in
    let k _ =
      over ();
      k ()
    in
    let print header tags k fmt =
      let open Btree.Private.Tag in
      let kind = match tags with None -> None | Some tags -> Logs.Tag.find kind_tag tags in
      let ppf = match kind with Some Stats -> nop_fmt | _ -> Fmt.stdout in
      Fmt.kpf k ppf ("[%+04.0fms] %a @[" ^^ fmt ^^ "@]@.") dt Logs_fmt.pp_header (level, header)
    in
    msgf @@ fun ?header ?tags fmt -> print header tags k fmt
  in
  { Logs.report } |> Progress.instrument_logs_reporter

let combine reporter =
  let report src level ~over k msgf =
    let v = app_reporter.Logs.report src level ~over:(fun () -> ()) k msgf in
    reporter.Logs.report src level ~over (fun () -> v) msgf
  in
  { Logs.report }

let reporter ppf statsppf =
  let counter = Mtime_clock.counter () in
  let report _src level ~over k msgf =
    let dt = Mtime_clock.count counter |> Mtime.Span.to_ms in
    let k _ =
      over ();
      k ()
    in
    let print header tags k fmt =
      let open Btree.Private.Tag in
      let kind = match tags with None -> None | Some tags -> Logs.Tag.find kind_tag tags in
      match kind with
      | Some Stats -> Fmt.kpf k statsppf ("%f;" ^^ fmt ^^ "@.") dt
      | _ ->
          Fmt.kpf k ppf ("[%+04.0fms] %a @[" ^^ fmt ^^ "@]@.") dt Logs_fmt.pp_header (level, header)
    in
    msgf @@ fun ?header ?tags fmt -> print header tags k fmt
  in
  { Logs.report } |> combine

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  ()
