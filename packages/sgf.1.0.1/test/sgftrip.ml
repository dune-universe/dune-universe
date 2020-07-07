(*
 * Copyright (c) 2012-2015 Vincent Bernardoff <vb@luminar.eu.org>
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

open Rresult

let () =
  let files = ref [] in
  let args = [] in
  let usage =
    Printf.sprintf "Usage: %s [filenames...]\nOptions are:" Sys.argv.(0) in

  let anon_f s = files := s :: !files in
  Arg.parse args anon_f usage;
  let collections =
    if !files = []
    then [Sgf.of_channel stdin]
    else
      List.map Sgf.of_file @@ List.rev !files
  in
  List.iter (function
      | Ok coll -> Sgf.pp_collection Format.std_formatter coll
      | Error _ -> ()
    )
    collections
