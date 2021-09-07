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

open Win_eventlog

let _ =
  let h = Eventlog.register "Docker.exe" in
  List.iter
    (fun ty ->
      List.iter
        (fun category ->
          List.iter
            (fun event ->
              Eventlog.report h ty category event
                [|
                  Printf.sprintf
                    "This should have type %s with category %d and event id %d"
                    (Eventlog.string_of_ty ty) category event;
                |])
            [ 3; 4; 5 ])
        [ 0; 1; 2 ])
    [ `Success; `Audit_failure; `Audit_success; `Error; `Information; `Warning ]
