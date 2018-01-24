(*
 * Copyright (c) 2018 Xavier R. Guérin <copyright@applepine.org>
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

open Grammar

let level = ref 0
let enabled = ref false

let get () =
  !level

let set v =
  level := v

let reset () =
  set 0

let toggle () =
  enabled := not !enabled;
  if !enabled then Ok T else Ok Nil

let enter t =
  if !enabled then begin
    for i = 0 to !level - 1 do Printf.printf "| "; done;
    print "⦿ " t;
  end;
  level := !level + 1;
  Ok t

let bind str t =
  if !enabled then begin
    for i = 0 to !level - 1 do Printf.printf "| "; done;
    print ("  " ^ str ^ " ← ") t;
  end

let leave t =
  level := !level - 1;
  match t with
  | Ok a ->
    if !enabled then begin
      for i = 0 to !level - 1 do Printf.printf "| " done;
      print "⮨ " a;
    end;
    t
  | Error _ -> t
