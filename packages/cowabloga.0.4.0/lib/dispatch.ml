(*
 * Copyright (c) 2014 Richard Mortier <mort@cantab.net>
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

type 'a t = {
  log: msg:string -> unit;
  ok: ?headers:(string*string) list -> string Lwt.t -> 'a Lwt.t;
  notfound: uri:Uri.t -> 'a Lwt.t;
  redirect: uri:Uri.t -> 'a Lwt.t;
}

module Log = struct
  let ok log path = log ~msg:(Printf.sprintf "200 GET %s" path)
  let notfound log path = log ~msg:(Printf.sprintf "404 NOTFOUND %s" path)
  let redirect log path = log ~msg:(Printf.sprintf "301 REDIRECT %s" path)
end

let split_path path =
  let rec aux = function
    | [] | [""] -> []
    | hd::tl -> hd :: aux tl
  in
  List.filter (fun e -> e <> "") (aux (Re.Str.(split_delim (regexp_string "/") path)))

let headers path = ["content-type", Magic_mime.lookup path]

let f io dispatchf uri =
  let path = Uri.path uri in
  let segments = split_path path in
  dispatchf segments >>= function
  | `Html page -> Log.ok io.log path; io.ok ~headers:Headers.html page
  | `Atom feed -> Log.ok io.log path; io.ok ~headers:Headers.atom feed
  | `Page (hs, body) -> Log.ok io.log path; io.ok ~headers:hs body
  | `Asset asset
    -> Log.ok io.log path; io.ok ~headers:(headers path) asset
  | `Redirect path
    -> Log.redirect io.log path; io.redirect ~uri:(Uri.of_string path)
  | `Not_found path
    -> Log.notfound io.log path; io.notfound ~uri:(Uri.of_string path)
