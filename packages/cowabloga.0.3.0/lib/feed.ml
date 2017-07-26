(*
 * Copyright (c) 2013 Anil Madhavapeddy <anil@recoil.org>
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

(** Generate an aggregated link feed from all the other feeds *)

open Lwt.Infix

type feed = [
  | `Blog of Atom_feed.t * Blog.Entry.t list
  | `Wiki of Atom_feed.t * Wiki.entry list
  | `Links of Atom_feed.t * Links.t list
]

let feed_icon =
  function
  | `Blog _ -> "fa-comment"
  | `Wiki _ -> "fa-book"
  | `Links _ -> "fa-external-link"

let feed_uri x =
  let str = match x with
    | `Blog f  -> f.Atom_feed.base_uri ^ "blog/" (* TODO: Proper URL routing *)
    | `Wiki f  -> f.Atom_feed.base_uri ^ "wiki/"
    | `Links f -> f.Atom_feed.base_uri ^ "links/"
  in
  Uri.of_string str

let to_atom_entries (feeds:feed list) =
  let open Cow.Atom in
  Lwt_list.map_s (
    function
    | `Blog (feed,entries) ->
      Blog.to_atom ~feed ~entries
      >|= fun c -> List.map (fun e -> (e, `Blog feed)) c.entries
    | `Wiki (feed,entries) ->
      Wiki.to_atom ~feed ~entries
      >|= fun c -> List.map (fun e -> (e, `Wiki feed)) c.entries
    | `Links (feed,entries) ->
      Links.to_atom ~feed ~entries
      >|= fun c -> List.map (fun e -> (e, `Links feed)) c.entries
  ) feeds
  >|= List.flatten
  >|= List.sort
    (fun (a,_) (b,_) -> Cow.Atom.compare b.entry.updated a.entry.updated)

let to_html ?limit feeds =
  let open Cow.Html in
  let open Cow.Atom in
  to_atom_entries feeds >|=
  List.mapi (fun n ({entry; _}, info) ->
      let fa = Printf.sprintf "fa-li fa %s" (feed_icon info) in
      (* Find an alternate HTML link *)
      try
        (match limit with Some x when n > x -> raise Not_found |_ -> ());
        let uri =
          let l = List.find (fun l ->
              l.rel = `alternate && l.typ = Some "text/html"
            ) entry.links
          in
          l.href in
        let (y,m,d,_,_) = entry.updated in
        let date =
          Printf.sprintf "(%d %s %d)" d (Date.short_string_of_month m) y
        in
        list [
          a ~href:(feed_uri info) (i ~cls:fa empty);
          a ~href:uri (string entry.title);
          string " ";
          i ~cls:"front_date" (string date)
        ]
      with Not_found -> empty
    ) >|= fun fs ->
  ul ~cls:"fa-ul" fs

let permalink feed id = Printf.sprintf "%supdates/%s" feed.Atom_feed.base_uri id

let to_atom ~meta ~feeds =
  let open Cow.Atom in
  let { Atom_feed.title; subtitle; base_uri; id; rights; _ } = meta in
  let id = base_uri ^ id in
  to_atom_entries feeds >|= List.map fst >|= fun entries ->
  let updated = (List.hd entries).entry.updated in
  let links = [
    mk_link (Uri.of_string (permalink meta "atom.xml"));
    mk_link ~rel:`alternate ~typ:"text/html" (Uri.of_string base_uri)
  ] in
  let atom_feed =
    { id; title; subtitle; author=meta.Atom_feed.author; rights;
      updated; links }
  in
  { feed=atom_feed; entries }
