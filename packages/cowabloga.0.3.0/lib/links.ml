(*
 * Copyright (c) 2010-2013 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2013 Richard Mortier <mort@cantab.net>
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

open Printf
open Lwt.Infix
open Cow
open Atom_feed
open Date

(** Link stream *)
type stream = {
  name: string;
  icon: string;
}

(* Individual link *)
type t = {
  id: string;
  uri: Uri.t;
  title: string;
  date: Date.date;
  stream: stream;
}

let permalink feed l =
  sprintf "%slinks/%s/%s" feed.base_uri l.stream.name l.id

let atom_entry_of_link (feed:Atom_feed.t) l =
  let perma_uri = Uri.of_string (permalink feed l) in
  let links = [
    (*  Atom.mk_link ~rel:`alternate ~typ:"text/html" perma_uri; *)
    Atom.mk_link ~rel:`alternate ~typ:"text/html" l.uri;
  ] in
  let content =
    Html.(list [
        a ~href:l.uri (string l.title);
        string " from ";
        string l.stream.name
      ])
  in
  let meta = {
    Atom.id      = Uri.to_string perma_uri;
    title        = l.title;
    subtitle     = None;
    author       = None;
    updated      = atom_date l.date;
    rights       = feed.rights;
    links;
  } in
  Lwt.return { Atom.entry = meta; summary= None; base= None; content }

let cmp_ent a b =
  Atom.compare (atom_date a.date) (atom_date b.date)

let to_atom ~feed ~entries =
  let mk_uri x = Uri.of_string (feed.base_uri ^ x) in
  let es = List.rev (List.sort cmp_ent entries) in
  let updated = atom_date (List.hd es).date in
  let id = feed.base_uri ^ "links/" in
  let links = [
    Atom.mk_link (mk_uri "atom.xml");
    Atom.mk_link ~rel:`alternate ~typ:"text/html" (mk_uri "")
  ] in
  Lwt_list.map_s (atom_entry_of_link feed) es >|= fun entries ->
  let feed = {
    Atom.id;
    title = feed.Atom_feed.title;
    subtitle = feed.subtitle;
    author = feed.Atom_feed.author;
    rights = feed.rights;
    updated;
    links
  } in
  { Atom.feed=feed; entries }
