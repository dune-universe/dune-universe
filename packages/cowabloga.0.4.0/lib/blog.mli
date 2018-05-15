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

module Entry : sig
  type t = {
    updated : Date.date;
    authors : Cow.Atom.author list;
    subject : string;
    permalink : string;
    body : string;
  }
  val permalink : Atom_feed.t -> t -> string
  val compare : t -> t -> int
  val to_html : feed:Atom_feed.t -> entry:t -> Cow.Html.t Lwt.t
  val to_atom : Atom_feed.t -> t -> Cow.Atom.entry Lwt.t
end

val to_html :
  ?sep:Cow.Xml.t ->
  feed:Atom_feed.t ->
  entries:Entry.t list ->
  Cow.Xml.t Lwt.t

val to_atom :
  feed:Atom_feed.t ->
  entries:Entry.t list ->
  Cow.Atom.feed Lwt.t

val recent_posts :
  ?active:string ->
  Atom_feed.t ->
  Entry.t list -> Foundation.Sidebar.t list
