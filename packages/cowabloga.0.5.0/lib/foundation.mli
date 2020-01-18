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

module Link : sig
  type t = string * Uri.t
  type links = t list

  val link : ?cl:string -> string * Uri.t -> Cow.Xml.t

  val top_nav : ?align:[< `Left | `Right > `Right ] -> links -> Cow.Xml.t
  val button_group : links -> Cow.Xml.t
  val side_nav : links -> Cow.Xml.t
  val bottom_nav : links -> Cow.Xml.t
end

module Sidebar : sig
  type t = [
    | `active_link of Link.t
    | `divider
    | `link of Link.t
    | `text of string
    | `html of Cow.Html.t
  ]
  val t : title:string -> content:t list -> Cow.Xml.t
end

module Index : sig
  val t: top_nav:Cow.Html.t -> Cow.Html.t
end

module Blog : sig
  val post: title:string * Uri.t -> authors:(string * Uri.t) list -> date:Cow.Html.t -> content:Cow.Html.t -> Cow.Html.t

  val t: title:string -> subtitle:string option -> sidebar:Cow.Html.t -> posts:Cow.Html.t -> copyright:Cow.Html.t -> unit -> Cow.Html.t

end

val body:
  ?google_analytics:(string * string) -> ?highlight:string
  -> title:string
  -> headers:Cow.Html.t -> content:Cow.Html.t -> trailers:Cow.Html.t
  -> unit -> Cow.Html.t

val top_nav : title:Cow.Html.t -> title_uri:Uri.t -> nav_links:Cow.Html.t -> Cow.Html.t

val page: body:Cow.Html.t -> string
