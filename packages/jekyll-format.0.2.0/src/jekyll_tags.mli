(*---------------------------------------------------------------------------
   Copyright (c) 2016 Anil Madhavapeddy. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   jekyll-format v0.2.0
  ---------------------------------------------------------------------------*)

(** Parse Jekyll-format delimited tags from message bodies *)

(** {1 General tag parsing} *)

open Astring

val extract_tag :
  ?start:int ->
  start_tag:string ->
  stop_tag:string ->
  string ->
  (int * string * int) option
(** [extract_tag ?start ~start_tag ~stop_tag s] will extract the indices that
    represents the [text] in ["<start><ws><text><ws><stop>"]. Whitespace is
    trimmed, and [None] is returned if non-empty text could not be parsed.

    @return starting index of the tag within the string, the tag contents with
    whitespace trimmed, and the index of the first character after the end tag
    (which may be out of bounds of the string) *)

val extract_tags :
  ?start:int ->
  start_tag:string ->
  stop_tag:string ->
  string ->
  (int * string * int) list
(** [extract_tags ?start ~start_tag ~stop_tag s] will extract the list of
    indices that represent the [text] in
    ["<start_Tag><ws><text><ws><stop_tag>"]. Whitespace is trimmed from the
    [text] body.

    @return list of tuples of the starting index of the tag within the string,
    the tag contents with whitespace trimmed, and the index of the first
    character after the end tag (which may be out of bounds of the string) *)

val map_tag : sub:String.sub -> int * string * int -> String.sub -> String.sub
(** [map_tag ~sub tag_info body] will substitute the tag_info (typically
    returned by {!extract_tag} with the value of [sub] *)

val map_tags :
  start_tag:string ->
  stop_tag:string ->
  f:(string -> string option) ->
  String.sub ->
  String.sub
(** [map_tags ~start_tag ~stop_tag ~f body] will apply the function [f] to all
    the tags found that match [start_tag] and [stop_tag]. The scanning is done
    via {!extract_tags} and covers all occurrences in [body]. [f] should return
    [None] if the tag is to be skipped, and [Some sub] where [sub] is the string
    to substitute into the tag body. *)

val map_tag_bodies :
  start_tag:string ->
  stop_tag:string ->
  f_start:(string -> 'a option) ->
  f_stop:(string -> bool) ->
  f_map:('a option -> String.sub -> String.sub) ->
  String.sub ->
  String.sub
(** [map_tag_bodies ~start_tag ~stop_tag ~f_start ~f_stop ~f_map body] searches
    for tags delimited by [start_tag] and [stop_tag]. The tags are pattern
    matched using [f_start] to find the starting tag, and [f_stop] for the end
    tag. When a matching tag pair is found, a substitution is generated using
    [f_map], and the resulting substitution is returned with the rest of the
    string unchanged. [f_start] can pass [Some args] (parsed from the start tag
    body to the [f_map] function so that tag arguments can be handled by the
    processor. *)

(** {1 Jekyll liquid tag parsing functions} *)

val extract_liquid_tag : ?start:int -> string -> (int * string * int) option
(** [extract_liquid_tag] behaves as {!extract_tag} but is specialised to parse
    Jekyll liquid tags of the form [{% ... %}]. *)

val extract_liquid_tags : ?start:int -> string -> (int * string * int) list
(** [extract_liquid_tags] behaves as {!extract_tags} but is specialised to parse
    Jekyll liquid tags of the form [{% ... %}]. *)

val map_liquid_tags : f:(string -> string option) -> String.sub -> String.sub
(** [map_liquid_tags ~f body] behaves as {!map_tags} but is specialised to parse
    Jekyll liquid tags of the form [{% ... %}]. *)

val map_liquid_tag_bodies :
  f_start:(string -> 'a option) ->
  f_stop:(string -> bool) ->
  f_map:('a option -> String.sub -> String.sub) ->
  String.sub ->
  String.sub
(** [map_liquid_tag_bodies] operates as {!map_tag_bodies} but is specialised to
    parse Jekyll liquid tags of the form [{% ... %}]. *)

(*---------------------------------------------------------------------------
   Copyright (c) 2016 Anil Madhavapeddy

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
