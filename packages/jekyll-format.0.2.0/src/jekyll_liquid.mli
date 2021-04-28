(*---------------------------------------------------------------------------
   Copyright (c) 2016 Anil Madhavapeddy. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   jekyll-format v0.2.0
  ---------------------------------------------------------------------------*)

(** Jekyll Liquid templates parsing module

    {{:http://shopify.github.io/liquid/} Liquid templates} are a library for
    flexible web apps. The original library is in Ruby, and this module is
    currently just a partial implementation that handles certain tags such as
    code highlighting. *)

open Astring

module Tags : sig
  type highlight = {
    lang : string option;  (** optional language of syntax *)
    body : String.Sub.t;  (** code to be highlighted *)
    linenos : bool;  (** whether line numbers should be emitted *)
  }
  (** [highlight] represents the various syntax highlighting options. *)

  val mk_highlight :
    ?lang:string -> ?body:String.sub -> ?linenos:bool -> unit -> highlight
  (** [mk_highlight] constructs a {!highlight} value. [lang] defaults to [None],
      [body] defaults to {!Astring.String.Sub.empty} and [linenos] defaults to
      [false]. *)

  val pp_highlight : highlight Fmt.t
  (** [pp_highlight] formats a {!highlight} in human-readable format. *)

  val highlight : string -> highlight option
  (** [highlight s] attempts to parse the contents of a [{% highlight %}] tag.
      [s] should have had the tags removed via {!extract_tag} and just contain
      the tag body. *)

  val endhighlight : string -> bool
  (** [endhighlight s] checks if a [{% endhighlight %}] tag is present. [s]
      should have had the tags removed via {!extract_tag} and just contain the
      tag body. *)
end

val highlight_exn :
  ?f:(Tags.highlight option -> String.sub -> String.sub) ->
  String.sub ->
  String.sub
(** [highlight body] parses the body for Jekyll [{% highlight %}] tags and
    applies [f] to them and substitutes the result into the tag body. The
    default [f] is {!highlight_markdown_code} that transforms them into vanilla
    Markdown with no special highlighting except for a code segment. *)

val highlight_markdown_code : Tags.highlight option -> String.sub -> String.sub
(** [highlight_markdown_code s] will wrap the code in [s] in a Markdown code
    segment. This can be parsed to {!highlight_exn} as a sensible default. *)

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
