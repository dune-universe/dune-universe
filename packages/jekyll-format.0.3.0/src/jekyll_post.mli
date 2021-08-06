(*---------------------------------------------------------------------------
   Copyright (c) 2016 Anil Madhavapeddy. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   jekyll-format v0.3.0
  ---------------------------------------------------------------------------*)

(** Jekyll blog post handling library

    {{:https://jekyllrb.com} Jekyll} is a simple, blog-aware static site
    generator that takes a template directory of files and turns them into a
    website. This library exists to parse those blog posts and make them easy to
    manipulate from OCaml code.

    {e v0.3.0 — {{:https://github.com/avsm/jekyll-format} homepage}} *)

type t = {
  fname : string option;
  title : string;
  date : Ptime.t;
  slug : string;
  body : Jekyll_format.body;
  fields : Jekyll_format.fields;
}
(** [t] is a single Jekyll-format post that has been parsed *)

val of_string : ?fname:string -> string -> (t, [> Rresult.R.msg ]) Result.result
(** [of_string ?fname body] *)

val of_string_exn : ?fname:string -> string -> t
(** [of_string_exn ?fname body] operates as {!of_string} except that it raises a
    {!Jekyll_format.Parse_failure} exception on error. *)

val compare : t -> t -> int
(** [compare a b] will compare by date and then lexigraphically on the title *)

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
