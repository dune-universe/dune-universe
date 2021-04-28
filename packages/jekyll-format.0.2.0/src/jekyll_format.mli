(*---------------------------------------------------------------------------
   Copyright (c) 2016 Anil Madhavapeddy. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   jekyll-format v0.2.0
  ---------------------------------------------------------------------------*)

(** Jekyll blog post parsing module *)

(** {1 Types and accessors} *)

type t
(** [t] is a single Jekyll-format post that has been parsed *)

type fields
(** [fields] represents the YAML front matter in the blog post *)

type body = string
(** [body] represents the blog post content, probably in Markdown format *)

val fields : t -> fields
(** [fields t] retrieves the YAML front matter fields from the blog post *)

val fields_to_yaml : fields -> Yaml.value
(** [fields_to_yaml fs] converts [fs] to a [Yaml.value] *)

val body : t -> body
(** [body t] retrieves the blog post content from the blog post *)

(** {1 YAML metadata} *)

val find : string -> fields -> Yaml.value option
(** [find key t] retrieves the first [key] from the YAML front matter, and
    [None] if the key is not present. Keys are case-sensitive as per the YAML
    specification. Whitespace is trimmed around the field value. *)

val keys : fields -> string list
(** [keys f] retrieves all of the key names in the YAML front matter. *)

val title :
  ?fname:string -> fields -> (string, [> Rresult.R.msg ]) Result.result
(** [title ?fname f] will query the title from the YAML metadata, and fallback
    to parsing the optional [fname] filename of the post if no explicit key is
    found. If nothing works then [None] is returned. *)

val title_exn : ?fname:string -> fields -> string
(** [title_exn ?fname f] operates as {!title} except that it raises a
    {!Parse_failure} exception on error. *)

val date :
  ?fname:string -> fields -> (Ptime.t, [> Rresult.R.msg ]) Result.result
(** [date ?fname f] will query the post date from the YAML metadata, and
    fallback to parsing the optional [fname] filename of the post if no explicit
    key is found. *)

val date_exn : ?fname:string -> fields -> Ptime.t
(** [date_exn ?fname f] operates as {!date} except that it raises a
    {!Parse_failure} in the error case. *)

val slug : ?fname:string -> fields -> (string, [> Rresult.R.msg ]) Result.result
(** [slug ?fname f] will query the slug name from the YAML metadata, or
    calculate it from the filename if no explicit slug field is set, and finally
    fallback to parsing the {!title} of the post if nothing else is found. The
    slug is calculated using {!slug_of_string}. *)

val slug_exn : ?fname:string -> fields -> string
(** [slug_exn ?fname f] operates as {!slug} except that it raises a
    {!Parse_failure} in the error case. *)

(** {1 Conversion functions} *)

val of_string : string -> (t, [> Rresult.R.msg ]) result
(** [of_string t] parses a Jekyll-format blog post and either returns a {!t} or
    signals an error in the result. *)

val of_string_exn : string -> t
(** [of_string_exn t] parses a Jekyll-format blog post and either returns a {!t}
    or raises a {!Parse_failure} exception with the error string. *)

val slug_of_string : string -> string
(** [slug_of_string s] replaces all non-ascii characters ([a..zA..Z0..9]) with
    the [-] hyphen character. The result is also lowercase. *)

val parse_filename :
  string -> (Ptime.t * string * string, [> Rresult.R.msg ]) Result.result
(** [parse_filename f] parses a Jekyll format filename
    [YEAR-MONTH-DAY-title.MARKUP] and returns the time, title and markup
    components respectively. If the time could not be parsed, then the header is
    assumed to be the title and [None] is returned for the time. *)

val parse_filename_exn : string -> Ptime.t * string * string
(** [parse_filename_exn f] operates as {!parse_filename} except that it raises a
    {!Parse_failure} in the error case. *)

val parse_date :
  ?and_time:bool -> string -> (Ptime.t, [> Rresult.R.msg ]) Result.result
(** [parse_date ?and_time s] parses a Jekyll format date field in
    [YYYY-MM-DD HH:MM:SS +/-TT:TT] format, where the HMS and timezone components
    are optional. [and_time] defaults to true and causes the non-date components
    to be parsed; setting it to false only causes the YMD portions to be parsed. *)

val parse_date_exn : ?and_time:bool -> string -> Ptime.t
(** [parse_date_exn ?and_time s] operates as {!parse_date} except that it raises
    a {!Parse_failure} in the error case. *)

exception Parse_failure of string
(** Exception raised on parse failure by the [_exn] functions in this module.
    The argument is a human-readable error message. *)

(** {1 Pretty printers} *)

val pp : t Fmt.t
(** [pp t] prints out the blog post and YAML front matter, using {!pp_fields}
    and {!pp_body} respectively. *)

val pp_body : body Fmt.t
(** [pp_body body] prints out the blog post [body] in the original layout. *)

val pp_fields : fields Fmt.t
(** [pp_fields f] prints out the YAML front matter in the original layout. *)

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
