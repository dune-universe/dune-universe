(*-------------------------------------------------------------------------
 * Copyright (c) 2020, 2021 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 *-------------------------------------------------------------------------*)

(** [Http_multipart_formdata] is a non-blocking, streaming HTTP
    [multipart/formdata] parser. Its design is based on two main ideas:

    - The parser should stream the results as soon as possible in a
      non-buffered, non-backtracking manner; and

    - The parser input must be non-blocking and incremental in nature.

    The parser implements HTTP [multipart/form-data] standard as defined in
    {{:https://tools.ietf.org/html/rfc7578} RFC 7578}. *)

(** {1 Types} *)

(** [reader] represents a HTTP multipart formdata reader. *)
type reader

(** [read] represents both the current state and data read by a {!type:reader}. *)
and read =
  [ `End  (** The reader has completed reading. *)
  | `Header of part_header  (** Multipart part header data. *)
  | `Body of Cstruct.t  (** Multipart part body data. *)
  | `Body_end  (** [reader] has completed reading the Multipart body data. *)
  | `Awaiting_input of [`Cstruct of Cstruct.t | `Eof] -> read
    (** The reader is waiting for it to be provided with input data. This is
        only returned when [`Incremental] is chosen as {!input}. *)
  | `Error of string  (** Represents an error in {!input} data. *) ]

and input =
  [ `Cstruct of Cstruct.t  (** A bigstring input. *)
  | `Incremental
    (** The caller of the library periodically provides input to the parser. *)
  ]

(** Represents a parsed multipart part header data. *)
and part_header

(** Represents the multipart boundary value. *)
and boundary

(** A form field name *)
and field_name = string

(** A Multipart body *)
and part_body = string

(** {1 Mulipart Boundary parser} *)

val boundary : string -> (boundary, string) result
(** [boundary content_type] parses [content_type] to extract {!type:boundary}
    value. [content_type] is the HTTP request [Content-Type] header value.

    {[
      let content_type =
        "multipart/form-data; \
         boundary=---------------------------735323031399963166993862150"
      in
      Http_multipart_formdata.boundary content_type
    ]} *)

(** {1 Streaming Multipart}

    API to stream multipart parts. Use these functions when you have to handle
    HTTP form submissions which has large file uploads and at the same time be
    memory efficient. *)

val reader : ?read_buffer_size:int -> boundary -> input -> reader
(** [reader ?read_buffer_size boundary input] creates reader. The default value
    for [read_buffer_size] is 1KB. *)

val read : reader -> read
(** [read reader] returns data read by [reader]. *)

val unconsumed : reader -> Cstruct.t
(** [unconsumed reader] returns any leftover data still remaining after
    {!type:reader} returns [`End]. *)

(** {1 Non-Streaming Multipart}

    Use these functions if the HTTP form submission is of a relatively small
    size. *)

val parts :
     boundary
  -> string
  -> ((field_name * (part_header * part_body)) list, string) result
(** [parts boundary http_body] returns a list of HTTP multipart parts parsed in
    [http_body].

    The returned parts list is keyed to a form field name so that one can do:

    {[
      let parts_kv = parts boundary http_body in
      match List.assoc_opt "field1" parts_vk with
      | Some v -> ...
      | None -> ..
    ]} *)

(** {1 Part header} *)

val name : part_header -> string
(** [name t] returns the form field name. *)

val content_type : part_header -> string
(** [content_type t] returns the part content-type. *)

val filename : part_header -> string option
(** [filename t] returns the uploaded filename if the multipart is a file. *)

val find : string -> part_header -> string option
(** [find name t] returns the multipart parameter value associated with [name]. *)

(** {3 Pretty Printers} *)

val pp_part_header : Format.formatter -> part_header -> unit
val pp_read_result : Format.formatter -> read -> unit
val pp_boundary : Format.formatter -> boundary -> unit
