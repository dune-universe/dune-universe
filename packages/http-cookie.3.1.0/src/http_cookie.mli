(*-------------------------------------------------------------------------
 * Copyright (c) 2019, 2020 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * http-cookie v3.1.0
 *-------------------------------------------------------------------------*)

(** A comprehensive and standards compliant HTTP cookies library for ocaml.

    HTTP cookie is serialized as follows:

    - In a [Cookie] header in a HTTP request
    - In a [Set-Cookie] header in a HTTP response.

    The library supports consuming and creating HTTP cookie in both requests and
    responses.

    The standard implemented by the library is
    {{:https://tools.ietf.org/html/rfc6265} RFC 6265}. *)

(** {1 Types} *)

(** Represents 'Same-site' cookie attribute. See
    https://tools.ietf.org/html/draft-ietf-httpbis-cookie-same-site-00. *)
module Same_site : sig
  type t = Default | None | Lax | Strict

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val to_string : t -> string
end

(** Represents a cookie name-value in [Cookie] request header or a set of cookie
    attributes in [Set-Cookie] response header. *)
type t

type date_time =
  { year: int  (** Four digit year value, e.g. 2020, 2019, 1980 etc. *)
  ; month: int  (** Begins from 0, i.e. January = 0. *)
  ; weekday: [`Sun | `Mon | `Tue | `Wed | `Thu | `Fri | `Sat]
  ; day_of_month: int  (** Day of the month value from 1 - 31. *)
  ; hour: int  (** 24 hour value from 0-23 *)
  ; minutes: int  (** Minutes value from 0 - 59*)
  ; seconds: int  (** Seconds value from 0 - 60 *) }

exception Cookie of string

(** {1 Constructors} *)

val create :
     ?path:string
  -> ?domain:string
  -> ?expires:date_time
  -> ?max_age:int
  -> ?secure:bool
  -> ?http_only:bool
  -> ?same_site:Same_site.t
  -> ?extension:string
  -> string
  -> value:string
  -> t
(** [create ~path ~domain ~expires ~max_age ~secure ~http_only ~same_site ~extension name
    ~value]
    returns a cookie instance {!type:t} with cookie name [name] and value
    [value] along with the given attributes.

    @raise Cookie if any of the cookie attributes fail validation. *)

val of_cookie_header : string -> t list
(** [of_cookie_header s] parses [s] - a string value which represents HTTP
    [Cookie] header value as defined in
    {:https://tools.ietf.org/html/rfc6265#section-4.2} and returns a list of
    [Cookie]s.

    {4 Examples}

    This returns two cookies with cookie names [SID] and [lang-en].

    {[ Http_cookie.of_cookie_header "SID=31d4d96e407aad42; lang=en-US" ]} *)

val to_set_cookie_header_value : t -> string
(** [to_set_header c] serializes cookie [c] into a string which can be encoded
    as value for HTTP [Set-Cookie] header.

    The datetime format for [expires] attribute is specified in
    {{:https://tools.ietf.org/html/rfc2616#section-3.3.1} RFC 2616}

    Example of a string returned by the function,

    {v
SID=31d4d96e407aad42; Path=/; Secure; HttpOnly; Expires=Sun, 06 Nov 1994 08:49:37 GMT
    v} *)

val to_cookie_header_value : t -> string
(** [to_cookie_header c] serializes [c] into a string which can be encoded as
    value for HTTP [Cookie] header.

    Example of a string returned by the function.

    {v SID=31d4d96e407aad42 v} *)

(** {1 Query Cookie Attributes}

    Cookie attributes are defined precisely at
    {{:https://tools.ietf.org/html/rfc6265#section-4.1.1} RFC 6262} *)

val name : t -> string
(** [name t] returns a cookie name.

    See {{:https://tools.ietf.org/html/rfc6265#section-4.1.1} cookie-name} *)

val value : t -> string
(** [value t] returns a cookie value.

    See {{:https://tools.ietf.org/html/rfc6265#section-4.1.1} cookie-value} *)

val path : t -> string option
(** [path t] returns cookie path attribute.

    See {{:https://tools.ietf.org/html/rfc6265#section-5.2.4} cookie-path} *)

val domain : t -> string option
(** [domain t] returns cookie domain attribute.

    See {{:https://tools.ietf.org/html/rfc6265#section-4.1.2.3} cookie-domain} *)

val expires : t -> date_time option
(** [expires t] returns a coookie expires attribute.

    See {{:https://tools.ietf.org/html/rfc6265#section-4.1.2.1.} cookie-expires} *)

val max_age : t -> int option
(** [max_age t] returns a cookie max_age attribute.

    See {{:https://tools.ietf.org/html/rfc6265#section-4.1.2.2} max-age} and
    {{:https://tools.ietf.org/html/rfc6265#section-4.1.1} max-age-av} *)

val secure : t -> bool option
(** [secure t] returns a secure attribute.

    See {{:https://tools.ietf.org/html/rfc6265#section-4.1.2.5} cookie-secure} *)

val http_only : t -> bool option
(** [http_only t] returns a http_only attribute.

    See {{:https://tools.ietf.org/html/rfc6265#section-4.1.2.6} http-only} *)

val same_site : t -> Same_site.t option
(** [same_site t] returns a same_site attribute.

    See {{:https://tools.ietf.org/html/draft-ietf-httpbis-cookie-same-site-00}
    same-site} *)

val extension : t -> string option
(** [extension t] returns a cookie extension value.

    See {{:https://tools.ietf.org/html/rfc6265#section-4.1.1} cookie-extension} *)

(** {1 Compare} *)

val compare : t -> t -> int
(** [compare c1 c2] returns [0] if [c1] and [c2] are equal, a positive integer
    if [c1] is greater than [c2] and a negative integer if [c1] is less than
    [c2] *)

(** {1 Updates} *)

val update_value : string -> t -> t
val update_name : string -> t -> t
val update_path : string option -> t -> t
val update_domain : string option -> t -> t
val update_expires : date_time option -> t -> t
val update_max_age : int option -> t -> t
val update_secure : bool option -> t -> t
val update_http_only : bool option -> t -> t
val update_same_site : Same_site.t option -> t -> t
val update_extension : string option -> t -> t
