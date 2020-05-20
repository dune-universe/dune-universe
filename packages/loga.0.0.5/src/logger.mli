(*
 * Copyright yutopp 2017 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

type t

type location_t = string * int

type 'a format_t = ('a, Format.formatter, unit, unit) format4

val create :
  severity:Severity.t -> formatter:Format.formatter -> timer:Timer.gen_t -> t

val create_default : unit -> t

val set_severity : t -> Severity.t -> unit

val set_formatter : t -> Format.formatter -> unit

val set_timer : t -> Timer.gen_t -> unit

val log : t -> Severity.t -> location_t -> 'a format_t -> 'a

val emergency : t -> location_t -> 'a format_t -> 'a

val alert : t -> location_t -> 'a format_t -> 'a

val critical : t -> location_t -> 'a format_t -> 'a

val error : t -> location_t -> 'a format_t -> 'a

val warning : t -> location_t -> 'a format_t -> 'a

val notice : t -> location_t -> 'a format_t -> 'a

val info : t -> location_t -> 'a format_t -> 'a

val debug : t -> location_t -> 'a format_t -> 'a
