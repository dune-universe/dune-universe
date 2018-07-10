(* {{{ COPYING *(

  This file is part of Sturgeon, a toolkit for remote higher-order control
  flow.

  Copyright (C) 2016  Frédéric Bour  <frederic.bour(_)lakaban.net>

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  The Software is provided "as is", without warranty of any kind, express or
  implied, including but not limited to the warranties of merchantability,
  fitness for a particular purpose and noninfringement. In no event shall
  the authors or copyright holders be liable for any claim, damages or other
  liability, whether in an action of contract, tort or otherwise, arising
  from, out of or in connection with the software or the use or other dealings
  in the Software.

)* }}} *)

(** This is an implementation of (a subset of) Emacs s-expressions. *)

(** {1 Parametric S-exp} *)

type 'a sexp =
    C of 'a sexp * 'a sexp  (** cons cell   *)
  | S of string       (** 'sym        *)
  | T of string       (** "text"      *)
  | P of 'a sexp      (** #(property) *)
  | I of int          (** 1           *)
  | F of float        (** 1.0         *)
  | V of 'a sexp list (** [vectors]   *)
  | M of 'a           (** user-defined construction, outside of s-exp language *)

(** Recursively transform a sexp.
    [map] function is applied on each atom and at the root of each list *)
val transform_list : inj:('a -> 'b sexp) -> ?map:('b sexp -> 'b sexp) -> 'a sexp -> 'b sexp

(** Recursively transform a sexp.
    [map] function is applied on each atom and each cons-cell *)
val transform_cons : inj:('a -> 'b sexp) -> ?map:('b sexp -> 'b sexp) -> 'a sexp -> 'b sexp

(** nil constant: S "nil" *)
val sym_nil : 'a sexp

(** t constant: S "t" *)
val sym_t : 'a sexp

(** Build a sexp list,
    []      -> nil
    x :: xs -> C (x, sexp_of_list xs)
*)
val sexp_of_list : ?tail:'a sexp -> 'a sexp list -> 'a sexp

(** Does an element belong to a sexp list? *)
val sexp_mem : 'a sexp -> 'a sexp -> bool

(** {1 Monomorphic Emacs S-exp format} *)

type void

val void: void -> 'a

type basic = void sexp

(** Recover polymorphism *)
val generalize_basic : basic -> 'a sexp

(** {1 Low-level IO} *)

(** Serialize an s-exp by repetively calling a string printing function. *)
val tell_sexp : (string -> unit) -> basic -> unit

(** Read an basic by repetively calling a character reading function.

    The character reading function can return '\000' to signal EOF.

    Returns the basic and, if any, the last character read but not part of the
    sexp, or '\000'.

    If the basic is not well-formed, a Failure is raised.  You can catch it and
    add relevant location information.
    The error is always due to the last call to the reading function, which
    should be enough to locate the erroneous input, except for unterminated
    string.
*)
val read_sexp : (unit -> char) -> basic * char

(** {1 Higher-level IO} *)

val to_buf : basic -> Buffer.t -> unit

val to_string : basic -> string

val of_string : string -> basic

(** Read from a file descriptor.

    [on_read] is called before a potentially blocking read is done, so that you
    can act before blocking (select, notify scheduler ...).

    Partial application (stopping before the last [()]) allows to read a stream
    of sexp.
*)
val of_file_descr :
  on_read:(Unix.file_descr -> unit) -> Unix.file_descr -> unit -> basic option

(** Read from a channel.

    Partial application (stopping before the last [()]) allows to read a stream
    of sexp.
*)
val of_channel : in_channel -> unit -> basic option
