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

(** An API to interconnect Sturgeon and Inuit. *)

open Inuit

(** Set of flags recognized by Sturgeon interface. *)
type flag = [ `Clickable | `Clicked | `Editable | `Prompt | `Focus
            | `Custom of (string * Sturgeon_sexp.basic) ]

(** A buffer shell is the abstract object representing the connection
    to the user-interface display.
    Through a shell, you can create one or more buffers.

    TODO:
    - bind [message] function from emacs for reporting information *)
type shell

(** [buffer_greetings] provide you with a session and a shell.
    Send the session as a greetings to emacs instance and the shell will
    open and display buffers in this instance. *)
val buffer_greetings : unit -> Sturgeon_session.t * shell

val message : shell -> string -> unit

type buffer

(** Create patch socket (low-level) from a shell. *)
val create_buffer : shell -> name:string -> buffer

(** *)
val open_cursor : buffer -> flag cursor
val manual_connect : buffer -> flag patch socket -> unit

(** Create cursor ready to output in a buffer. *)
val create_cursor : shell -> name:string -> flag cursor

(** Auxiliary interactions *)

type 'a menu = string * [ `Item of 'a | `Sub of 'a menu list ]

val popup_menu :
  shell -> string -> 'a menu list -> 'a Sturgeon_session.cont -> unit

val read_file_name :
  shell -> prompt:string -> ?dir:string -> ?default:string ->
  string Sturgeon_session.cont -> unit

val fit_to_window : buffer -> unit

val split : buffer -> name:string -> [`Left|`Right|`Top|`Bottom] -> buffer
