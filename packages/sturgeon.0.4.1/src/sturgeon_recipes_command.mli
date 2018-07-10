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

(** WIP A few high-level entry points for session-based commands. *)

open Sturgeon

val command :
  ?greetings:Session.t ->
  ?cogreetings:(Session.t -> unit) ->
  unit -> 'a

val text_command : (args:Session.t -> Stui.shell -> unit) -> 'a
