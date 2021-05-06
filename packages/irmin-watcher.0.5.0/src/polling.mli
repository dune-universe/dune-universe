(*---------------------------------------------------------------------------
   Copyright (c) 2016 Thomas Gazagnaire. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   irmin-watcher 0.5.0
  ---------------------------------------------------------------------------*)

(** Active polling backend for Irmin watchers.

    {e 0.5.0 — {{:https://github.com/mirage/irmin-watcher} homepage}} *)

open Core

val with_delay : float -> t
(** [with_delay delay id p f] is the hook calling [f] everytime a sub-path of
    [p] is modified. Return a function to call to remove the hook. Active
    polling is done every [delay] seconds. *)

val v : t Lazy.t
(** [v] is [with_delay !default_polling_time]. *)

val mode : [ `Polling ]

(*---------------------------------------------------------------------------
   Copyright (c) 2016 Thomas Gazagnaire

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
