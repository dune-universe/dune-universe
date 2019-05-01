(*---------------------------------------------------------------------------
   Copyright (c) 2019 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

val reporter : unit -> Logs.reporter
(** The following reporter will play nice with [Async]'s runtime, it
    will behave synchronously for the log functions of this module and
    asynchronously for those of the {!Logs} module (see {!Logs.sync}).

    It reuses {!Logs_fmt.reporter} and will produce colorful output if
    the standard formatters are setup to do so. For example it can be
    used instead of {!Logs_fmt.reporter} in the {{!Logs_cli.ex}full
    setup example}. *)

val level_arg : Logs.level option Async.Command.Arg_type.t
(** Argument type to be used for use in [Command] params. *)

val set_level_via_param : Logs.src option -> unit Async.Command.Param.t
(** [set_level_via_param src] is a param that sets the level of [src]
    (or all srcs if [src] is [None]). *)

(*---------------------------------------------------------------------------
   Copyright (c) 2019 Vincent Bernardoff

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
