(*
 * Copyright 2019 Cedric LE MOIGNE, cedlemo@gmx.com
 * This file is part of OCaml-GObject-Introspection.
 *
 * OCaml-GObject-Introspection is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * OCaml-GObject-Introspection is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with OCaml-GObject-Introspection.  If not, see <http://www.gnu.org/licenses/>.
 *)

(** Flags defined in the module Signals of the GObject library
    The signal flags are used to specify a signal's behaviour, the overall
    signal description outlines how especially the RUN flags control the stages
    of a signal emission. *)
type flags =
  | Run_first     (** Invoke the object method handler in the first
                      emission stage. *)
  | Run_last      (** Invoke the object method handler in the third
                      emission stage. *)
  | Run_cleanup   (** Invoke the object method handler in the last
                      emission stage. *)
  | No_recurse    (** Signals being emitted for an object while currently
                      being in emission for this very object will not be
                      emitted recursively, but instead cause the first
                      emission to be restarted. *)
  | Detailed      (** This signal supports "::detail" appendices to the
                      signal name upon handler connections and emissions. *)
  | Action        (** Action signals are signals that may freely be
                      emitted on alive objects from user code via
                      g_signal_emit() and friends, without the need of
                      being embedded into extra code that performs pre or
                      post emission adjustments on the object. They can
                      also be thought of as object methods which can be
                      called generically by third-party code. *)
  | No_hooks      (** No emissions hooks are supported for this signal. *)
  | Must_collect  (** Varargs signal emission will always collect the
                      arguments, even if there are no signal handlers
                      connected. Since 2.30. *)
  | Deprecated    (** The signal is deprecated and will be removed in a
                      future version. A warning will be generated if it is
                      connected while running with G_ENABLE_DIAGNOSTIC=1.
                      Since 2.32. *)

let string_of_flag = function
  | Run_first -> "Run_first"
  | Run_last -> "Run_last"
  | Run_cleanup -> "Run_cleanup"
  | No_recurse -> "No_recurse"
  | Detailed -> "Detailed"
  | Action -> "Action"
  | No_hooks -> "No_hooks"
  | Must_collect -> "Must_collect"
  | Deprecated -> "Deprecated"

module Flags = functor (T : Cstubs.Types.TYPE) -> struct
  let g_signal_run_first = T.constant "G_SIGNAL_RUN_FIRST" T.int64_t
  let g_signal_run_last = T.constant "G_SIGNAL_RUN_LAST" T.int64_t
  let g_signal_run_cleanup = T.constant "G_SIGNAL_RUN_CLEANUP" T.int64_t
  let g_signal_no_recurse = T.constant "G_SIGNAL_NO_RECURSE" T.int64_t
  let g_signal_detailed = T.constant "G_SIGNAL_DETAILED" T.int64_t
  let g_signal_action = T.constant "G_SIGNAL_ACTION" T.int64_t
  let g_signal_no_hooks = T.constant "G_SIGNAL_NO_HOOKS" T.int64_t
  let g_signal_must_collect = T.constant "G_SIGNAL_MUST_COLLECT" T.int64_t
  let g_signal_deprecated = T.constant "G_SIGNAL_DEPRECATED" T.int64_t
  let flags = T.enum "GSignalFlags" ~typedef:true [] ~unexpected:(fun x -> x)
end
