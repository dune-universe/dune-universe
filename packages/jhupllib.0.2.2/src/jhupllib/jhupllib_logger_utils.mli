(*
 * TinyBang - Simple Logging
 *
 * Features
 * --------
 *
 * 1. Always logs to the stderr, which is the best thing to do [1]. Note that the
 *    article in [1] says to log to stdout, but currently the interpreter is only
 *    run in interactive mode, so stdout is already taken and we log the next best
 *    thing, which is stderr.
 *
 * 2. Handles different log levels.
 *
 * 3. Is contextualized by the module and different log levels per module can
 *    be configured.
 *
 * 4. Is a thin layer around BatLog.
 *
 * Usage
 * -----
 *
 * 1. In `_oasis`, for the library that requires logging, add `tiny-bang-utils` to
 *    the `BuildDepends` section.
 *
 * 2. In the module that needs logging, on the top of the file, create a logger
 *    with a name that identifies the module. For example, in `tiny_bang_toploop.ml`:
 *
 *    ```ocaml
 *    let logger = Tiny_bang_logger.make_logger "Tiny_bang_toploop"
 *    ```
 *
 * 3. Add log entries. For example:
 *
 *    ```ocaml
 *    logger `debug "chunky tempeh!"
 *    ```
 *
 *    Allowed log levels are: `trace | `debug | `info | `warn | `error
 *                            | `fatal | `always.
 *
 * 4. (Optional) Log levels can be selected for the entire executable or on a
 *               per-module basis using command line arguments. Refer to the
 *               `README.md' for more information.
 *
 *
 * [1]: http://12factor.net/logs
 *)

type level = [`trace|`debug|`info|`warn|`error|`fatal|`always];;

val level_of_string : string -> level option

val set_default_logging_level : level -> unit

val set_logging_level_for : string -> level -> unit

val make_logger : string -> level -> string -> unit

val make_lazy_logger : string -> level -> (unit -> string) -> unit

val bracket_log :
  (string -> unit) ->
  string ->
  ('a -> string) ->
  (unit -> 'a) ->
  'a

val lazy_bracket_log :
  ((unit -> string) -> unit) ->
  (unit -> string) ->
  ('a -> string) ->
  (unit -> 'a) ->
  'a