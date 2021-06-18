open Async

(** Feather_async provides functions to run Feather cmd's within an Async context. *)

val run :
  ?cwd:string -> ?env:(string * string) list -> Feather.cmd -> unit Deferred.t

val collect_stdout :
  ?cwd:string -> ?env:(string * string) list -> Feather.cmd -> string Deferred.t

val collect_lines :
  ?cwd:string ->
  ?env:(string * string) list ->
  Feather.cmd ->
  string list Deferred.t

val fzf :
  ?cwd:string ->
  ?env:(string * string) list ->
  Feather.cmd ->
  string option Deferred.t

val run_bg : [ `Use_deferred_dot_upon ]
  [@@alert
    run_bg
      {|
         [Feather_async.run_bg] intentionally shadows [Feather.run_bg].

         When using [Feather_async], use either [Deferred.upon] or
         [Async.don't_wait_for] to run things asynchronously.
|}]

val last_exit : [ `Do_not_use ]
  [@@alert
    last_exit
      {| [Feather.last_exit] does not work when using [Feather_async]. |}]
