open Opam_compiler

val github_client_fail_all : Github_client.t

val runner_fail_all : Runner.t

val ( let$ ) : 'a * (unit -> unit) -> ('a -> 'b) -> 'b
(** Run a cleanup function at the end of a let binding. *)
