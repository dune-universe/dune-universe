(* 
 * iocaml - an OCaml kernel for IPython
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: log file for debugging
 *
 *)

val src : Logs.Src.t
(** Logs source for jupyter-kernel
    @since NEXT_RELEASE *)

val file_reporter : string -> Logs.reporter
[@@ocaml.deprecated "install reporter in your binary, not library"]
(** Logs reporter that writes into the given file.
    @since NEXT_RELEASE *)

val open_log_file : string -> unit
(** Open log file using [Logs].
    This will add a reporter to [Logs], and is a bit brittle as it might be
    replaced. See {!file_reporter} to build the underlying reporter
    and install it yourself.
*)

val log : string -> unit
[@@ocaml.deprecated "use `logs`"]

val logf : ('a, unit, string, unit) format4 -> 'a
[@@ocaml.deprecated "use `logs`"]


include Logs.LOG
(** Logs for the {!src} above.
    @since NEXT_RELEASE *)
