(** Implements a value that is either in a file, or in memory, but not both. Is used by
    live and the friend to store sequence numbers and counters. If the value is moved to
    memory, changed, and then the process crashes, the file will correctly reflect that
    the value has been lost. *)

open Core
open Import

module type Arg = Sexpable

module type S = sig
  type persistent_singleton

  val load
    :  string
    -> default:persistent_singleton
    -> persistent_singleton Or_error.t Deferred.t

  val load'
    :  string
    -> default:persistent_singleton
    -> [ `Ok of persistent_singleton
       | `Can_not_load_due_to_unclean_shutdown
       | `Can_not_determine_whether_file_exists
       ] Deferred.t

  val load_exn
    :  string
    -> default:persistent_singleton
    -> persistent_singleton Deferred.t

  val save
    :  string
    -> value:persistent_singleton
    -> unit Deferred.t
end

module Make (Z : Arg) : S with type persistent_singleton = Z.t = struct
  type persistent_singleton = Z.t

  let save_sexp file option =
    Writer.save_sexp file (Option.sexp_of_t Z.sexp_of_t option)
  ;;

  let load' file ~default =
    Sys.file_exists file
    >>= function
    | `Unknown -> return `Can_not_determine_whether_file_exists
    | `No      -> return (`Ok default)
    | `Yes     ->
      Reader.load_sexp_exn file (Option.t_of_sexp Z.t_of_sexp)
      >>= fun res ->
      save_sexp file None
      >>| fun () ->
      match res with
      | Some res -> `Ok res
      | None     -> `Can_not_load_due_to_unclean_shutdown
  ;;

  let load file ~default =
    load' file ~default
    >>| function
    | `Ok x -> Ok x
    | `Can_not_load_due_to_unclean_shutdown ->
      error_s [%message [%here] "can not load due to unclean shutdown" (file : string)]
    | `Can_not_determine_whether_file_exists ->
      error_s [%message [%here] "can not determine whether file exists" (file : string)]
  ;;

  let load_exn file ~default = load file ~default >>| ok_exn

  let save file ~value = save_sexp file (Some value)
end
