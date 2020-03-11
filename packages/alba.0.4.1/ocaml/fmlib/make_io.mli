open Module_types

(**

    This module contains a minimal signature for an io module and a functor
    which converts a minimal implementation into a full implementation of an io
    module.

*)


module type SIG =
sig
    type in_file
    type out_file

    val stdin:  in_file
    val stdout: out_file
    val stderr: out_file

    module M: MONAD
    include MONAD


    val exit: int -> 'a t
    val execute: unit t -> unit
    val command_line: string array t
    val current_working_directory: string  t



    val cli_loop:
        'a
        -> ('a -> string option)
        -> ('a -> string -> 'a t)
        -> ('a -> 'a t)
        -> 'a t


    val path_separator: char
    val path_delimiter: char

    val read_directory: string -> string array option t

    module Read: functor (W:WRITABLE) ->
                 sig
                   val read_buffer: in_file -> W.t -> W.t t
                   val read: in_file -> W.t -> W.t t
                 end
    module Write: functor (R:READABLE) ->
                  sig
                    val write_buffer: out_file -> R.t -> R.t t
                    val write: out_file -> R.t -> R.t t
                  end
end




module Make (Base: SIG): Io.SIG
(** [Make (Base)] transforms a basic implementation of an io module which
conforms to the signature [SIG] into a full implementation of an io module. *)

