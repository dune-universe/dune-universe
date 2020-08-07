open Fmlib
open Alba_core

module Make (Io: Io.SIG):
sig
    val compile: string Character_parser.Located.t -> Context.t option Io.t
    val run: _ -> unit Io.t
end
