open Fmlib

module Make (Io: Io.SIG):
sig
    val run_cli:  _ -> unit Io.t
    val run_eval: _ -> unit Io.t
end
