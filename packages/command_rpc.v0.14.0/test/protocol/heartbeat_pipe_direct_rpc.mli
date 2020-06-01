open! Core
open! Async
open! Import

val rpc : (int, unit, Error.t) Rpc.Pipe_rpc.t
val implementations : Command_rpc.Command.Invocation.t Rpc.Implementation.t list
