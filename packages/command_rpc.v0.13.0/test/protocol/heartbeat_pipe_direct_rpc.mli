open! Core
open! Async
open! Import

include
  Command_rpc.Command.T_pipe_direct_bin_io_only
  with type query = int
   and type response = unit
   and type error = Error.t
