(******************************************************************************)
(*                                                                            *)
(*                                  Monolith                                  *)
(*                                                                            *)
(*                              François Pottier                              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU Lesser General Public License as published by the Free   *)
(*  Software Foundation, either version 3 of the License, or (at your         *)
(*  option) any later version, as described in the file LICENSE.              *)
(*                                                                            *)
(******************************************************************************)

open Printf

(* -------------------------------------------------------------------------- *)

(* [abort()] aborts the process by sending a SIGABRT signal to itself. We
   seldom need to use it directly, as letting an uncaught exception escape has
   the same effect. We use it only when we wish to abort without displaying a
   backtrace. *)

let abort () =
  flush stdout;
  flush stderr;
  let self = Unix.getpid() in
  Unix.kill self Sys.sigabrt;
  (* This point should be unreachable. *)
  assert false

(* -------------------------------------------------------------------------- *)

(* Misuses of the library by the user are reported as follows. *)

(* We use [abort] rather than [exit], because we do not want a misuse to go
   unnoticed when running under AFL. *)

let error format =
  ksprintf (fun msg ->
    printf "Error: %s\n%!" msg;
    abort()
  ) format
