(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.md.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

(** OCaml bindings for [libdlm(3)].

    {e %%VERSION%% - {{:%%PKG_HOMEPAGE%% }homepage}}.

    All operations require [DLM] (Linux Distributed Lock Manager) to be running.

    Consult the {{!lockspaces}lockspaces}, and {{!locks}locks} documentation or
    read the {{!example}example}.

    {3 References.}

    - {e {{:http://people.redhat.com/ccaulfie/docs/rhdlmbook.pdf}
    Programming Locking Applications.}}
*)

(**
{1:lockspaces Lockspaces}

   Lockspaces are identified by a cluster-wide name, they are intended as
   private namespaces for locks belonging to a single application.
*)

type t
(** the type of a DLM lockspace *)

(** [join ?mode lockspace] creates and joins the specified [lockspace]
    on the current node.
    
    Requires CAP_SYSADMIN privileges, access to the created lockspace is
    controlled by [mode].

    @raise Unix.Unix_error on failure
*)
val join : ?mode:PosixTypes.mode_t -> string -> unit Lwt.t

(** [leave ?force lockspace] leaves the specified [lockspace] on the current node.
@param force will leave the lockspace even if the current node has active locks
*)
val leave : ?force:bool -> string -> unit Lwt.t

(** [with_lockspace lockspace ~f] opens an existing [lockspace],
    calls [f t] with the lockspace handle [t], and closes the lockspace
    after [f] terminates.
    The lockspace is also automatically closed on process exit.
*)
val with_lockspace :
  string -> f:(t -> 'a Lwt.t) -> 'a Lwt.t

(**
   {1:locks Locks}

   Locks are a named cluster-wide resource inside a specific {{!lockspaces}lockspace}.

   Only a simplified interface is provided that acquires a lock, performs an operation and releases it.
   Locking can be nested, even with same lock name, but note that trying to acquire an exclusive lock twice will fail.

   It is recommended to use the {!const:LKM_PRMODE} mode for reading and {!const:LKM_EXMODE} for writing.
*)

(** lock mode *)
type mode =
  | LKM_NLMODE (** null lock - just allocate the lock, used internally by {!val:with_lock} *)
  | LKM_CRMODE (** concurrent read - read while others can read/write. *)
  | LKM_CWMODE (** concurrent write - read/write while others can read/write. *)
  | LKM_PRMODE (** protected read - read, while others can only read. *)
  | LKM_PWMODE (** protected write - read/write, while others can only read *)
  | LKM_EXMODE (** exclusive - exclusive read/write, others have no access *)

(** [with_lock lshandle ?mode ?try_ ?timeout lockname ~f]
    acquires [lockname] in lock[mode] and calls [f] when the lock is acquired.
    Releases the lock after [f] terminates.

    @param mode lock mode defaults to {!const:LKM_EXMODE}
    @param timeout specifies how long to wait for the lock to be acquired
    @param try_ fail with EAGAIN if the lock cannot be granted immediately
    @raise Unix.Unix_error if the lock cannot be granted
*)
val with_lock :
  t ->
  ?mode:mode ->
  ?try_:bool ->
  ?timeout:float -> string -> f:(unit -> 'a Lwt.t) -> 'a Lwt.t

(**
   {1:example Example}

   Need to be run as root, and with a working [DLM].
   You can use [dlm_tool status] to check for a working [DLM].
   {[
     #use "topfind";;
     #require "dlm";;
     open Lwt.Infix;;

     let lockspace = "myapp" in
     Dlm.join lockspace >>= fun () ->
     Dlm.with_lockspace lockspace (fun ls ->
         Dlm.with_lock ls "mylock1" ~f:(fun () ->
             (* acquired exclusive lock *)
             Lwt.return_unit
           ) >>= fun () ->
         Dlm.with_lock ~mode:Dlm.LKM_PRMODE ls "mylock1" ~f:(fun () ->
             (* acquired shared read lock *)
             Lwt.return_unit
           )
       ) >>= fun () ->
     Dlm.leave lockspace
   ]}
*)
