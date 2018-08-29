(*
Copyright (2010-2014) INCUBAID BVBA

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*)

module Hotc : sig
  type t
  type bdb = Otc.Bdb.bdb
  type bdbcur = Otc.Bdb.bdbcur

  val filename : t -> string
  val transaction :  t -> (bdb -> 'd Lwt.t) -> 'd Lwt.t
  val with_cursor :  bdb -> (bdb -> Otc.Bdb.bdbcur -> 'e Lwt.t) -> 'e Lwt.t
  val batch : t -> int -> string -> string option -> (string * string) list Lwt.t
  val get_bdb : t -> bdb
  val read : t -> (bdb -> 'b Lwt.t) -> 'b Lwt.t
  val create: ?mode:int -> ?lcnum:int -> ?ncnum:int ->
    string -> Otc.Bdb.opt list -> t Lwt.t
  val delete: t -> unit Lwt.t
  val optimize: t -> unit Lwt.t
  val reopen: t -> (unit -> unit Lwt.t) -> int -> unit Lwt.t
  val do_locked : t -> (unit -> 'a Lwt.t) -> 'a Lwt.t
  val sync : t -> unit
  val sync_nolock : t -> unit
  val close : t -> unit Lwt.t
  val defrag : ?step:int64 -> t -> int Lwt.t
end
