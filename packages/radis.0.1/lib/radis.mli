(*****************************************************************************)
(*  Copyright (C) 2018 Romain Calascibetta                                   *)
(*                                                                           *)
(*  This program is free software: you can redistribute it and/or modify     *)
(*  it under the terms of the GNU Lesser General Public License as           *)
(*  published by the Free Software Foundation.                               *)
(*                                                                           *)
(*  This program is distributed in the hope that it will be useful,          *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(*  GNU General Public License for more details.                             *)
(*                                                                           *)
(*  You should have received a copy of the GNU General Public License        *)
(*  along with this program.  If not, see <http://www.gnu.org/licenses/>.    *)
(*                                                                           *)
(*****************************************************************************)

(** The Radix tree. *)

module type KEY =
sig
  type t

  val equal: t -> t -> bool
  val get: t -> int -> char
  val length: t -> int
end

(** A concrete iterable structure. *)
type 'a sequence = ('a -> unit) -> unit

(** A Radix tree is a optimized container to bind a [Key.t] with a
    value. *)
module type S =
sig
  include Map.S

  val to_sequence: 'a t -> (key * 'a) sequence
  (** [to_sequence t] makes a abstract representation o the
      radix-tree. *)

  val pp: key Fmt.t -> 'a Fmt.t -> 'a t Fmt.t
  (** A pretty-printer for the radix-tree. *)
end

module Make (Key: KEY): Map.S with type key = Key.t
