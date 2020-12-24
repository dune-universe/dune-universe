(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

module Make(Primary: Mirage_block_lwt.S)(Secondary: Mirage_block_lwt.S): sig
  include Mirage_block_lwt.S

  val connect:
    ?progress_cb:([ `Percent of int | `Complete ]-> unit)
    -> Primary.t -> Secondary.t -> t Lwt.t
  (** [connect ?progress primary secondary] creates a block device which performs I/O
      against [primary], while building a mirror of [primary] on top of
      [secondary] in the background. Existing data in [secondary] will be
      destroyed.

      If [?progress_cb] is provided then it will be called on every percentage
      change in mirror progress.

      It is an error if the block size of either [primary] or [secondary]
      is not an integer multiple of the other.

      It is an error if [primary] and [secondary] have different lengths.

      It is an error if [secondary] is read-only.
  *)

  val string_of_error: error -> string

end
