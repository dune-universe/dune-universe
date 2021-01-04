(*
 * Copyright 2007-2008 Romain Beauxis
 *
 * This file is part of ocaml-bjack.
 *
 * ocaml-bjack is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * ocaml-bjack is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with ocaml-bjack; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * As a special exception to the GNU Library General Public License, you may
 * link, statically or dynamically, a "work that uses the Library" with a publicly
 * distributed version of the Library to produce an executable file containing
 * portions of the Library, and distribute that executable file under terms of
 * your choice, without any of the additional requirements listed in clause 6
 * of the GNU Library General Public License.
 * By "a publicly distributed version of the Library", we mean either the unmodified
 * Library as distributed by The Savonet Team, or a modified version of the Library that is
 * distributed under the conditions defined in clause 3 of the GNU Library General
 * Public License. This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU Library General Public License.
 *
 *)

(* @author Romain Beauxis *)

(** Ocaml blocking API to jack audio connection kit *)

(** {2 Types and exceptions } *)

(** Type for a Bjack device *)
type t

(** Various possible samplerate converters *)
type converter =
  | Best_quality
  | Medium_quality
  | Fastest
  | Zero_order_hold
  | Linear

exception Open
exception Bytes_per_output_frame_invalid
exception Bytes_per_input_frame_invalid
exception Too_many_output_channels
exception Port_name_output_channel_mismatch
exception Port_not_found
exception Too_many_input_channels
exception Port_name_input_channel_mismatch

(** Set conversion function *)
val set_conversion_function : converter -> unit

(** Various jack port options *)
type port_flag = Input | Output | Physical | Monitor | Terminal

(** {2 Blocking API } *)

(** Open a Bjack device *)
val open_t :
  ?ringbuffer_size:int ->
  ?server_name:string ->
  rate:int ->
  bits_per_sample:int ->
  input_channels:int ->
  output_channels:int ->
  flags:port_flag list ->
  client_name:string ->
  unit ->
  t

(** Close a Bjack device *)
val close : t -> unit

(** Write to a device.
  * Raises [Too_many_output_channels] if there are no output
    channels available on the device *)
val write : t -> string -> int

(** Read from a device.
  * Raises [Too_many_input_channels] if there are no input 
    channels available on the device *)
val read : t -> int -> string

(** Reset a Bjack device *)
val reset : t -> unit

(** {2 Parameters and informations } *)

type position_unit = Bytes | Milliseconds
type position_type = Played | Written_to_jack | Written

val get_position :
  position_unit:position_unit -> position_type:position_type -> t -> int

val set_position : position_unit:position_unit -> t -> int -> unit
val get_output_latency : t -> int
val get_input_latency : t -> int

type playing_state = Playing | Paused | Stopped | Closed | Reset

val set_state : t -> playing_state -> unit
val get_state : t -> playing_state
val get_max_output_buffered_bytes : t -> int
val get_max_input_buffered_bytes : t -> int
val get_jack_buffered_bytes : t -> int

type volume = Linear | Decibel

val set_volume_effect_type : t -> volume -> volume
val set_all_volume : t -> int -> unit
val set_channel_volume : channel:int -> device:t -> int -> unit
val get_channel_volume : t -> int -> int
val get_output_bytes_per_second : t -> int
val get_input_bytes_per_second : t -> int
val get_bytes_stored : t -> int
val get_bytes_free_space : t -> int
val get_bytes_used_space : t -> int
val get_bytes_per_output_frame : t -> int
val get_bytes_per_input_frame : t -> int
val get_num_output_channels : t -> int
val get_num_input_channels : t -> int
val get_sample_rate : t -> int
