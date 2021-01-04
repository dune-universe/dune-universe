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

type t

exception Open
exception Bytes_per_output_frame_invalid
exception Bytes_per_input_frame_invalid
exception Too_many_output_channels
exception Port_name_output_channel_mismatch
exception Port_not_found
exception Too_many_input_channels
exception Port_name_input_channel_mismatch

let _ =
  Callback.register_exception "bio2jack_exn_open" Open;
  Callback.register_exception "bio2jack_exn_bytes_per_output_frame_invalid"
    Bytes_per_output_frame_invalid;
  Callback.register_exception "bio2jack_exn_bytes_per_input_frame_invalid"
    Bytes_per_input_frame_invalid;
  Callback.register_exception "bio2jack_exn_too_many_output_channels"
    Too_many_output_channels;
  Callback.register_exception "bio2jack_exn_port_name_output_channel_mismatch"
    Port_name_output_channel_mismatch;
  Callback.register_exception "bio2jack_exn_port_not_found" Port_not_found;
  Callback.register_exception "bio2jack_exn_too_many_input_channels"
    Too_many_input_channels;
  Callback.register_exception "bio2jack_exn_port_name_input_channel_mismatch"
    Port_name_input_channel_mismatch

external private_value_int : string -> int = "caml_bjack_priv_value_int"

type converter =
  | Best_quality
  | Medium_quality
  | Fastest
  | Zero_order_hold
  | Linear

let int_of_converter c =
  let f = private_value_int in
  match c with
    | Best_quality -> f "SRC_SINC_BEST_QUALITY"
    | Medium_quality -> f "SRC_SINC_MEDIUM_QUALITY"
    | Fastest -> f "SRC_SINC_FASTEST"
    | Zero_order_hold -> f "SRC_ZERO_ORDER_HOLD"
    | Linear -> f "SRC_LINEAR"

external set_conversion_function : int -> unit
  = "caml_bjack_set_conversion_function"

let set_conversion_function c = set_conversion_function (int_of_converter c)

type port_flag = Input | Output | Physical | Monitor | Terminal

let int_of_port_flag flag =
  let f = private_value_int in
  match flag with
    | Input -> f "JackPortIsInput"
    | Output -> f "JackPortIsOutput"
    | Physical -> f "JackPortIsPhysical"
    | Monitor -> f "JackPortCanMonitor"
    | Terminal -> f "JackPortIsTerminal"

let iport_flags flags =
  List.fold_left (fun x y -> x lor int_of_port_flag y) 0 flags

external open_t :
  int -> int -> string -> string -> int -> int -> int -> int -> t
  = "caml_bjack_open_byte" "caml_bjack_open"

let open_t ?(ringbuffer_size = 4096) ?(server_name = "") ~rate ~bits_per_sample
    ~input_channels ~output_channels ~flags ~client_name () =
  let flags = iport_flags flags in
  open_t bits_per_sample rate client_name server_name input_channels
    output_channels flags ringbuffer_size

external close : t -> unit = "caml_bjack_close"
external write : t -> string -> int = "caml_bjack_write"
external read : t -> int -> string = "caml_bjack_read"
external reset : t -> unit = "caml_bjack_reset"

type position_unit = Bytes | Milliseconds
type position_type = Played | Written_to_jack | Written

let int_of_position_unit t =
  let f = private_value_int in
  match t with Bytes -> f "BYTES" | Milliseconds -> f "MILLISECONDS"

let int_of_position_type t =
  let f = private_value_int in
  match t with
    | Played -> f "PLAYED"
    | Written_to_jack -> f "WRITTEN_TO_JACK"
    | Written -> f "WRITTEN"

external get_position : t -> int -> int -> int = "caml_bjack_get_position"

let get_position ~position_unit ~position_type device =
  get_position device
    (int_of_position_unit position_unit)
    (int_of_position_type position_type)

external set_position : t -> int -> int -> unit = "caml_bjack_set_position"

let set_position ~position_unit device position =
  set_position device (int_of_position_unit position_unit) position

external get_output_latency : t -> int = "caml_bjack_get_output_latency"
external get_input_latency : t -> int = "caml_bjack_get_input_latency"

type playing_state = Playing | Paused | Stopped | Closed | Reset

let int_of_playing_state s =
  let f = private_value_int in
  match s with
    | Playing -> f "PLAYING"
    | Paused -> f "PAUSED"
    | Stopped -> f "STOPPED"
    | Closed -> f "CLOSED"
    | Reset -> f "RESET"

let playing_state_of_int n =
  let f = private_value_int in
  match n with
    | s when s = f "PLAYING" -> Playing
    | s when s = f "PAUSED" -> Paused
    | s when s = f "STOPPED" -> Stopped
    | s when s = f "CLOSED" -> Closed
    | s when s = f "RESET" -> Reset
    | _ -> raise Not_found

external set_state : t -> int -> unit = "caml_bjack_set_state"

let set_state device state = set_state device (int_of_playing_state state)

external get_state : t -> int = "caml_bjack_set_state"

let get_state device = playing_state_of_int (get_state device)

external get_max_output_buffered_bytes : t -> int
  = "caml_bjack_get_max_output_buffered_bytes"

external get_max_input_buffered_bytes : t -> int
  = "caml_bjack_get_max_input_buffered_bytes"

external get_jack_buffered_bytes : t -> int
  = "caml_bjack_get_jack_buffered_bytes"

type volume = Linear | Decibel

let int_of_volume v =
  let f = private_value_int in
  match (v : volume) with Linear -> f "linear" | Decibel -> f "dbAttenuation"

let volume_of_int n : volume =
  let f = private_value_int in
  match n with
    | e when e = f "linear" -> Linear
    | e when e = f "dbAttenuation" -> Decibel
    | _ -> raise Not_found

external set_volume_effect_type : t -> int -> int
  = "caml_bjack_set_volume_effect_type"

let set_volume_effect_type device volume =
  volume_of_int (set_volume_effect_type device (int_of_volume volume))

external set_all_volume : t -> int -> unit = "caml_bjack_set_all_volume"

external set_channel_volume : t -> int -> int -> unit
  = "caml_bjack_set_channel_volume"

let set_channel_volume ~channel ~device volume =
  set_channel_volume device channel volume

external get_channel_volume : t -> int -> int = "caml_bjack_get_channel_volume"

external get_output_bytes_per_second : t -> int
  = "caml_bjack_get_output_bytes_per_second"

external get_input_bytes_per_second : t -> int
  = "caml_bjack_get_input_bytes_per_second"

external get_bytes_stored : t -> int = "caml_bjack_get_bytes_stored"
external get_bytes_free_space : t -> int = "caml_bjack_get_bytes_free_space"
external get_bytes_used_space : t -> int = "caml_bjack_get_bytes_used_space"

external get_bytes_per_output_frame : t -> int
  = "caml_bjack_get_bytes_per_output_frame"

external get_bytes_per_input_frame : t -> int
  = "caml_bjack_get_bytes_per_input_frame"

external get_num_output_channels : t -> int
  = "caml_bjack_get_num_output_channels"

external get_num_input_channels : t -> int = "caml_bjack_get_num_input_channels"
external get_sample_rate : t -> int = "caml_bjack_get_sample_rate"
