(*
 * Copyright 2003-2010 Savonet team
 *
 * This file is part of Ocaml-flac.
 *
 * Ocaml-flac is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Ocaml-flac is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Ocaml-flac; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(* Author; Romain Beauxis <toots@rastageeks.org> *)

(** {1 Ogg/flac encoder/decoder modules for OCaml} *)

(** Decode ogg/flac data *)
module Decoder : sig
  (** {3 Usage} *)

  (** Usage is similar to the case
    * of the native FLAC decoder, using
    * the appropriate ogg/flac decoding
    * callbacks. 
    *
    * The main difference is that in the 
    * case of the ogg/flac decoding, the
    * exception [Ogg.Not_enough_data] may 
    * be raised if the ogg stream used to
    * create the decoder does not contain 
    * enough data. In this case, you should 
    * feed more data into the ogg stream and 
    * call the decoding function again.
    * 
    * This remark is valid for both the
    * [Flac.Decoder.init] and [Flac.Decoder.process] 
    * functions. *)

  (** {3 Types} *)

  (** Variant type for ogg/flac decoder *)
  type ogg

  (** Check if an ogg packet is the first
    * packet of an ogg/flac stream. *)
  val check_packet : Ogg.Stream.packet -> bool

  (** Create a set of callbacks to decode an ogg/flac stream *)
  val get_callbacks : Flac.Decoder.write -> ogg Flac.Decoder.callbacks

  (** Create an ogg/flac decoder *)
  val create : Ogg.Stream.packet -> Ogg.Stream.stream -> ogg Flac.Decoder.dec

  (** Update the [Ogg.Stream.stream] associated
    * to the decoder. *)
  val update_ogg_stream : ogg Flac.Decoder.t -> Ogg.Stream.stream -> unit
end

(** Encode ogg/flac data *)
module Encoder : sig
  (** {3 Usage} *)

  (** Usage is similar to the case
    * of the native FLAC encoder, using
    * the appropriate ogg/flac encoding
    * callbacks. *)

  (** {3 Types} *)

  (** Variant type for ogg/flac encoder *)
  type ogg

  (** Create a set of ogg/flac callbacks to 
    * encoder an ogg/flac stream *)
  val callbacks : ogg Flac.Encoder.callbacks

  (** Create an ogg/flac encoder.
    * 
    * The returned value contains an encoder value
    * that can be used with the functions from the 
    * [Flac.Encoder] module, as well as an initial 
    * ogg packet, that should be placed in its own
    * page at the beginning of the ogg stream, and
    * then the remaining initial packets, containing
    * comments data, that should be placed in some ogg 
    * pages before and not containing any audio data. 
    * See ogg stream documentation for more information 
    * on ogg data muxing. *)
  val create :
    ?comments:(string * string) list ->
    Flac.Encoder.params ->
    Ogg.Stream.stream ->
    ogg Flac.Encoder.t * Ogg.Stream.packet * Ogg.Stream.packet list

  (** Terminate an ogg/flac encoder. Causes the encoder
    * to flush remaining encoded data. The encoder should not
    * be used anymore afterwards. *)
  val finish : ogg Flac.Encoder.t -> unit
end

(** Ogg/flac skeleton module *)
module Skeleton : sig
  (** Generate a flac fisbone packet with
    * these parameters, to use in an ogg skeleton.
    * Default value for [start_granule] is [Int64.zero],
    * Default value for [headers] is ["Content-type","audio/x-flac"]
    *
    * See: http://xiph.org/ogg/doc/skeleton.html. *)
  val fisbone :
    ?start_granule:Int64.t ->
    ?headers:(string * string) list ->
    serialno:Nativeint.t ->
    samplerate:Int64.t ->
    unit ->
    Ogg.Stream.packet
end
