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

(** {1 Native FLAC decoder/encoder modules for OCaml} *)

(** Decode native FLAC data *)
module Decoder : sig
  (** {3 Usage} *)

  (** A typical use of the FLAC decoder is the following:
    *
    * {v  (* Raise this when streams has ended. *)
    * exception End_of_stream
    * (* Define a read function *)
    * let input = (..a function of type read..) in
    * (* Define a write function *)
    * let output = (..a function of type write..) in
    * (* Create callbacks *)
    * let callbacks = Flac.Decoder.get_callbacks input write in
    * (* Create an unitialized decoder *)
    * let decoder = Flac.Decoder.create callbacks in
    * (* Initialize decoder *)
    * let decoder,info,comments = Flac.Decoder.init decoder callbacks in
    * (..do something with info and comments..)
    * (* Decode data *)
    * match Flac.Decoder.state decoder c with
    *   | `Search_for_metadata
    *   | `Read_metadata
    *   | `Search_for_frame_sync
    *   | `Read_frame ->
    *         Flac.Decoder.process decoder callbacks
    *   | _ -> raise End_of_stream v}
    *
    * Some remarks: 
    * - Exceptions raised by callbacks should be treated
    *   as fatal errors. The dehaviour of the flac library 
    *   after being interrupted by an exception is unknown.
    *   The only notable exception is Ogg/flac decoding, where
    *   the read callback raises [Ogg.Not_enough_data].
    * - The state of the decoder should be checked prior to calling
    *   [process]. Termination may not be detected nor raise an
    *   exception so it is the caller's responsibility to check
    *   on this. 
    * - See FLAC documentation for the information on the 
    *   callbacks. 
    * - The variant type for decoder and callbacks is used
    *   to make sure that different type of decoders 
    *   (generic, file, ogg) are only used with the same
    *   type of callbacks. *)

  (** {3 Types } *)

  (** Type of an uninitialized decoder. *)
  type 'a dec

  (** Type of an initialized decoder. *)
  type 'a t

  (** Type of a write callback. *)
  type write = float array array -> unit

  (** Type of  a read callback. *)
  type read = bytes -> int -> int -> int

  (** Type of a collection of callbacks. *)
  type 'a callbacks

  (** Generic variant type for callbacks and decoder. *)
  type generic

  (** Info about decoded FLAC data. *)
  type info = {
    sample_rate : int;
    channels : int;
    bits_per_sample : int;
    total_samples : int64;
    md5sum : string;
  }

  (** (Vorbis) comments of decoded FLAC data. *)
  type comments = string * (string * string) list

  (** Possible states of a decoder. *)
  type state =
    [ (* The decoder is ready to search for metadata. *)
      `Search_for_metadata
      (* The decoder is ready to or is in the process of reading metadata. *)
    | `Read_metadata
      (* The decoder is ready to or is in the process of searching for the
         frame sync code. *)
    | `Search_for_frame_sync
      (* The decoder is ready to or is in the process of reading a frame. *)
    | `Read_frame (* The decoder has reached the end of the stream. *)
    | `End_of_stream (* An error occurred in the underlying Ogg layer. *)
    | `Ogg_error
      (* An error occurred while seeking.  The decoder must be flushed
         or reset before decoding can continue. *)
    | `Seek_error (* The decoder was aborted by the read callback. *)
    | `Aborted
      (* An error occurred allocating memory.  The decoder is in an invalid
         state and can no longer be used. *)
    | `Memory_allocation_error
      (* This state is seen in the case of
         an uninitialized ogg decoder. *)
    | `Uninitialized ]

  (** {3 Exceptions } *)

  (** An error in the stream caused the decoder to lose synchronization. *)
  exception Lost_sync

  (** The decoder encountered a corrupted frame header. *)
  exception Bad_header

  (** The frame's data did not match the CRC in the footer. *)
  exception Frame_crc_mismatch

  (** The decoder encountered reserved fields in use in the stream. *)
  exception Unparseable_stream

  (** Raised if trying to decode a stream that
    * is not flac. *)
  exception Not_flac

  (** {3 Functions} *)

  (** Create a set of callbacks. *)
  val get_callbacks :
    ?seek:(int64 -> unit) ->
    ?tell:(unit -> int64) ->
    ?length:(unit -> int64) ->
    ?eof:(unit -> bool) ->
    read ->
    write ->
    generic callbacks

  (** Create an uninitialized decoder. *)
  val create : 'a callbacks -> 'a dec

  (** Initialize a decoder. The decoder will be used to decode
    * all metadata. Initial audio data shall be immediatly available
    * after this call. *)
  val init : 'a dec -> 'a callbacks -> 'a t * info * comments option

  (** Decode one frame of audio data. *)
  val process : 'a t -> 'a callbacks -> unit

  (** Flush the input and seek to an absolute sample.
    * Decoding will resume at the given sample. Note 
    * that because of this, the next write callback may 
    * contain a partial block.  The client must support seeking 
    * the input or this function will fail and return [false].  
    * Furthermore, if the decoder state is [`Seek_error]
    * then the decoder must be flushed or reset
    * before decoding can continue. *)
  val seek : 'a t -> 'a callbacks -> Int64.t -> bool

  (** Flush the stream input.
    *  The decoder's input buffer will be cleared and the state set to
    *  [`Search_for_frame_sync].  This will also turn
    *  off MD5 checking. *)
  val flush : 'a t -> 'a callbacks -> bool

  (** Reset the decoding process.
    *  The decoder's input buffer will be cleared and the state set to
    *  [`Search_for_metadata]. MD5 checking will be restored to its original
    *  setting.
    *
    *  If the decoder is seekable, the decoder will also attempt to seek to 
    *  the beginning of the stream. If this rewind fails, this function will 
    * return [false].  It follows that [reset] cannot be used when decoding 
    * from [stdin].
    *
    *  If the decoder is not seekable (i.e. no seek callback was provided) 
    *  it is the duty of the client to start feeding data from the beginning 
    *  of the stream on the next [process]. *)
  val reset : 'a t -> 'a callbacks -> bool

  (** Get the state of a decoder. *)
  val state : 'a t -> 'a callbacks -> state

  (** {3 Convenience} *)

  (** Convert an audio array to a S16LE string for 
    * decoding FLAC to WAV and raw PCM *)
  val to_s16le : float array array -> string

  (** Local file decoding. *)
  module File : sig
    (** Convenience module to
      * decode local files *)

    (** {3 Types} *)

    (** File variant type for a file decoder *)
    type file

    (* Handler for file decoder *)
    type handle = {
      fd : Unix.file_descr;
      dec : file t;
      (* These callback support [seek] and [tell]
       * if the underlying [Unix.file_descriptor]
       * supports them. *)
      callbacks : file callbacks;
      info : info;
      comments : (string * (string * string) list) option;
    }

    (** {3 Functions} *)

    (** Create a file decoder from a Unix file
      * descriptor 
      *
      * Note: this decoder requires seeking thus will only work on seekable
      * file descriptor. *)
    val create_from_fd : write -> Unix.file_descr -> handle

    (** Create a file decoder from a file URI *)
    val create : write -> string -> handle
  end
end

(** Encode native FLAC data *)
module Encoder : sig
  (** {3 Usage} *)

  (** A typical use of the FLAC encoder is the following:
    * {v  (* A function to write encoded data *)
    * let write = (..a function of type write..) in
    * (* Create the encoding callbacks *)
    * let callbacks = Flac.Encoder.get_callbacks write in
    * (* Define the parameters and comments *)
    * let params = (..a value of type params ..) in
    * let comments = [("title","FLAC encoding example")] in
    * (* Create an encoder *)
    * let enc = Flac.Encoder.create ~comments params callbacks in
    * (* Encode data *)
    * let data = (..a value of type float array array.. in 
    *  Flac.Encoder.process enc callbacks data ;
    * (..repeat encoding process..)
    * (* Close encoder *)
    * Flac.Encoder.finish enc callbacks v}
    * 
    * Remarks: 
    * - Exceptions raised by the callbacks should be treated
    *   as fatal. The behaviour of the FLAC encoding library is
    *   unknown after interrupted by an exception.
    * - Encoded data should have the same number of channels as
    *   specified in encoder's parameters and the same number of
    *   samples in each channels. 
    * - See FLAC documentation for informations about the callbacks.
    *   Note in particular that some information about encoded data
    *   such as md5 sum and total samples are only written when a 
    *   [seek] callback is given. 
    * - Variant types for callbacks and encoder are used to make sure
    *   that different type of callbacks (generic, file, ogg) are always
    *   used with the corresponding decoder type. *)

  (** {3 Types} *)

  (** Type of an encoder. *)
  type 'a t

  (** Type of a write callback *)
  type write = bytes -> unit

  (** Type of a set of callbacks *)
  type 'a callbacks

  (** Generic type for an encoder *)
  type generic

  (** Type of encoding parameters *)
  type params = {
    channels : int;
    bits_per_sample : int;
    sample_rate : int;
    compression_level : int option;
    total_samples : int64 option;
  }

  (** (Vorbis) comments for encoding *)
  type comments = (string * string) list

  (** {3 Exceptions} *)

  (** Raised when submiting invalid data to
    * encode *)
  exception Invalid_data

  (** {3 Functions} *)

  (** Create a set of encoding callbacks *)
  val get_callbacks :
    ?seek:(int64 -> unit) -> ?tell:(unit -> int64) -> write -> generic callbacks

  (** Create an encoder *)
  val create : ?comments:comments -> params -> 'a callbacks -> 'a t

  (** Encode some data *)
  val process : 'a t -> 'a callbacks -> float array array -> unit

  (** Terminate an encoder. Causes the encoder to
    * flush remaining encoded data. The encoder should
    * not be used anymore afterwards. *)
  val finish : 'a t -> 'a callbacks -> unit

  (** {3 Convenience} *)

  (** Convert S16LE pcm data to an audio array for 
    * encoding WAV and raw PCM to flac. *)
  val from_s16le : string -> int -> float array array

  (** Encode to a local file *)
  module File : sig
    (** Convenience module to encode to a local native FLAC file. *)

    (** {3 Types} *)

    (** Generic variant type for file encoder *)
    type file

    (** Handle for file encoder *)
    type handle = {
      fd : Unix.file_descr;
      enc : file t;
      callbacks : file callbacks;
    }

    (** {3 Functions} *)

    (** Create a file encoder writing data to a given Unix file descriptor.
      * 
      * Note: this encoder requires seeking thus will only work on seekable
      * file descriptor. *)
    val create_from_fd :
      ?comments:comments -> params -> Unix.file_descr -> handle

    (** Create a file encoder writing data to the given file URI *)
    val create : ?comments:comments -> params -> string -> handle
  end
end

(** Raised when an internal error occured. Should be
  * reported if seen. *)
exception Internal
