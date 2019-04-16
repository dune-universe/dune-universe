(*
 * Copyright (c) 2015, Christopher Zimmermann
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
 * SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)


(**
   Bindings to the zlib compression library providing deflate compression with
   or without zlib or gzip headers.

   This library uses bigarrays as buffers and can therefore release the OCaml
   runtime during (de)compression, allowing other OCaml threads to continue.

   @see <https://github.com/madroach/ocaml-zlib> latest version on github
   @see <http://zlib.net/manual.html> Zlib manual
   @see <http://zlib.net/> Zlib homepage

   @author Christopher Zimmermann
*)


type status =
  | Ok                   (** 0 *)
  | Stream_end           (** 1 *)
  | Need_dict            (** 2 *)
  | Buf_error            (** 3 (zlib -5) *)
  | Data_error of string (** 0 (zlib -3) *)
(** Zlib return codes. Only non-fatal return codes are returned. Fatal error
   codes are translated to the standard exceptions [Failure _],
   [Invalid_argument _] or [Out_of_memory]. *)

type algo = Deflated
(** Compression algorithm. Only deflate is provided by current zlib. *)

type strategy =
  | Default_strategy  (** 0 *)
  | Filtered          (** 1 *)
  | Huffman_only      (** 2 *)
  | RLE               (** 3 *)
  | Fixed             (** 4 *)
(** Compression strategy - see zlib manual for details. *)

type flush =
  | No_flush          (** 0 *)
  | Partial_flush     (** 1 *)
  | Sync_flush        (** 2 *)
  | Full_flush        (** 3 *)
  | Finish            (** 4 *)
  | Block             (** 5 *)
  | Trees             (** 6 *)
(** The type of the flush parameter passed to flate.
   Use [Finish] when all input has been provided, otherwise use [No_flush].
   For the other flush values see the zlib manual. *)

type data_type =
  | Binary            (** 0 *)
  | Text              (** 1 *)
  | Unknown           (** 2 *)
(** Best guess of flate about the type of data being compressed. *)

type deflate
type inflate
(** Pseudo types to specify whether a zlib stream state is used to inflate or
   deflate. *)

type 'a state
(** Internal state *)

type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
type 'a t =
  { state :'a state
  ; mutable in_buf     :bigstring
    (** bigstring input buffer *)
  ; mutable out_buf    :bigstring
    (** bigstring output buffer *)
  ; mutable in_ofs    :int
    (** offset into the input buffer *)
  ; mutable out_ofs   :int
    (** offset into the output buffer *)
  ; mutable in_len    :int
    (** Length of available input data *)
  ; mutable out_len   :int
    (** Length of available output data *)
  ; mutable in_total  :int
    (** Length of input data processed so far *)
  ; mutable out_total :int
    (** Length of output data processed so far *)
  ; mutable data_type :int
    (** For deflate streams a guess about the type of data is returned here: [0]
        for binary data, [1] for text and [2] for unknown.

        For inflate streams the number of unused bits in the last byte taken
        from the input stream is stored here.
        If {!flate} just finished decoding the header or returned after an
        end-of-block code [128] is added.
        If {!flate} is currentry decoding the last block [64] is added. *)
  ; mutable cksum     :int32
    (** The checksum of the decompressed data produced resp. consumed so far.
        When {!flate} returns {!status.Need_dict} the adler32 checksum of the required
        dictionary is returned here instead. *)
  }
(** Record holding the internal state and input / output buffers of data as
    well as other data returned from the zlib inflate and deflate routines. *)

type header =
  { text        :bool
    (** Compressed data believed to be text? *)
  ; mtime       :int32
    (** mtime of compressed file. Set to zero if unknown. *)
  ; os          :int
    (** filesystem type on which the compressed file was stored.
        See {{: https://www.ietf.org/rfc/rfc1952.txt } RFC1952 } for possible values. *)
  ; xflags      :int
    (** Extra flags according to {{: https://www.ietf.org/rfc/rfc1952.txt } RFC1952 }. For deflate compression method the
        compression level is stored here. *)
  ; extra       :string option
    (** Extra header field according to {{: https://www.ietf.org/rfc/rfc1952.txt } RFC1952 }. *)
  ; name        :string option
    (** Original file name of the compressed file translated to ISO 8859-1
        (LATIN-1). *)
  ; comment     :string option
    (** File comment. According to {{: https://www.ietf.org/rfc/rfc1952.txt } RFC1952 } only ISO 8859-1 (LATIN-1) characters
        are allowed. Linebreak is a single linefeed. *)
  }
(** Record holding the data in a gzip header. *)

val create_inflate : ?window_bits:int -> unit -> inflate t
(** [create_inflate ()]
                        Creates zlib internal state and buffer description for
                        decompression.
    @param window_bits  the base 2 logarithm of the maximum window size. It
                        should be in tha range 8..15 and greater or equal to
                        the [window_bits] parameter used for compression.
                        By default the zlib format is decoded.
                        Use a negative value -8..-15 for raw deflate
                        decompression without zlib or gzip header.
                        Add [16] to [window_bits] to decode the gzip format.
                        Add [32] to [window_bits] to decode zlib or gzip format
                        with automatic header detection. *)

val create_deflate :
  ?level:int ->
  ?algo:algo ->
  ?window_bits:int ->
  ?memory:int ->
  ?strategy:strategy ->
  unit -> deflate t
(** [create_deflate ()] Creates zlib internal {!state} and buffer description
                        for compression.
    @param level        the compression level must be betweed [-1] and [9].
                        [-1] selects default compression level, [1] gives best
                        speed [9] gives best compression, anything in between
                        is a compromise of speed/compression. [0] gives no
                        compression at all.
    @param algo         Only {!algo.Deflated} available at the moment.
    @param window_bits  the base 2 logarithm of the maximum window size. It
                        should be in tha range 8..15 and greater or equal to
                        the [window_bits] parameter used for compression.
                        Use a negative value -8..-15 for raw deflate
                        compression without zlib or gzip header.
                        Add 16 to [window_bits] to write a gzip header.
    @param memory       selects how much memory to use for compression in the
                        range 1..9. More memory means faster and better
                        compression.
                        A value of [9] uses 256kb, [8] 128kb, [7] 64kb...
    @param strategy     See the zlib manual for details about this parameter. *)

external inflate_init : window_bits:int -> inflate state = "zlib_inflate_init"
(** [inflate_init window_bits] like {!create_inflate}, but only creates the
    internal {!state}. *)

external deflate_init :
  level:int -> algo:algo -> window_bits:int -> memory:int -> strategy:strategy
  -> deflate state
  = "zlib_deflate_init"
(** [deflate_init level algo window_bits memory strategy] like {!create_deflate}, but
    only creates the internal {!state}. *)

external deflate_bound : deflate state -> int -> int = "zlib_deflate_bound"
(** [deflate_bound state len] calculates an upper bound on the size of the
                        compressed data. This functions assumes the zlib format
                        is used. The resulting compressed size might be larger
                        than the returned bound when the gzip format is being
                        used.
    @return             An upper bound on the compressed data when using the
                        zlib format.
    @param state        {!deflate} {!state} to be used in compression.
    @param len          size of the data to be compressed. *)

external flate : 'a t -> flush -> status = "zlib_flate"
(** [flate buffers flush] (de)compresses data from the provided input to the
                        output buffers.
    @return             {!status.Ok} if some progress has been made but more input or
                        output is expected. {!status.Stream_end} if all data has been
                        processed. {!status.Need_dict} if a dictionary is needed when
                        inflating zlib data. {!status.Data_error} if the provided data
                        is inconsistent.
    @param buffers      this record is documented above. It's fields are
                        updated by this function.
    @param flush        Whether and how the output should be flushed.
                        See the type definition above and the zlib manual for
                        details.
*)

external inflate_set_dictionary : inflate state -> string -> status
  = "zlib_inflate_set_dictionary"
(** [inflate_set_dictionary state dict] Sets a preset dictionary for
                        decompression. Must be called after {!flate} requested a
                        dictionary by returning {!status.Need_dict}; the cksum field of
                        {!state} will then contain the adler32 checksum of the
                        required dictionary.
    @return             {!status.Data_error} when the adler32 checksum of the provided
                        dictionary doesn't match the requested checksum.
    @param state        A {!inflate} {!state} used for compression.
*)

external deflate_set_dictionary : deflate state -> string -> int32
  = "zlib_deflate_set_dictionary"
(** [deflate_set_dictionary state dict] Sets a preset dictionary for
                        compression. When using the zlib format this needs to
                        be called before the first call to {!flate}.
                        No dictionary may be used for the gzip format.
    @return             the adler32 checksum of the provided dictionary.
    @param state        A {!deflate} {!state} used for compression.
*)

external get_header : inflate state -> header         = "zlib_get_header"
(** [get_header header] Retrieve a header after a gzip header has been read by
                        {!flate}.
    @return             the header read by {!flate}.
    @raise              Failure ["Zlib.get_header: Header not yet completed."]
                        if the header has not yet been completely read.
    @raise              Not_found if not reading gzip format.
*)

external set_header : deflate state -> header -> unit = "zlib_set_header"
(** [set_header state header] Provide a header when writing the gzip format. Must be
                        called before any call to {!flate}.
    @param header       fields to be stored th the header. The {!header.xflags} field
                        is not used when compressing.
*)

external reset : 'a t -> unit = "zlib_reset"
(** [reset buffers]     Prepares for a new stream of data to be (de)compressed.
                        The parameters passed to {!inflate_init} resp.
                        {!deflate_init} are left unchanged.
    @param buffers      resets all fields to sane initial values and resets the
                        internal {!state}.
*)

val get_data_type : deflate t -> data_type
(** [get_data_type buffers] gets the data type of the data being compressed.
    @return             Best guess on the data being compressed.
    @param buffers      The buffers of the running compression.
*)

external adler32 : int32 -> string -> int32 = "zlib_adler32"
(** [adler32 cksum buf] Updates the running adler32 checksum
    @return             updated checksum.
    @param cksum        The running adler32 checksum to be updated.
    @param buf          The data with which to update the [cksum].
*)

val adler32_empty : int32
(** Initial value to be used with {!adler32} *)
