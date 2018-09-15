(** Buffered sockets (Pervasive like functions for sockets working on
    all platforms).

    This library is distributed under the terms of the GNU Lesser
    General Public License, with the special exception on linking as
    for the OCaml Library.

    @author Christophe Troestler
    @version 1.2.2
*)
(* Since (in|out)_channel_of_descr does NOT work under win32, we need
   to implement a minimal buffered interface.

   For ease of use, the functions in this module have the same names
   and semantic as the ones in the Pervasive module.
*)

type in_channel
type out_channel
  (** Abstract types for buffered sockets. *)

val in_channel_of_descr : Unix.file_descr -> in_channel
  (** Create an input channel reading from the given descriptor. *)

val out_channel_of_descr : Unix.file_descr -> out_channel
  (** Create an output channel writing on the given descriptor. *)

val descr_of_in_channel : in_channel -> Unix.file_descr
  (** Return the descriptor corresponding to an input channel. *)

val descr_of_out_channel : out_channel -> Unix.file_descr
  (** Return the descriptor corresponding to an output channel. *)



(** {2 High-level network connection functions} *)

val open_connection : Unix.sockaddr -> in_channel * out_channel
  (** Connect to a server at the given address.
    Return a buffered socket.
    Can raise the same exceptions as [Unix.open_connection]. *)

val shutdown_connection : in_channel -> unit
  (** ``Shut down'' a connection established with
    {!Socket.open_connection}; that is, transmit an end-of-file
    condition to the server reading on the other side of the
    connection.  (You should flush the out_channels connected to the
    same socket if you want to make sure all data is transmitted.) *)


(** {2 Output functions} *)

val output : out_channel -> Bytes.t -> int -> int -> unit
  (** [output oc buf pos len] writes [len] characters from string
    [buf], starting at offset [pos], to the given buffered socket [oc].

    @raise Invalid_argument "Socket.output" if [pos] and [len] do not
    designate a valid substring of [buf].
    @raise Sys_blocked_io if we are in non-blocking mode and [output]
    would block. *)

val output_char : out_channel -> char -> unit
  (** Write the character on the given buffered socket. *)

val output_string : out_channel -> string -> unit
  (** Write the string on the given buffered socket. *)

val fprintf : out_channel -> ('a, unit, string, unit) format4 -> 'a
  (** [fprintf oc format arguments] is like [Printf.fprintf] except
    that [oc] is a buffered socket. *)

val flush : out_channel -> unit
  (** Flush the output buffer associated with the given buffered
    socket. *)

val close_out : out_channel -> unit
  (** [close_out oc] closes the socket [oc], flushing all buffered
    write operations.  (This closes the underlying file descriptor as
    well.)  Output functions raise a [Sys_error] exception when they
    are applied to a closed output channel, except [close_out] and
    [flush] which do nothing.  *)


(** {2 Input functions} *)

val input : in_channel -> Bytes.t -> int -> int -> int
  (** [input ic buf pos len] reads up to [len] characters from the
    given socket [ic], storing them in string [buf], starting at
    character number [pos].  It returns the actual number of
    characters read, between 0 and [len] (inclusive).

    A return value of 0 means that the end of file was reached.

    @raise Invalid_argument "Socket.input" if [pos] and [len] do not
    designate a valid substring of [buf].
    @raise Sys_blocked_io if we are in non-blocking mode and [input]
    would block (no characters are then read).   *)

val input_char : in_channel -> char
  (** Read one character from the given input channel.
    @raise End_of_file if there are no more characters to read.
    @raise Sys_blocked_io if we are in non-blocking mode and [input_char]
    would block (no characters are then read).  *)

val really_input : in_channel -> Bytes.t -> int -> int -> unit
  (** [really_input ic buf pos len] reads [len] characters from the
    buffered socket [ic], storing them in string [buf], starting at
    character number [pos].

    @raise End_of_file if the end of file is reached before [len]
    characters have been read.
    @raise Invalid_argument "Socket.really_read" if [pos] and [len]
    do not designate a valid substring of [buf].
    @raise Sys_blocked_io if we are in non-blocking mode and [really_input]
    would block (the characters that may have been read are lost). *)

val input_line : in_channel -> string
  (** [input_line ic] returns the next line from the socket [ic]
    without the final '\n'.

    @raise End_of_file if the end of the file is reached.
    @raise Sys_blocked_io if we are in non-blocking mode and
    [input_line] would block (the characters that may have been read
    are lost). *)

val input_till : char -> in_channel -> Bytes.t -> int -> int -> int
  (** [input_till c ic buf pos len] reads up to [len] characters
    different from [c] from the socket [ic], storing them in string
    [buf], starting at character number [pos].  The return value is
    the actual number of characters read (different from [c]), between
    0 and [len] (inclusive).  If [c] is encountered, the reading
    stops.  [c] is left in the input stream; thus all further
    [input_till] commands will return [0].

    @raise End_of_file if the end of the file is reached (i.e.,
    there are no more characters to read).  This is different from the
    return value being [0] -- the latter indicating that the first
    character in the stream is [c].

    @raise Invalid_argument "Socket.input_till" if [pos] and [len]
    do not designate a valid substring of [buf].
    @raise Sys_blocked_io if we are in non-blocking mode and [input_till]
    would block (no characters are then read).  *)

val input_all_till : char -> in_channel -> string
  (** [input_all_till c ic] returns the next chunk from the socket
    [ic] from the current position to the character [c] (excluded) or
    the end of the file.  The character [c] is read and discarded.

    @raise End_of_file if the end of the file is reached before any
    character can be read.  (This is different from an empty string
    being returned which indicates that [c] is the first character in
    the input stream.)
    @raise Sys_blocked_io if we are in non-blocking mode and [input_all_till]
    would block (the characters that may have been read are lost). *)

val close_in : in_channel -> unit
  (** [close_in ic] closes the socket [ic].  (This closes the
    underlying file descriptor as well.)  Input functions raise a
    [Sys_error] exception when they are applied to a closed input
    channel, except [close_in], which does nothing when applied to an
    already closed channel.  *)


(** {2 Polling} *)

val select : in_channel list -> out_channel list -> float
  -> in_channel list * out_channel list
  (** [select inl outl t] waits at most [t] seconds until some
      input/output operations become possible on some channels among
      [inl] and [outl].  A negative [t] means unbounded wait.  The
      result is composed of two sets of channels: those ready for
      reading (first component) and those ready for writing (second
      component).  *)


(** {2 I/O objects} *)

(** I/O objects for the channels respecting the conventions for
  {{:http://ocaml-programming.de/rec/IO-Classes.html}IO-Classes}.  The
  objects only give another access to the channels and their methods
  can be interspersed with the above function calls. *)

class out_channel_obj : out_channel ->
object
  method output : Bytes.t -> int -> int -> int
    (** [#output buf pos len] writes up to [len] characters from
      string [buf], starting at offset [pos], to the given object
      output stream and returns the number of characters actually
      written.  A return value [0] (when [len > 0]) means that the
      write would block (and we are in non-blocking mode).

      @raise Invalid_argument "Socket.output" if [pos] and [len]
      do not designate a valid substring of [buf]. *)

    method output_char : char -> unit
      (** See {!Socket.output_char}. *)

    method output_string : string -> unit
      (** See {!Socket.output_string}. *)

    method fprintf : ('a, unit, string, unit) format4 -> 'a
      (** See {!Socket.fprintf}. *)

    method flush : unit -> unit
      (** Flush the buffer associated with the output channel,
        performing all pending writes on that channel.  Interactive
        programs must be careful about flushing standard output and
        standard error at the right time. *)

    method close_out : unit -> unit
      (** Flush all buffered write operations and close the given
          channel (and underlying socket).  See {!Socket.close_out}.*)
end

class in_channel_obj : in_channel ->
object
  method input : Bytes.t -> int -> int -> int
    (** [#input buf pos len] reads up to [len] characters from the
        object, storing them in string [buf], starting at character
        number [pos].  It returns the actual number of characters read,
        between 0 and [len] (inclusive).  A return value [0] (when
        [len > 0]) means that the write would block (and we are in
        non-blocking mode).

        @raise End_of_file when the end of the stream is reached, and
        there are no more octets that could be read.
        @raise Invalid_argument "Socket.input" if [pos] and [len] do
        not designate a valid substring of [buf].  *)

    method input_char : unit -> char
      (** See {!Socket.input_char}. *)

    method input_line : unit -> string
      (** See {!Socket.input_line}. *)

    method really_input : Bytes.t -> int -> int -> unit
      (** See {!Socket.really_input}. *)

    method input_till : char -> Bytes.t -> int -> int -> int
      (** See {!Socket.input_till}. *)

    method input_all_till : char -> string
      (** See {!Socket.input_all_till}. *)

    method close_in : unit -> unit
      (** Close the given channel (and underlying socket).  See
          {!Socket.close_in}. *)
  end

