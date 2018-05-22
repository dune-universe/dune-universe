(* This library implements the subset of the basic encoding rules
  necessary to implement the ldap protocol. See ITU-T X.680 and X.690
  for a description of ASN.1, and the basic encoding rules.

  Copyright (C) 2004 Eric Stokes, Matthew Backes, and The California
  State University at Northridge

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
  USA
*)

(** This library implements the subset of ber *)

exception Decoding_error of string
exception Encoding_error of string

type readbyte_error = End_of_stream
                      | Transport_error
                      | Peek_error
                      | Request_too_large
                      | Not_implemented
exception Readbyte_error of readbyte_error

type readbyte = ?peek:bool -> int -> string
type writebyte = char -> unit
type ber_class = Universal | Application | Context_specific | Private
type ber_length = Definite of int | Indefinite

type ber_val_header = {
  ber_class : ber_class;
  ber_primitive : bool;
  ber_tag : int;
  ber_length : ber_length;
}

(** return a readbyte function for a string, currently not implemented *)
val readbyte_of_string : string -> readbyte

(** return a readbyte implementation which uses another readbyte, but
    allows setting a read boundry. Useful for constructing views of the
    octet stream which end at the end of a ber structure. This is
    essential for reading certian structures because length is only
    encoded in the toplevel in order to save space. Currently only
    implemented for definite lengths.

    @raise Readbyte_error in the event of a an io error, or the end of file *)
val readbyte_of_ber_element : ber_length -> readbyte -> readbyte

(** a readbyte implementation which reads from an FD. It implements a
    peek buffer, so it can garentee that it will work with
    rb_of_ber_element, even with blocking fds.

    @raise Readbyte_error in the event of a an io error, or the end of file *)
val readbyte_of_fd: Unix.file_descr -> readbyte

(** a readbyte implementation which reads from an SSL socket. It is
    otherwise the same as readbyte_of_fd.

    @raise Readbyte_error in the event of a an io error, or the end of file *)
val readbyte_of_ssl: Ssl.socket -> readbyte

(** decoding and encoding of the ber header *)
val decode_ber_header : ?peek:bool -> readbyte -> ber_val_header
val encode_ber_header : ber_val_header -> string

(** reads the contents octets *)
val read_contents : ?peek:bool -> readbyte -> ber_length -> string

(**
  ENCODING and DECODING Functions

  Explanation of optional arguments:
  The optional arguments are there to deal with a number of
  situations, cls, and tag are for context specific or application
  situations where it is expected that the value will not be marked
  with the class and tag defined in X.680. Contents is there for
  akward situations which arise because of the choice
  structure. Normally the decode functions will always read the header
  for you, however with the choice structure this is impossible. In
  this case you should read the header manually, determine which
  decode function to call, unpack the contents with read_contents, and
  send them in the contents optional. If contents is not None, then
  readbyte will never be called, and no attempt will be made to read
  the header or length. *)

(** Encoding/Decoding of the boolean primative ASN.1 type. Encode
  function encodes a valid ber type, including the header and length
  octets. *)
val decode_ber_bool : ?peek:bool -> ?cls:ber_class -> ?tag:int ->
  ?contents:string option -> readbyte -> bool
val encode_ber_bool : ?cls:ber_class -> ?tag:int -> bool -> string

(** Encoding/Decoding of the integer primative ASN.1 type.  Note, in
  this library, integers are represented as 32 bit values. In ASN.1
  there is no practical limit to the size of an integer, later on,
  this library may provide an encoder/decoder to Int64, and Bigints,
  however for now, this will have to do. Encode function encodes a
  valid ber type, including the header and length octets *)
val decode_ber_int32 : ?peek:bool -> ?cls:ber_class -> ?tag:int ->
  ?contents:string option -> readbyte -> int32
val encode_ber_int32 : ?cls:ber_class -> ?tag:int -> int32 -> string

(** Encoding/Decoding of enum primative ASN.1 type. Enums are simply
  integers, the same drawbacks apply as for decode_ber_int32. Encode
  function encodes a valid ber type, including the header and length
  octets *)
val decode_ber_enum : ?peek:bool -> ?cls:ber_class -> ?tag:int ->
  ?contents:string option -> readbyte -> int32
val encode_ber_enum : ?cls:ber_class -> ?tag:int -> int32 -> string

(** Encoding/Decoding of octetstring ASN.1 types. The Nested or
  "segmented" version of the octetstring encoding described in X.690
  is not yet supported. Encode function encodes a valid ber type,
  including the header and length octets *)
val decode_ber_octetstring : ?peek:bool -> ?cls:ber_class -> ?tag:int ->
  ?contents:string option -> readbyte -> string
val encode_ber_octetstring : ?cls:ber_class -> ?tag:int -> string -> string

(** Encoding/Decoding of Null ASN.1 type.  Almost useful as an
  assertion-type operation *)
val decode_ber_null : ?peek: bool -> ?cls:ber_class -> ?tag:int ->
  ?contents:string option -> readbyte -> unit
val encode_ber_null : ?cls:ber_class -> ?tag:int -> unit -> string

(** this function is for encoding lists of bervals, a common case.
  you pass it a list of things to encode, and an encoding function, and it
  will apply the encoding function to each element in the list, storing the
  resulting encoding in a buffer (which you may either pass in or not) *)
val encode_berval_list : ?buf:Buffer.t -> ('a -> string) -> 'a list -> string

(** this is the reverse of the above, it takes a readbyte structure, and
  returns a list of decoded elements, processed according to the decoder
  function you pass in. Note, that you MUST pass a readbyte structure built
  with readbyte_of_string, OR, your reabyte function must raise Stream.Failure
  when you reach the end of input. Otherwise this function will explode. That said,
  it is usually not practical to pass anything but a readbyte created by
  readbyte_of_string so this should not be a huge problem. *)
val decode_berval_list : ?lst:'a list -> (readbyte -> 'a) -> readbyte -> 'a list
