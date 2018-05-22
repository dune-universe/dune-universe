(* these are the Basic Encoding Rules, standardized by the ITU-T in
   X.690 all comments containing "sec. x.x.x.x" are section numbers
   referring to sections in x.690

   Copyright (C) 2004 Eric Stokes, and The
   California State University at Northridge

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
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

exception Decoding_error of string
exception Encoding_error of string

type readbyte_error = End_of_stream
                      | Transport_error
                      | Peek_error
                      | Request_too_large
                      | Not_implemented
exception Readbyte_error of readbyte_error

(* our sole interface with the data is to read and write a byte.
   the user of the encodeing functions herin will pass a function
   of the type readbyte, or writebyte to us when the encoding function
   is called. We will use that function to get or set raw data *)
type readbyte = ?peek:bool -> int -> string
type writebyte = (char -> unit)

(* note on syntax. In this program I use some somewhat little used,
   but very useful syntatic notations for numbers in Ocaml.
   eg. 0b11000000, the 0b indicates a binary number, everything after
   it is the number.  eg. 0b1100_0000, the _ has no meaning, it is
   just a seperator, however, seperating the nibbles in this way makes
   binary numbers very readable *)

(* X.690 sec. 8.1.1 structure of an encoding *)
type ber_class = Universal
                 | Application
                 | Context_specific
                 | Private

type ber_length = Definite of int
                | Indefinite

(* all the meta info about a ber value *)
type ber_val_header = {ber_class: ber_class;
                       ber_primitive: bool;
                       ber_tag: int;
                       ber_length: ber_length}

(* readbyte implementations. A readbyte is a higher order function
   which creates functions which provide a uniform way for decoding
   functions to read data without having to know anything about where
   it comes from. To OO people it can essentially be viewed as a
   functional version of a class, it exposes "methods", and hides data
   and code. the Lber module includes several readbyte
   implementations, which allow the decoding functions to read data
   from sockets, SSL sockets, other readbyte classes (with imposed
   read barriers), and from simple strings. *)

(* return a readbyte implementation which uses another readbyte, but
   allows setting a read boundry. Useful for constructing views of the
   octet stream which end at the end of a ber structure. This is
   essential for reading certian structures because length is only
   encoded in the toplevel in order to save space. This function is the
   secret of Ocamldap's performance. *)
let readbyte_of_ber_element limit (rb:readbyte) =
  let peek_counter = ref 1
  and byte_counter = ref 0 in
    match limit with
        Definite limit ->
          let f ?(peek=false) length =
            if not peek then
              if !byte_counter + length <= limit then (
                peek_counter := 1;
                byte_counter := !byte_counter + length;
                rb length
              )
              else raise (Readbyte_error End_of_stream)
            else if !peek_counter + length <= limit && !byte_counter < limit then (
              peek_counter := !peek_counter + length;
              rb ~peek:true length
            )
            else raise (Readbyte_error End_of_stream)
          in
            f
      | Indefinite ->
(*          let peek_saw_eoc_octets = ref false
          and saw_eoc_octets = ref false
          and eoc_buf = String.create 1
          and eoc_buf_len = ref 0 in
          let f ?(peek=false) length =
            if !eoc_buf_len = 0 then
              if peek && !peek_saw_eoc_octets then
                raise (Readbyte_error End_of_stream)
              else if !saw_eoc_octets then
                raise (Readbyte_error End_of_stream)
              else
                let b = rb ~peek:peek 1 in
                  if (int_of_char b) = 0b0000_0000 then
                    let b1 = rb ~peek:peek 1 in
                      if (int_of_char b1) = 0b0000_0000 then
                        ((if peek then peek_saw_eoc_octets := true
                          else saw_eoc_octets := true);
                         raise (Readbyte_error End_of_stream))
                      else
                        (eoc_buf.[0] <- b1;
                         eoc_buf_len := 1;
                         String.make 1 b)
                  else String.make 1 b
            else
              (eoc_buf_len := 0;
               eoc_buf)
          in
            f *)
          raise (Readbyte_error Not_implemented)

(* return a readbyte implementation which works using a string *)
let readbyte_of_string octets =
(*  let strm = Stream.of_string octets in
  let peek_counter = ref 1 in
  let limit = ref 0 in
  let f ?(peek=false) length =
    let rec last lst =
      match lst with
          h :: [] -> h
        | h :: t -> last t
        | [] -> failwith "readbyte bug in \"last\" function!"
    in
      if not peek then (
        peek_counter := 1; (* reset the peek counter when we really read a byte *)
        try String.make 1 (Stream.next strm)
        with Stream.Failure -> raise (Readbyte_error End_of_stream)
      )
      else
        let elts = (Stream.npeek !peek_counter strm) in
          if List.length elts = !peek_counter then
            (peek_counter := !peek_counter + 1;
             (String.make 1 (last elts)))
          else raise (Readbyte_error End_of_stream)
            (* if there are not enough elements in the stream, fail *)
  in
    f
*)
  raise (Readbyte_error Not_implemented)

let readbyte_of_readfun rfun =
  let bufsize = 16384 in (* must be this for ssl *)
  let buf = Bytes.create (bufsize * 2) in
  let buf_len = ref 0 in
  let buf_pos = ref 0 in
  let peek_pos = ref 0 in
  let peek_buf_len = ref 0 in
  let read buf off len =
    try rfun buf off len
    with exn -> raise (Readbyte_error Transport_error)
  in
  let read_at_least_nbytes buf off len nbytes =
    let total = ref 0 in
      while !total < nbytes
      do
        let rd = read buf (!total + off) (len - !total) in
          if rd <= 0 then
            raise (Readbyte_error Transport_error);
          total := !total + rd;
      done;
      !total
  in
  let rec rb ?(peek=false) length =
    if length <= 0 then raise (Invalid_argument "Readbyte.length");
    if length > bufsize then (
      if length > Sys.max_string_length then raise (Readbyte_error Request_too_large);
      let result = Bytes.create length in
      let total = ref 0 in
        while !total < length
        do
          let nbytes_to_read =
            if length - !total < bufsize then
              length - !total
            else bufsize
          in
          let iresult = rb ~peek nbytes_to_read in
            String.blit iresult 0 result !total nbytes_to_read;
            total := !total + nbytes_to_read
        done;
        Bytes.to_string result
    )
    else if not peek then (
      if length <= !buf_len - !buf_pos then (
        let result = Bytes.sub_string buf !buf_pos length in
          buf_pos := !buf_pos + length;
          peek_pos := !buf_pos;
          result
      )
      else (
        let result = Bytes.create length in
        let nbytes_really_in_buffer = (!buf_len - !buf_pos) + !peek_buf_len in
        let nbytes_in_buffer =
          if nbytes_really_in_buffer > length then length
          else nbytes_really_in_buffer
        in
        let nbytes_to_read = length - nbytes_in_buffer in
          if nbytes_in_buffer > 0 then
            Bytes.blit buf !buf_pos result 0 nbytes_in_buffer;
          if nbytes_to_read > 0 then (
            let nbytes_read = read_at_least_nbytes buf 0 bufsize nbytes_to_read in
              Bytes.blit buf 0 result nbytes_in_buffer nbytes_to_read;
              buf_pos := nbytes_to_read;
              buf_len := nbytes_read;
              peek_pos := !buf_pos;
              peek_buf_len := 0;
              Bytes.to_string result
          )
          else (
            Bytes.blit buf 0 buf (!buf_pos + length) (nbytes_really_in_buffer - length);
            buf_len := (nbytes_really_in_buffer - length);
            buf_pos := 0;
            peek_pos := !buf_pos;
            peek_buf_len := 0;
            Bytes.to_string result
          )
      )
    ) (* if not peek *)
    else (
      if length <= (!buf_len + !peek_buf_len) - !peek_pos then (
        let result = Bytes.sub_string buf !peek_pos length in
          peek_pos := !peek_pos + length;
          result
      )
      else (
        if length + !peek_pos > 2 * bufsize then raise (Readbyte_error Peek_error);
        let result = Bytes.create length in
        let nbytes_in_buffer = (!buf_len + !peek_buf_len) - !peek_pos in
        let nbytes_to_read = length - nbytes_in_buffer in
        let read_start_pos = !peek_pos + nbytes_in_buffer in
          Bytes.blit buf !peek_pos result 0 nbytes_in_buffer;
          let nbytes_read =
            read_at_least_nbytes buf
              read_start_pos
              (bufsize - (!buf_len + !peek_buf_len))
              nbytes_to_read
          in
            Bytes.blit buf read_start_pos result nbytes_in_buffer nbytes_read;
            peek_buf_len := !peek_buf_len + nbytes_read;
            peek_pos := !peek_pos + length;
            Bytes.to_string result
      )
    )
  in
    rb

(* a readbyte implementation which reads from an FD. It implements a
   peek buffer, so it can garentee that it will work with
   readbyte_of_ber_element, even with blocking fds. *)
let readbyte_of_fd fd =
  readbyte_of_readfun
    (fun buf off len ->
       try Unix.read fd buf off len
       with exn ->
         (try Unix.close fd with _ -> ());raise exn)

(* a readbyte implementation which reads from an SSL socket. It is
   otherwise the same as rb_of_fd *)
let readbyte_of_ssl fd =
  readbyte_of_readfun
    (fun buf off len ->
       try Ssl.read fd buf off len
       with exn ->
         (try Ssl.shutdown fd with _ -> ());raise exn)

let decode_ber_length ?(peek=false) (readbyte:readbyte) = (* sec. 8.1.3.3, the definite length form *)
  let octet = int_of_char (readbyte ~peek:peek 1).[0] in
    if octet = 0b1111_1111 then
      (* sec/ 8.1.3.5c *)
      raise (Decoding_error "illegal initial length octet")
    else if octet = 0b1000_0000 then
      (* sec. 8.1.3.6 indefinite form *)
      Indefinite
    else if octet land 0b1000_0000 = 0b0000_0000 then
      (* sec. 8.1.3.4, definite length, short form *)
      Definite (octet land 0b0111_1111)
    else
      (* sec. 8.1.3.5, definite length, long form *)
      let rec decode_multioctet_length (readbyte:readbyte) numoctets remainingoctets value =
        if numoctets > 4 then raise (Decoding_error "length cannot be represented");
        if remainingoctets = 0 then Definite value
        else
          let octet = int_of_char (readbyte ~peek:peek 1).[0] in
            if ((numoctets = 4) && (remainingoctets = 4) &&
                (octet land 0b1000_0000 = 0b1000_0000)) (* we have only 31 bits *)
            then
              raise (Decoding_error "length cannot be represented")
            else
              decode_multioctet_length readbyte numoctets (remainingoctets - 1)
                (value + (octet lsl ((numoctets - (numoctets - remainingoctets) - 1) * 8)))
      in
      let numoctets = octet land 0b0111_1111 in
        decode_multioctet_length readbyte numoctets numoctets 0

let decode_ber_header ?(peek=false) (readbyte:readbyte) =
  let leading_octet = int_of_char (readbyte ~peek:peek 1).[0] in
  let ber_tag = (* sec. 8.1.2.2c *)
    if leading_octet land 0b0001_1111 = 0b0001_1111 then
      (* sec. 8.1.2.4 multi octet tag encoding *)
      let rec decode_multioctet_tag (readbyte:readbyte) tag_value =
        let octet = int_of_char (readbyte ~peek:peek 1).[0] in
          if octet land 0b1000_0000 = 0b0000_0000 then tag_value + (octet land 0b0111_1111)
          else decode_multioctet_tag readbyte (tag_value + (octet land 0b0111_1111))
      in
        decode_multioctet_tag readbyte 0
    else
      (* sec. 8.1.2.2 single octet tag encoding *)
      leading_octet land 0b0001_1111
  in
  let ber_length = decode_ber_length ~peek:peek readbyte in
    {ber_class = (* sec. 8.1.2.2a table 1 *)
       (match leading_octet land 0b1100_0000 with
            0b0000_0000 -> Universal
          | 0b0100_0000 -> Application
          | 0b1000_0000 -> Context_specific
          | 0b1100_0000 -> Private
          | _ -> raise (Decoding_error "ber_class, decoder bug"));
     ber_primitive = (* sec. 8.1.2.5 *)
       (match leading_octet land 0b0100_0000 with
            0b0100_0000 -> false (* value is constructed *)
          | 0b0000_0000 -> true
          | _ -> raise (Decoding_error "ber_primitive, decoder bug")); (* value is primative *)
     ber_tag = ber_tag;
     ber_length = ber_length}

let encode_ber_header {ber_class=cls;ber_primitive=pri;ber_tag=tag;ber_length=len} =
  let buf = Buffer.create 3 in
  let rec encode_multioctet_tag tag buf =
    if tag > 127 then
      (Buffer.add_char buf (char_of_int 255);
       encode_multioctet_tag (tag - 127) buf)
    else
      Buffer.add_char buf (char_of_int tag)
  in
  let rec long_form_length len buf = (* sec 8.1.3.5 encode the length in up to 1 + 4 octets *)
    if len < 255 then (* fits in 8 bits? *)
      (Buffer.add_char buf (char_of_int 0b1000_0001); (* long form with one octet *)
       Buffer.add_char buf (char_of_int len))
    else if len < 65535 then (* fits in 16 bits? *)
      (Buffer.add_char buf (char_of_int 0b1000_0010); (* long form with two octets *)
       Buffer.add_char buf (char_of_int ((len land 0b11111111_00000000) lsr 8));
       Buffer.add_char buf (char_of_int (len land 0b00000000_11111111)))
    else if len < 16777215 then (* fits in 24 bits? *)
      (Buffer.add_char buf (char_of_int 0b1000_0011); (* long form in three octets *)
       Buffer.add_char buf (char_of_int ((len land 0b11111111_00000000_00000000) lsr 16));
       Buffer.add_char buf (char_of_int ((len land 0b00000000_11111111_00000000) lsr 8));
       Buffer.add_char buf (char_of_int (len land 0b00000000_00000000_11111111)))
    else (* can't currently encode anything bigger than 31 bits *)
      (Buffer.add_char buf (char_of_int 0b1000_0100);
       Buffer.add_char buf (char_of_int ((len land 0b00111111_00000000_00000000_00000000) lsr 24));
       Buffer.add_char buf (char_of_int ((len land 0b00000000_11111111_00000000_00000000) lsr 16));
       Buffer.add_char buf (char_of_int ((len land 0b00000000_00000000_11111111_00000000) lsr 8));
       Buffer.add_char buf (char_of_int  (len land 0b00000000_00000000_00000000_11111111)));
  in
    Buffer.add_char buf (* deal with the header *)
      (char_of_int
         ((match cls with
               Universal -> 0b0000_0000
             | Application -> 0b0100_0000
             | Context_specific -> 0b1000_0000
             | Private -> 0b1100_0000) lor
          (if pri then 0b0000_0000 else 0b0010_0000) lor
          (if tag > 31 then 0b0001_1111 else tag)));
    if tag > 31 then encode_multioctet_tag tag buf;
    (match len with (* deal with the length *)
         Definite len ->
           if len < 127 then Buffer.add_char buf (char_of_int len)
           else long_form_length len buf;
       | Indefinite -> raise (Encoding_error "indefinite length encoding not implemented"));
    Buffer.contents buf

let read_contents ?(peek=false) (readbyte:readbyte) len =
  let rec readuntileoc (readbyte:readbyte) buf =
    let octet1 = (readbyte ~peek 1).[0] in
      if (int_of_char octet1) = 0b0000_0000 then
        let octet2 = (readbyte ~peek 1).[0] in
          if (int_of_char octet2) = 0b0000_0000 then
            Buffer.contents buf
          else
            (Buffer.add_char buf octet1;Buffer.add_char buf octet2;
             readuntileoc readbyte buf)
      else
        (Buffer.add_char buf octet1;readuntileoc readbyte buf)
  in
    match len with
        Definite n -> if n = 0 then "" else readbyte ~peek n
      | Indefinite -> readuntileoc readbyte (Buffer.create 5)

let decode_ber_end_of_contents ?(peek=false) (readbyte:readbyte) =
  if not (((int_of_char (readbyte ~peek 1).[0]) = 0) &&
          (int_of_char (readbyte ~peek 1).[0]) = 0) then
    raise (Decoding_error "missing end of contents octets")

(* sec. 8.2 *)
let decode_ber_bool ?(peek=false) ?(cls=Universal) ?(tag=1) ?(contents=None)
  (readbyte:readbyte) =
  let decode_ber_bool' contents =
    if (int_of_char contents.[0]) = 0 then false else true
  in
    match contents with
        None ->
          (match decode_ber_header ~peek:peek readbyte with
               {ber_class=c;ber_tag=t;ber_length=bool_length} when c=cls && t=tag ->
                 decode_ber_bool' (read_contents ~peek:peek readbyte bool_length)
             | _ -> raise (Decoding_error "expected bool"))
      | Some contents -> decode_ber_bool' contents

let encode_ber_bool ?(cls=Universal) ?(tag=1) value =
  let buf = Buffer.create 3 in
    Buffer.add_string buf
      (encode_ber_header
         {ber_class=cls;ber_primitive=true;ber_tag=tag;ber_length=Definite 1});
    Buffer.add_char buf
      (if value then char_of_int 1
       else char_of_int 0);
    Buffer.contents buf

(* sec 8.3 *)
let decode_ber_int32 ?(peek=false) ?(cls=Universal) ?(tag=2) ?(contents=None)
  (readbyte:readbyte) =
  let decode_ber_int32' contents =
    let length = String.length contents in
      if length > 5 then
        raise (Decoding_error "integer overflow, use bigger decode function?")
      else if length > 0 then
        let c i = Int32.of_int (int_of_char i) in
        let rec convert octets l i v =
          if i <= l then
            convert octets l (i + 1)
              (Int32.logor v (Int32.shift_left (c octets.[i]) (8 * (l - i))))
          else v
        in
        let v = convert contents (length - 1) 0 0l in
          if (Int32.logand (c contents.[0]) 0b10000000l) = 0b10000000l then
            (* the number should be negative, fix it. For a less than
               4 byte encoding, we need to set all the bits left of the data to
               1. This operation will have no effect on a 4 byte encoding *)
            (Int32.logor
               (Int32.shift_left (-1l) (length * 8))
               v)
          else
            v
      else raise (Decoding_error "integer, no contents octets") (* sec 8.3.1 *)
  in
    match contents with
        None -> (* we have not yet read the header, and unpacked the contents *)
          (match decode_ber_header ~peek:peek readbyte with
               {ber_class=c;ber_tag=t;ber_length=int_length} when c=cls && t=tag ->
                 decode_ber_int32' (read_contents ~peek:peek readbyte int_length)
             | _ -> raise (Decoding_error "expected int"))
      | Some contents -> decode_ber_int32' contents (* we already have the contents *)

let encode_ber_int32 ?(cls=Universal) ?(tag=2) value =
  let to_char i = char_of_int (Int32.to_int i) in
  let encode_positive_int32 value =
    let buf = Buffer.create 4 in
      (if value < 0b01111111l then (* fits in 7 bits + sign bit? *)
         Buffer.add_char buf (to_char value) (* byte one, MSB *)
       else if value < 0b01111111_11111111l then (* fits in 15 bits + sign bit? *)
         (Buffer.add_char buf (* byte one, MSB *)
            (to_char
               (Int32.shift_right
                  (Int32.logand value 0b01111111_00000000l)
                  8));
          Buffer.add_char buf (* byte two *)
            (to_char (Int32.logand value 0b00000000_11111111l)))
       else if value < 0b01111111_11111111_11111111l then (* fits in 23 bits + sign bit? *)
         (Buffer.add_char buf (* byte one, MSB *)
            (to_char
               (Int32.shift_right
                  (Int32.logand value 0b01111111_00000000_00000000l)
                  16));
          Buffer.add_char buf (* byte two *)
            (to_char
               (Int32.shift_right
                  (Int32.logand value 0b00000000_11111111_00000000l)
                  8));
          Buffer.add_char buf (* byte three *)
            (to_char (Int32.logand value 0b00000000_00000000_11111111l)))
       else (* use 31 bits + sign bit *)
         (Buffer.add_char buf (* byte one, MSB *)
            (to_char
               (Int32.shift_right
                  (Int32.logand value 0b01111111_00000000_00000000_00000000l)
                  24));
          Buffer.add_char buf (* byte two *)
            (to_char
               (Int32.shift_right
                  (Int32.logand value 0b00000000_11111111_00000000_00000000l)
                  16));
          Buffer.add_char buf (* byte three *)
            (to_char
               (Int32.shift_right
                  (Int32.logand value 0b00000000_00000000_11111111_00000000l)
                  8));
          Buffer.add_char buf (* byte four *)
            (to_char
               (Int32.logand value 0b00000000_00000000_00000000_11111111l))));
      buf
  in
  let encode_negative_int32 value =
    let buf = Buffer.create 4 in
      (* We must manually set the sign bit for the first octet of the
         encoding. So we must turn the real sign bit off, and set the
         first bit of the first octet in the encoded stream, because
         it will become the sign bit on the other side. *)
      (if value > 0b11111111_11111111_11111111_10000000l then
         (* fits in 7 bits + sign bit *)
         Buffer.add_char buf (* byte one, MSB *)
           (to_char
              (Int32.logor (* flip what WILL be the sign bit in the encoded byte ON *)
                 0b1000_0000l
                 (Int32.logand (* flip the sign bit for the WHOLE word OFF *)
                    0b00000000_00000000_00000000_1111111l
                    value)))
       else if value > 0b11111111_11111111_10000000_00000000l then
         (* fits in 15 bits + sign bit *)
         (Buffer.add_char buf (* byte one, MSB *)
            (to_char
               (Int32.logor (* flip what WILL be the sign bit in the encoded byte ON *)
                  0b1000_0000l
                  (Int32.shift_right
                     (Int32.logand (* this mask also accomplishes flipping the sign bit OFF *)
                        0b00000000_00000000_11111111_00000000l
                        value)
                     8)));
          Buffer.add_char buf
            (to_char
               (Int32.logand
                  0b00000000_00000000_00000000_11111111l
                  value))) (* byte two *)
       else if value > 0b11111111_10000000_00000000_00000000l then
         (* fits in 23 bits + sign bit *)
         (Buffer.add_char buf (* byte one, MSB *)
            (to_char
               (Int32.logor (* flip what WILL be the sign bit in the encoded byte ON *)
                  0b1000_0000l
                  (Int32.shift_right
                     (Int32.logand (* this mask also accomplishes flipping the sign bit OFF *)
                        0b00000000_11111111_00000000_00000000l
                        value)
                     16)));
          Buffer.add_char buf (* byte two *)
            (to_char
               (Int32.shift_right
                  (Int32.logand (* this mask also accomplishes flipping the sign bit OFF *)
                     0b00000000_00000000_11111111_00000000l
                     value)
                  8));
          Buffer.add_char buf (* byte three *)
            (to_char
               (Int32.logand (* this mask also accomplishes flipping the sign bit OFF *)
                  0b00000000_00000000_00000000_11111111l
                  value)))
       else
         (* fits in 31 bits + sign bit *)
         (Buffer.add_char buf (* byte one, MSB *)
            (to_char
               (Int32.logor (* flip what WILL be the sign bit in the encoded byte ON *)
                  0b1000_0000l
                  (Int32.shift_right
                     (Int32.logand (* this mask also accomplishes flipping the sign bit OFF *)
                        0b01111111_00000000_00000000_00000000l
                        value)
                     24)));
          Buffer.add_char buf (* byte two *)
            (to_char
               (Int32.shift_right
                  (Int32.logand (* this mask also accomplishes flipping the sign bit OFF *)
                     0b00000000_11111111_00000000_00000000l
                     value)
                  16));
          Buffer.add_char buf (* byte three *)
            (to_char
               (Int32.shift_right
                  (Int32.logand (* this mask also accomplishes flipping the sign bit OFF *)
                     0b00000000_00000000_11111111_00000000l
                     value)
                  8));
          Buffer.add_char buf (* byte four *)
            (to_char
               (Int32.logand (* this mask also accomplishes flipping the sign bit OFF *)
                  0b00000000_00000000_00000000_11111111l
                  value))));
      buf
  in
  let buf =
    if value < 0l then (* if its less than zero we must encode differently *)
      encode_negative_int32 value
    else
      encode_positive_int32 value
  in
  let buf1 = Buffer.create 5 in
    Buffer.add_string buf1
      (encode_ber_header
         {ber_class=cls;
          ber_tag=tag;
          ber_primitive=true;
          ber_length=Definite (Buffer.length buf)});
    Buffer.add_buffer buf1 buf;
    Buffer.contents buf1

(* sec. 8.4 *)
let decode_ber_enum ?(peek=false) ?(cls=Universal) ?(tag=10) ?(contents=None)
  (readbyte:readbyte) =
  decode_ber_int32 ~peek:peek ~cls:cls ~tag:tag ~contents:contents readbyte

let encode_ber_enum ?(cls=Universal) ?(tag=10) value =
  encode_ber_int32 ~cls:cls ~tag:tag value

(* sec 8.7 *)
let decode_ber_octetstring ?(peek=false) ?(cls=Universal) ?(tag=4) ?(contents=None)
  (readbyte:readbyte) =
  match contents with
      None -> (* have not yet read the header, or unpacked the contents *)
        (match decode_ber_header readbyte with
             {ber_class=c;ber_tag=t;ber_length=octetstring_length} when c=cls && t=tag ->
               read_contents ~peek readbyte octetstring_length
           | _ -> raise (Decoding_error "expected octetstring"))
    | Some contents -> contents

let encode_ber_octetstring ?(cls=Universal) ?(tag=4) string =
  let len = String.length string in
  let buf = Buffer.create (len + 3) in
    Buffer.add_string buf
      (encode_ber_header
         {ber_class=cls;
          ber_tag=tag;
          ber_primitive=true;
          ber_length=Definite len});
    Buffer.add_string buf string;
    Buffer.contents buf

let encode_ber_null ?(cls=Universal) ?(tag=5) () =
  encode_ber_header {ber_class=cls;
                     ber_tag=tag;
                     ber_primitive=true;
                     ber_length=Definite 0}

let decode_ber_null ?(peek=false) ?(cls=Universal) ?(tag=5) ?(contents=None)
  (readbyte:readbyte) =
  let decode_ber_null' contents = () in
    match contents with
        None ->
          (match decode_ber_header ~peek:peek readbyte with
               {ber_class=c; ber_tag=t; ber_length=l}
                 when c=cls && t=tag && l=Definite 0 ->
                   decode_ber_null' None
             | _ -> raise (Decoding_error "expected null"))
      | Some contents -> decode_ber_null' contents

let rec encode_berval_list ?(buf=Buffer.create 50) efun lst =
  match lst with
      hd :: [] ->
        Buffer.add_string buf (efun hd);
        Buffer.contents buf
    | hd :: tl ->
        (encode_berval_list
           ~buf:(Buffer.add_string buf (efun hd);buf) efun tl)
    | [] -> Buffer.contents buf

let rec decode_berval_list ?(lst=[]) dfun (readbyte:readbyte) =
  try decode_berval_list ~lst:((dfun readbyte) :: lst) dfun readbyte
  with Readbyte_error End_of_stream -> lst
