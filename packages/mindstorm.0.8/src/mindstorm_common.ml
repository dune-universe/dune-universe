(* File: mindstorm_common.ml

   Copyright (C) 2007-

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umons.ac.be/anum/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 2.1 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)

[@@@warning "-32"]

(* Specialised implementation for speed *)
let min i j = if (i:int) < j then i else j

(** Helper functions *)

(* Char of a signed int, 2's complement.  All uses of this function
   are for values in the range -100 .. 100, so if outside the value is
   mapped to the closer endpoint. *)
let signed_chr i =
  let i = if i < -100 then -100 else if i > 100 then 100 else i in
  Char.unsafe_chr(if i >= 0 then i else 256 + i)

(* int of a char, seen as a signed int *)
let signed_code c =
  if Char.code c <= 127 then Char.code c else Char.code c - 256

(* Converts the 2 bytes s.[i] (least significative byte) and s.[i+1]
   (most significative byte) into the corresponding UNSIGNED integer. *)
let uint16 s i =
  assert(i + 1 < Bytes.length s);
  Char.code(Bytes.get s i) lor (Char.code(Bytes.get s (i+1)) lsl 8)

(* Converts the 2 bytes s.[i] (least significative byte) and s.[i+1]
   (most significative byte) into the corresponding SIGNED integer. *)
let int16 s i =
  assert(i + 1 < Bytes.length s);
  let x = Char.code(Bytes.get s i) lor (Char.code(Bytes.get s (i+1)) lsl 8) in
  if x land 0x8000 = 0 then (* positive *) x else
    (* negative, complete with 1 the higher bits *)
    x lor (-0x10000)

let copy_uint16 (i: int) s ofs =
  assert(ofs + 1 < Bytes.length s);
  Bytes.set s ofs (Char.unsafe_chr(i land 0xFF)); (* LSB *)
  Bytes.set s (ofs + 1) (Char.unsafe_chr((i lsr 8) land 0xFF)) (* MSB *)

(* Converts the 4 bytes s.[i] (LSB) to s.[i+3] (MSB) into the
   corresponding UNSIGNED int.  Used when the spec specifies a ULONG.
*)
let uint32 s i =
  assert(i + 3 < Bytes.length s);
#ifndef AMD64
    (* OCaml int are 31 bits (on a 32 bits platform), thus raise an
       exception if the last bit is set. *)
    if Bytes.get s (i + 3) >= '\x40' then
      failwith MODULE(uint32: overflow (32 bits));
#endif
  Char.code(Bytes.get s i)
  lor (Char.code(Bytes.get s (i + 1)) lsl 8)
  lor (Char.code(Bytes.get s (i + 2)) lsl 16)
  lor (Char.code(Bytes.get s (i + 3)) lsl 24)


(* [fill32 = -0x1_0000_0000] but in a way that is accepted by camlp4
   even on 32 bits platforms (no range overflow).  The goal is to add
   enough 1 in front of a 32 bits integer so it is properly
   interpreted as a 63 bits one. *)
let fill32 = -(1 lsl 32)

(* Converts the 4 bytes s.[i] (LSB) to s.[i+3] (MSB) into the
   corresponding SIGNED int.  Used when the spec specifies a SLONG.
   Since OCaml int are 31 bits (on a 32 bits platform), raise an exn
   if the last bit is set. *)
let int32 s i =
  assert(i + 3 < Bytes.length s);
  let msb = Char.code (Bytes.get s (i + 3)) in
  if msb >= 0x80 then (
    (* negative number *)
#ifndef AMD64
    (* 32 bits architecture *)
    if msb land 0x40 = 0 then failwith MODULE(int32: overflow (32 bits));
#endif
    let x = Char.code (Bytes.get s i)
            lor (Char.code(Bytes.get s (i + 1)) lsl 8)
            lor (Char.code(Bytes.get s (i + 2)) lsl 16)
            lor (msb lsl 24) in
#ifdef AMD64
    (* bits 0 .. 31 are set by x ; complete by setting to 1 the bits
       32 to 62 (Caml ints are 63 bits). *)
    x lor fill32
#else
    x (* "sign bit" set because [msb land 0x40 = 1] *)
#endif
  )
  else (
    (* positive number *)
#ifndef AMD64
    if msb >= 0x40 then failwith MODULE(int32: overflow (32 bits));
#endif
    Char.code (Bytes.get s i)
    lor (Char.code(Bytes.get s (i + 1)) lsl 8)
    lor (Char.code(Bytes.get s (i + 2)) lsl 16)
    lor (msb lsl 24)
  )

(* Copy the int [i] as 4 bytes (little endian) to [s] starting at
   position [ofs].  Used when the spec specifies a ULONG. *)
let copy_uint32 i s ofs =
  assert(i >= 0);
  Bytes.set s ofs (Char.unsafe_chr(i land 0xFF)); (* LSB *)
  Bytes.set s (ofs + 1) (Char.unsafe_chr((i lsr 8) land 0xFF));
  Bytes.set s (ofs + 2) (Char.unsafe_chr((i lsr 16) land 0xFF));
  Bytes.set s (ofs + 3) (Char.unsafe_chr((i lsr 24) land 0xFF)) (* MSB *)

(* Extracts the filename in [s.[ofs .. ofs+19]] *)
let get_filename s ofs =
  try
    let i = Bytes.index_from s ofs '\000' in
    if i > ofs + 19 then
      failwith MODULE_ERR(invalid filename send by the brick!);
    Bytes.sub_string s ofs (i - ofs)
  with Not_found ->
    failwith MODULE_ERR(invalid filename send by the brick!)

(** [blit_filename funname fname pkg ofs] raises
    [Invalid_argument] if the filename [fname] is not valid
    according to the brick limitations; otherwise copy it to [pkg]
    starting at [ofs].  *)
let blit_filename : string -> string -> Bytes.t -> int -> unit =
  fun funname fname pkg ofs ->
    let len = String.length fname in
    if len > 19 then invalid_arg funname;
    for i = 0 to String.length fname - 1 do
      if fname.[i] < ' ' || fname.[i] >= '\127' then invalid_arg funname;
    done;
    String.blit fname 0 pkg ofs len;
    (* All filenames must be 19 bytes long + null terminator, pad if
       needed.  *)
    Bytes.fill pkg (ofs + len) (20 - len) '\000'

let usleep sec =
  ignore(Unix.select [] [] [] sec)

[@@@warning "@32"]
