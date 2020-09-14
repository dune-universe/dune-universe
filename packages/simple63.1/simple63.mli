(** Simple63 is a module for compressing and decompressing sequences
   of integers along the ideas described in the
   {{:https://scholar.google.com/scholar?q=Index+compression+using+64-bit+words+moffat+anh}2010
   paper by Anh and Moffat}. Like Simple-8b technique described in
   that paper, Simple63 is a word-bounded, and (as the name suggests)
   is the result of adapting Simple-8b to work with OCaml's 63-bit
   integers. While using [int64] integer types would have been
   possible, the additional boxing required to manipulate [int64]'s
   make this option unappealing. *)

val max_value : int
(** The range of integers that can be encoded is [0] to [max_value],
   where [max_value] = [(1 lsl 59) - 1] = [576460752303423488] ~ [5.8e17].
   *)

exception Invalid of int
(** exception [Invalid] is raised whenever the value of an integer to
   be encoded falls outside the valid range of such values. See
   {!val:max_value}. *)

val encode_to_seq : int Seq.t -> int Seq.t
(** [encode_to_seq seq] returns an sequence of encoded words
   (63-bit integers). Example use:
    {[
       let in_lst = [1; 22; 333; 4444] in
       let in_seq = List.to_seq in_lst in
       let out_seq = encode_to_seq in_seq in

       (* confirm that we get out what we've put in: *)
       let in_seq' = decode_from_seq out_seq in
       let in_lst' = List.of_seq in_seq' in
       assert (in_lst = in_lst')
     ]}
*)

val decode_from_seq : int Seq.t -> int Seq.t
(** [decode_from_seq seq] returns a sequence of decoded integers,
   where [seq] is a sequence of encoded integers. See
   {!val:encode_to_seq} for an example. *)

val encode_len : int Seq.t -> int
(** [encode_len seq] returns the number of words into which input
   sequence [seq] would be encoded. The quotient of that count and the
   length of [seq] is the compression ratio. [encode_len] merely
   encodes [seq] to determine the length of output. *)

type iba = (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t

val encode_to_bigarray : int Seq.t -> int -> iba -> int
(** [encode_to_bigarray seq offset a] encodes input sequence [seq]
   onto bytearray [a] starting at offset [offset] *)

val decode_from_bigarray : (int -> unit) -> n:int -> offset:int -> iba -> unit
(** [decode_from_bigarray f ~n ~offset a] decodes [n] integers from
   bigarray [a], starting at array offset [offset], and calling
   function [f] with each decoded value. *)

val encode_to_channel : int Seq.t -> out_channel -> unit
(** [encode_to_channel seq ouch] encodes input sequence [seq] into
   output channel [ouch]. *)

val decode_from_channel : (int -> unit) -> in_channel -> unit
(** [decode_from_channel f inch] decodes values from input channel
   [inch], calling function [f] with each decode value. If
   [End_of_file] is encountered prematurely, it is raised. *)
