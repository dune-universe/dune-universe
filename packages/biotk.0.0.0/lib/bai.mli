(**
   BAI format: index for BAM file

   @see https://samtools.github.io/hts-specs/SAMv1.pdf
   @see https://academic.oup.com/bioinformatics/article/32/14/2202/1742831
*)
open Stdio

(** [chunk_beg] should be less than [chunk_end], except for chunks in
   the infamous metadata bin (bin id 37450) *)
type chunk = {
  chunk_beg : Int64.t ;
  chunk_end : Int64.t ;
}

type bin = {
  bin : int ;
  n_chunk : int ;
  chunks : chunk array ;
}

type interval = Ioffset of Int64.t [@@unboxed]

type reference_sequence = {
  n_bin : int ;
  bins : bin array ;
  n_intv : int ;
  intervals : interval array ;
}

type t = {
  n_ref : int ;
  reference_sequences : reference_sequence array ;
  n_no_coor : Int64.t option ;
}

val read : In_channel.t -> (t, [> `Msg of string]) result

val reg2bin : int -> int -> int
val reg2bins : int -> int -> init:'a -> f:('a -> int -> 'a) -> 'a
