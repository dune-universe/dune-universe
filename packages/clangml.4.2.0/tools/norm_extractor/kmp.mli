module type WORD = sig
  type t

  type letter

  val letter_equal : letter -> letter -> bool

  val length : t -> int

  val unsafe_get : t -> int -> letter
end

module type S = sig
  type word

  type letter

  val find : word -> ?offset:int -> letter Seq.t -> int Seq.t
end

module Make (Word : WORD)
    : S with type word = Word.t and type letter = Word.letter

module On_string : S with type word = string and type letter = char

val seq_of_in_channel : in_channel -> char Seq.t
