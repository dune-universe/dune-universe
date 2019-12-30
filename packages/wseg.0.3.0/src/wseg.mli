(*
 * wseg.mli
 * -----------
 * Copyright : (c) 2019, ZAN DoYe <zandoye@gmail.com>
 * Licence   : MIT
 *
 * This file is a part of wseg
 *)

module Dict :
  sig
    module Tree :
      sig
        type 'a node
      end

    (** (word|char) * (count|occurrence probability) *)
    type entry= string * float

    type entries= entry list

    (** (char list) * (occurrence probability) *)
    type word= string list * float
    type chunk= word list

    (** print all conditions *)
    val dispConds : word list -> unit

    (** print all candidates *)
    val dispCands : chunk list -> unit

    (** compose a result string from a candidate *)
    val result_of_cand : chunk -> string

    (** build a (word|char * occurrence probability) entry list from a (word|char * count) list *)
    val buildEntries : entries -> entries

    (** build an index, to be used as a dictionary *)
    val buildIndex : entries-> float Tree.node

    (** query all the candidates from a dictionary.
      [Dict.candidates dict str max] query [str] in [dict], and limit maximum number in a chunk to be [max]
      *)
    val candidates : float Tree.node -> string -> int -> chunk list
  end

(** MMSEg: A Word Identification System Based on the Maximum Matching Algorithm. *)
module MMSEG :
  sig
    (** MMSEG rule1: maximum matching *)
    val rule1 : Dict.chunk list -> Dict.chunk list

    (** MMSEG rule2: largest average word length *)
    val rule2 : Dict.chunk list -> Dict.chunk list

    (** MMSEG rule3: smallest variance of word lengths *)
    val rule3 : Dict.chunk list -> Dict.chunk list

    (** MMSEG rule4: largest sum of probability of the chain of all words *)
    val rule4 : Dict.chunk list -> Dict.chunk list

    (** rule final, [List.hd] *)
    val rule_final : Dict.chunk list -> Dict.chunk option

    (** apply rule1 .. rule4 .. rule final to a chunk list *)
    val seg : Dict.chunk list -> Dict.chunk option
  end

