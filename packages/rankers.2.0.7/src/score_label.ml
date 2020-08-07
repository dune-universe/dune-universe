(* Copyright (C) 2019, Francois Berenger

   Yamanishi laboratory,
   Department of Bioscience and Bioinformatics,
   Faculty of Computer Science and Systems Engineering,
   Kyushu Institute of Technology,
   680-4 Kawazu, Iizuka, Fukuoka, 820-8502, Japan. *)

module type SL = sig
  type t
  val get_score: t -> float
  val get_label: t -> bool
  val get_name: t -> string
  (* to do a decreasing sort of a score labels list *)
  val high_score_first_cmp: t -> t -> int
end
