val gc : string -> float
val local_gc : int -> string -> float Seq.t

module type Parser = sig
  type t
  type score

  (** filter arguments:
      - position
      - GC content around this position
      - optimal score of a match finishing at this position
      - start position of the optimal match finishing at this position *)
  type 'a match_filter = int -> float -> int -> int -> 'a option

  type statistics = {
    nb_gc_levels : int ;
    values : score array array ; (* gc levels -> sorted score values *)
  }

  (** [statistics nbl parser] computes statistics for [nbl] number of GC levels *)
  val statistics : generator:(int -> float -> string) -> int -> t -> statistics
  val cdf_of_statistics : statistics -> float -> float -> float
  val average_cdf_of_statistics : statistics -> float -> float
  val bound_of_fpr : statistics -> float -> float
  val bound_for_gc_and_fpr : statistics -> gc:float -> fpr:float -> float

  (** returns (start, end), raw score, gc content, normalized score *)
  (* val fpr_filter : statistics -> float -> ((int * int) * int * float * float) match_filter *)

  (** returns location, raw score, gc content, normalized score *)
  (* val loc_fpr_filter : statistics -> float -> GLoc.t -> (GLoc.t * int * float * float) match_filter *)

(*
  val normalize : statistics -> string -> int array -> float array

  val fdr_scan : fdr:float -> t -> statistics -> string -> (int * int * float) list
  val use_location : Location.t -> (int * int * float) list -> (Location.t * float) list *)
end

module Parser_of_char(P : Wfa.Profile with type symbol = Wfa.Nucleotide.t
                                       and type score = float) : Parser with type score := float
