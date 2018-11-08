
(* we hide some scores from score.ml;
   because the actual score being used is supposed to be chosen via
   the Flags module and CLI options *)

val get_score: Flags.score -> (float array -> Fingerprint.t -> float)

val get_fp_score: Flags.score -> (Fingerprint.t -> Fingerprint.t -> float)

val fp_tanimoto_score: Fingerprint.t -> Fingerprint.t -> float

val fp_tanimoto_dist: Fingerprint.t -> Fingerprint.t -> float

val tanimoto_intmap: float BatMap.Int.t -> Fingerprint.t -> float
