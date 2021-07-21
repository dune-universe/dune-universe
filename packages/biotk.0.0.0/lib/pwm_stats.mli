(** Implementation of {https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2238751/}*)

module TFM_pvalue : sig
  val score_distribution :
    ?alpha:float ->
    ?beta:float ->
    Pwm.t ->
    Pwm.background ->
    (float * float) list

  val fast_pvalue :
    Pwm.t ->
    Pwm.background ->
    float ->
    float

  val score_of_pvalue :
    Pwm.t ->
    Pwm.background ->
    float ->
    float
end

val naive_score_distribution :
  Pwm.t ->
  Pwm.background ->
  (float * float) list
