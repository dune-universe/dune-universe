module Naive_MEME : sig
  module Simulation : sig
    type t = {
      motif_sequences : string array ;
      background_sequences : string array ;
      pi : float ;
    }
    val make :
      Profile_matrix.DNA.t ->
      motif_probability:float ->
      n_sequences:int ->
      t
  end

  type input = string array

  type param = {
    pi : float ;
    motif : float array array ;
    bg : float array ;
  }

  val infer :
    motif_length:int ->
    input ->
    (float array * param) list

  val demo :
    ?alpha:float ->
    ?motif_length:int ->
    unit -> unit
end

module Naive_MEME_revcomp : sig
  module Simulation : sig
    type t = {
      motif_sequences : string array ;
      background_sequences : string array ;
      pi : float ;
    }
    val make :
      Profile_matrix.DNA.t ->
      motif_probability:float ->
      n_sequences:int ->
      t
  end

  type input = string array

  type param = {
    pi : float ;
    motif : float array array ;
    revcomp_motif : float array array ;
    bg : float array ;
  }

  val infer :
    ?niter:int ->
    motif_length:int ->
    input ->
    (float array array * param) list

  val demo :
    ?niter:int ->
    ?alpha:float ->
    ?motif_length:int ->
    ?n_sequences:int ->
    unit -> unit
end
