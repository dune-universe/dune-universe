(** Utilities for the {{https://github.com/nboley/idr}IDR program} *)

module Narrow_output : sig
  module Row : sig
    type t = {
      chrom : string ;
      chromStart : int ;
      chromEnd : int ;
      name : string ;
      score : int ;
      strand : [`Plus | `Minus] option ;
      signalValue : float ;
      pvalue : float ;
      qvalue : float ;
      summit : int ;
      localIDR : float ;
      globalIDR : float ;
    }

    val loc : t -> GLoc.t
  end

  val from_file : string -> (Row.t list, [> `Msg of string]) result
end

module Broad_output : sig
  module Row : sig
    type t = {
      chrom : string ;
      chromStart : int ;
      chromEnd : int ;
      name : string ;
      score : int ;
      strand : [`Plus | `Minus] option ;
      signalValue : float ;
      pvalue : float ;
      qvalue : float ;
      localIDR : float ;
      globalIDR : float ;
    }

    val loc : t -> GLoc.t
  end

  val from_file : string -> (Row.t list, [> `Msg of string]) result
end
