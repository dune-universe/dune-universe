open Biocaml_base

type record = {
  chrom : string ;
  chromStart : int ;
  chromEnd : int ;
  name : string ;
  score : int ;
  strand : [ `Plus | `Minus | `Not_relevant | `Unknown ] ;
  signalValue : float ;
  pValue : float option ; (** -log10 pval *)
  qValue : float option ; (** -log10 qval *)
  peak : int option ;
}

type item = [
  | `Comment of string
  | `Record of record
  | `Track of string
]

module Item : sig
  type t = item
  val parse : Line.t -> item
end

include Line_oriented.S with type item := item
