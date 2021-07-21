(**
   https://github.com/taoliu/MACS/blob/macs_v1/README.rst
*)

open Biocaml_base

module Xls : sig

  type entry = {
      seqid : string ;
      pos_start : int ;
      pos_end : int ;
      length : int ;
      summit : int ;
      tags : int ;
      pvalue : float ;
      fold : float ;
      fdr : float option ;
  }

  type item = [
    | `Comment of string
    | `Record of entry
    | `Header
  ]

  val parse : Line.t -> item
  val unparse : item -> string
  (* val summits_to_bed5 : t item -> Bed.Bed5.t Bed.item
   * val to_bed5 : t item -> Bed.Bed5.t Bed.item *)
end
