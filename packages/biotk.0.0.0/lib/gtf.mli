open Core_kernel
open Biocaml_base

module Item : sig
  type t = Gff.item
  include Line_oriented.Item with type t := t
  val to_record : t -> Gff.record option
end

module Record : sig
  type t = Gff.record
  val loc : t -> GLoc.t
  val compare : t -> t -> int
end

include Line_oriented.S with type item = Item.t

module Annotation : sig
  type t

  val of_items :
    ?gene_id_label:string ->
    ?transcript_id_label:string ->
    Item.t list ->
    t

  val genes : t -> Gene.t String.Table.t * (string * Error.t) list
  val utr3' : t -> Record.t String.Table.t
  val utr5' : t -> Record.t String.Table.t
end
