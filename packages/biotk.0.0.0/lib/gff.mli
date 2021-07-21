open Biocaml_base

module Record : sig
  type t = Gff.record = {
    seqname    : string ;
    source     : string option ;
    feature    : string option ;
    start_pos  : int ;
    stop_pos   : int ;
    score      : float option ;
    strand     : [`Plus | `Minus | `Not_stranded | `Unknown ] ;
    phase      : int option ;
    attributes : (string * string list) list ;
  }
  [@@deriving sexp]

  val loc : t -> GLoc.t
  val length : t -> int
  val attribute_exn : t -> string -> string
end

module Item : sig
  type t = Gff.item
  include Line_oriented.Item with type t := t
  val to_record : t -> Gff.record option
end

include Line_oriented.S with type item = Item.t
