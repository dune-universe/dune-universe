open Biocaml_base

type 'a item = [
  | `Comment of string
  | `Record of 'a
]

type strand = [
  | `Plus
  | `Minus
  | `Not_relevant
  | `Unknown
]
val parse_strand : string -> (strand, string) result
val unparse_strand : strand -> string

module type Record = sig
  type t
  val loc : t -> GLoc.t
  val of_line : Line.t -> t
  val to_line : t -> string
end

module type S = sig
  type record
  val load : string -> record item list
  val load_records : string -> record list
  val load_as_lmap : string -> record GAnnot.LMap.t
  val save : record item list -> string -> unit
  val save_records : record list -> string -> unit
end

module Record : Record
include S with type record := Record.t

module Bed3 : sig
  type record = {
    chrom : string ;
    chromStart : int ;
    chromEnd : int ;
  }
  module Record : sig
    include Record with type t = record
    val of_loc : GLoc.t -> t
  end
  include S with type record := Record.t
end


module Bed4 : sig
  type record = {
    chrom : string ;
    chromStart : int ;
    chromEnd : int ;
    name : string ;
  }
  module Record : Record with type t = record
  include S with type record := Record.t
end

module Bed5 : sig
  type record = {
    chrom : string ;
    chromStart : int ;
    chromEnd : int ;
    name : string ;
    score : int ;
  }
  module Record : sig
    include Record with type t = record
    val to_bed4 : t item -> Bed4.record item
  end
  include S with type record := Record.t
end

module Bed6 : sig
  type record = {
    chrom : string ;
    chromStart : int ;
    chromEnd : int ;
    name : string ;
    score : int ;
    strand : strand ;
  }
  module Record : Record with type t = record
  include S with type record := Record.t
end
