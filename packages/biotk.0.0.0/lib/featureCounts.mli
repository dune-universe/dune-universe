module Tsv : sig
  type t = {
    id : string ;
    chr : string ;
    lo : int ;
    hi : int ;
    strand : string ;
    length : int ;
    count : int ;
  } [@@deriving fields, csv]

  val load : string -> t list
  val loc : t -> GLoc.t
end

module Summary : sig
  type t = {
    assigned : int ;
    unassigned_unmapped : int ;
    unassigned_mappingquality : int ;
    unassigned_chimera : int ;
    unassigned_fragmentlength : int ;
    unassigned_duplicate : int ;
    unassigned_multimapping : int ;
    unassigned_secondary : int ;
    unassigned_nonjunction : int ;
    unassigned_nofeatures : int ;
    unassigned_overlapping_length : int ;
    unassigned_ambiguity : int ;
  }
  val load : string -> t
end
