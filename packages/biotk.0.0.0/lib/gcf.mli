(** GCF = Genomic coordinates format loosely defined as a TSV format
    whose first column represents a location with the syntax <chr>:<start>-<end>
*)

type row = GLoc.t * string list

val to_bed :
  ?open_end:bool ->
  string ->
  string ->
  unit
