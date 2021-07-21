module Experiment_list : sig
  type t = item list
  and item = {
    data_type : [  | `CAGE | `ChIA_PET | `ChIP_seq
                   | `Combined | `DNA_PET | `DNase_DGF
                   | `DNase_seq | `Exon_array | `FAIRE_seq
                   | `GENCODE | `Genotype | `_5C ] ;
    cell_type : string ;
    experimental_factors : (string * string) list ;
    treatment : string ;
    lab : string ;
    pi : string ;
    assembly : string ;
    status : string ;
    geo_accession : string ;
    dcc_accession : string ;
    data_unrestricted : string
  }

  exception Parse_error of string * int

  val parse : string -> [ `Ok of t | `Error of string * int ]
  val parse_exn : string -> t

  val human_url : string
  val mouse_url : string
end
