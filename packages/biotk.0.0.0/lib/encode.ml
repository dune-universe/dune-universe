open Core_kernel

module Experiment_list = struct
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

  let data_type_of_string = function
    | "5C" -> `_5C
    | "CAGE" -> `CAGE
    | "ChIA-PET" -> `ChIA_PET
    | "ChIP-seq" -> `ChIP_seq
    | "Combined" -> `Combined
    | "DNA-PET" -> `DNA_PET
    | "DNase-DGF" -> `DNase_DGF
    | "DNase-seq" -> `DNase_seq
    | "Exon Array" -> `Exon_array
    | "FAIRE-seq" -> `FAIRE_seq
    | "GENCODE" -> `GENCODE
    | "Genotype" -> `Genotype
    | x -> failwith ("Encode.data_type_of_string: unknown data type ``" ^ x ^ "''")

  let experimental_factors_of_string _ = assert false

  let item_of_line i = function
    | [ data_type ; cell_type ; experimental_factors ; treatment ; lab ; pi ; assembly ; status ; geo_accession ; dcc_accession ; data_unrestricted ] ->
      {
        data_type = data_type_of_string data_type ;
        cell_type ;
        experimental_factors = experimental_factors_of_string experimental_factors ;
        treatment ;
        lab ;
        pi ;
        assembly ;
        status ;
        geo_accession ;
        dcc_accession ;
        data_unrestricted ;
      }
    | l -> raise (Parse_error (String.concat ~sep:"\t" l, i))

  let parse_exn fn =
    In_channel.read_lines fn
    |> List.tl_exn (* skip header *)
    |> List.map ~f:(String.split ~on:'\t')
    |> List.mapi ~f:item_of_line

  let parse fn =
    try `Ok (parse_exn fn)
    with Parse_error (l,lno) -> `Error (l,lno)

  let human_url = "https://spreadsheets.google.com/pub?key=0AvQL5qBL6AfEdFJqaU16U3JjT2hUX0JjeFFKVk56QlE&hl=en&output=csv"
  let mouse_url = "https://spreadsheets.google.com/pub?key=0AvQL5qBL6AfEdEJKd3RmYl9tbkc0d04wZDdiXzRrbmc&output=csv"
end
