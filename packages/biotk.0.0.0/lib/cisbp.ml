open Core_kernel

let pwm_archive_url = "http://cisbp.ccbr.utoronto.ca/data/1.02/DataFiles/Bulk_downloads/EntireDataset/PWMs.zip"
let tf_information_archive_url = "http://cisbp.ccbr.utoronto.ca/data/1.02/DataFiles/Bulk_downloads/EntireDataset/TF_Information_all_motifs.txt.zip"

module TF_information = struct
  type item = {
    tf_id : string ;
    family_id : string ;
    tsource_id : string ;
    motif_id : string option ;
    msource_id : string option ;
    dbid : string ;
    tf_name : string ;
    tf_species : string ;
    tf_status : string ;
    family_name : string ;
    dbds : string list ;
    dbd_count : int ;
    cutoff : float ;
    dbid2 : string option ;
    motif_type : string option ;
    msource_identifier : string option ;
    msource_type : string option ;
    msource_author : string option ;
    msource_year : int option ;
    pmid : string option ;
    msource_version : string option ;
    tfsource_name : string ;
    tfsource_url : string ;
    tfsource_year : int ;
    tfsource_month : string ;
    tfsource_day : int ;
  }
  type t = item list

  let opt f = function
    | "." -> None
    | s -> Some (f s)

  let string x = x

  let int x = Int.of_string x
  let float x = Float.of_string x
  let list f x =
    String.split ~on:',' x
    |> List.map ~f

  let parse_fields = function
    | [ tf_id ;
        family_id ;
        tsource_id ;
        motif_id ;
        msource_id ;
        dbid ;
        tf_name ;
        tf_species ;
        tf_status ;
        family_name ;
        dbds ;
        dbd_count ;
        cutoff ;
        dbid2 ;
        motif_type ;
        msource_identifier ;
        msource_type ;
        msource_author ;
        msource_year ;
        pmid ;
        msource_version ;
        tfsource_name ;
        tfsource_url ;
        tfsource_year ;
        tfsource_month ;
        tfsource_day ;
      ] -> {
        tf_id ;
        family_id ;
        tsource_id ;
        motif_id = opt string motif_id ;
        msource_id = opt string msource_id ;
        dbid ;
        tf_name ;
        tf_species ;
        tf_status ;
        family_name ;
        dbds = list string dbds ;
        dbd_count = int dbd_count ;
        cutoff = float cutoff ;
        dbid2 = opt string dbid2 ;
        motif_type = opt string motif_type ;
        msource_identifier = opt string msource_identifier ;
        msource_type = opt string msource_type ;
        msource_author = opt string msource_author ;
        msource_year = opt int msource_year ;
        pmid = opt string pmid ;
        msource_version = opt string msource_version ;
        tfsource_name ;
        tfsource_url ;
        tfsource_year = int tfsource_year ;
        tfsource_month ;
        tfsource_day = int tfsource_day ;
      }
    | fields -> failwithf "incorrect line format: %s" (String.concat ~sep:"\t" fields) ()

  let from_file fn =
    In_channel.read_all fn
    |> String.split ~on:'\n'
    |> Fn.flip List.drop 1
    |> List.filter ~f:(String.( <> ) "")
    |> List.map ~f:(String.split ~on:'\t')
    |> List.map ~f:parse_fields
end

module Motif = struct
  type t = float array array

  let parse_line l =
    let f = Float.of_string in
    match String.split l ~on:'\t' with
    | [ _ ; a ; c ; g ; t ] ->
      [| f a ; f c ; f g ; f t |]
    | _ ->
      failwithf "Cis_bp.Motif.parse_line: unable to parse %s" l ()

  let from_file fn =
    In_channel.read_lines fn
    |> Fn.flip List.drop 1
    |> List.map ~f:parse_line
    |> Array.of_list

  let read_all_in_dir dir =
    Sys.readdir dir
    |> Array.to_list
    |> List.filter ~f:(Fn.flip Filename.check_suffix ".txt")
    |> List.map ~f:(fun fn -> fn, from_file (Filename.concat dir fn))


  let pwm m =
    let bg = Pwm.flat_background () in
    Array.map m ~f:(fun pos ->
        Array.map pos ~f:(fun p -> Float.to_int (p *. 100.))
      )
    |> Fn.flip Pwm.make bg
end
