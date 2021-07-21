open Core_kernel

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

let parse_pValue x = match Float.of_string x with
  | -1. -> None
  | x -> Some x

let parse_peak x = match Int.of_string x with
  | -1 -> None
  | x -> Some x

module Item = struct
  type t = [
    | `Comment of string
    | `Record of record
    | `Track of string
  ]

  let parse line =
    match (line : Biocaml_base.Line.t :> string) with
    | "" -> `Comment ""
    | line ->
      if Char.(line.[0] = '#') then
        `Comment (String.slice line 1 0)
      else if String.length line >= 6 && String.is_prefix line ~prefix:"track " then
        `Track (String.slice line 5 0)
      else (
        match String.split ~on:'\t' line with
        | [ chrom ; chromStart ; chromEnd ; name ; score ; strand ; signalValue ; pValue ; qValue ; peak ] ->
          (
            try
              `Record {
                chrom ;
                chromStart = Int.of_string chromStart ;
                chromEnd = Int.of_string chromEnd ;
                name ;
                score = Int.of_string score ;
                strand = (
                  match Bed.parse_strand strand with
                  | Ok s -> s
                  | Error _ -> failwith ""
                ) ;
                signalValue = Float.of_string signalValue ;
                pValue = parse_pValue pValue ;
                qValue = parse_pValue qValue ;
                peak = parse_peak peak ;
              }
            with _ -> failwith line
          )
        | _ -> failwith line
      )

  let unparse = function
    | `Comment s -> "#" ^ s
    | `Track s -> "track " ^ s
    | `Record r ->
      String.concat ~sep:"\t" [
        r.chrom ;
        Int.to_string r.chromStart ;
        Int.to_string r.chromEnd ;
        r.name ;
        Int.to_string r.score ;
        Bed.unparse_strand r.strand ;
        Float.to_string r.signalValue ;
        Float.to_string (Option.value ~default:(-1.) r.pValue) ;
        Float.to_string (Option.value ~default:(-1.) r.qValue) ;
        Int.to_string (Option.value ~default:(-1) r.peak) ;
      ]
end

include Line_oriented.Make(Item)
