open Core
open Biocaml_base

module Xls = struct

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

  let header_prefix =
    "chr\tstart\tend\tlength\tsummit\ttags\t-10*log10(pvalue)\tfold_enrichment"
  (* the header changes if there is a control, there is an additional FDR column *)

  let parse line =
    let fail () =
      failwithf "Macs.Xls.parse failed: %s" (line : Line.t :> string) ()
    in
    match (line : Line.t :> string) with
    | "" -> `Comment ""
    | line when String.is_prefix line ~prefix:header_prefix ->
      `Header
    | line ->
      if Char.equal line.[0] '#' then `Comment (String.slice line 1 0)
      else (
        match String.split ~on:'\t' line with
        | seqid :: pos_start :: pos_end :: length ::
          summit :: tags :: pvalue :: fold :: maybe_fdr ->
          let fdr = match maybe_fdr with
            | [] -> None
            | [ fdr ] -> Some (Float.of_string fdr)
            | _ -> fail ()
          in
          `Record {
            seqid ;
            pos_start = Int.of_string pos_start ;
            pos_end = Int.of_string pos_end ;
            length = Int.of_string length ;
            summit = Int.of_string summit ;
            tags = Int.of_string tags ;
            pvalue = Float.of_string pvalue ;
            fold = Float.of_string fold ;
            fdr
          }
        | _ -> fail ()
      )

  let unparse = function
    | `Comment "" -> ""
    | `Comment c -> sprintf "#%s\n" c
    | `Header -> header_prefix ^ "\n" (* FIXME: should add FDR column header if told to do so *)
    | `Record r ->
      sprintf "%s\n" (
        String.concat ~sep:"\t" (
          r.seqid :: sprintf "%d" r.pos_start :: sprintf "%d" r.pos_end ::
          sprintf "%d" r.length :: sprintf "%d" r.summit ::
          sprintf "%d" r.tags :: sprintf "%g" r.pvalue ::
          sprintf "%g" r.fold ::
          Option.value_map r.fdr ~default:[] ~f:(fun x -> [ sprintf "%g" x ])
        )
      )
    | _ -> assert false

  (* let summits_to_bed5 = function
   *   | `Comment c -> `Comment c
   *   | `Header -> `Comment ""
   *   | `Record r ->
   *     `Record { chrom = r.seqid ;
   *               chromStart = r.pos_start + r.summit - 1 ;
   *               chromEnd = r.pos_start + r.summit ;
   *               Bed.Bed5.name =
   *                 r.seqid ^ "." ^ Int.to_string (r.pos_start + r.summit - 1)
   *                 ^ "." ^ Int.to_string (r.pos_start + r.summit) ;
   *               score = r.pvalue } *)

  (* let to_bed5 = function
   *   | `Comment c -> `Comment c
   *   | `Header -> `Comment ""
   *   | `Record r ->
   *     `Record { chrom = r.seqid ; chromStart = r.pos_start ;
   *               chromEnd = r.pos_end ;
   *               Bed.Bed5.name = r.seqid ^ "." ^ Int.to_string r.pos_start
   *                               ^ "." ^ Int.to_string r.pos_end ;
   *               score = r.pvalue } *)

end
