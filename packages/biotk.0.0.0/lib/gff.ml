open Core
open Biocaml_base

module Record = struct
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

  let loc r = GLoc.{ chr = r.seqname ; lo = r.start_pos ; hi = r.stop_pos }
  let length r = r.stop_pos - r.start_pos + 1
  let attribute_exn r k =
    List.Assoc.find_exn ~equal:String.equal r.attributes k
    |> List.hd_exn
end

module Item = struct
  type t = Gff.item

  let parse line =
    match Gff.gff3_item_of_line line with
    | Ok item -> item
    | Error (`Msg msg) -> failwith msg

  let unparse item =
    (Gff.line_of_item `three item :> string)

  let to_record = function
    | `Record r -> Some r
    | `Comment _ -> None
end

include Line_oriented.Make(Item)
