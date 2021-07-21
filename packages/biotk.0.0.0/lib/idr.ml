open Core_kernel

type fields = string list
[@@deriving show]

let from_file from_fields fn =
  try
    Ok (
      In_channel.read_lines fn
      |> List.map ~f:(fun l ->
          String.split l ~on:'\t'
          |> from_fields
        )
    )
  with exn -> Error (`Msg (Exn.to_string exn))

module Maybe_strand = struct
  type t = [`Plus | `Minus] option

  let of_string = function
    | "+" -> Some `Plus
    | "-" -> Some `Minus
    | "." -> None
    | _ -> failwith "Incorrect syntax for strand field"
end

module Narrow_output = struct
  module Row = struct
    type t = {
      chrom : string ;
      chromStart : int ;
      chromEnd : int ;
      name : string ;
      score : int ;
      strand : Maybe_strand.t ;
      signalValue : float ;
      pvalue : float ;
      qvalue : float ;
      summit : int ;
      localIDR : float ;
      globalIDR : float ;
    }

    let from_fields = function
      | chrom :: chromStart :: chromEnd :: name :: score :: strand :: signalValue ::
        pvalue :: qvalue :: summit :: localIDR :: globalIDR :: _ ->
        { chrom ;
          chromStart = Int.of_string chromStart ;
          chromEnd = Int.of_string chromEnd ;
          name ;
          score = Int.of_string score ;
          strand = Maybe_strand.of_string strand ;
          signalValue = Float.of_string signalValue ;
          pvalue = Float.of_string pvalue ;
          qvalue = Float.of_string qvalue ;
          summit = Int.of_string summit ;
          localIDR = Float.of_string localIDR ;
          globalIDR = Float.of_string globalIDR ;
        }
      | l -> failwithf "Expected more fields, got %s" (show_fields l) ()

    let loc r = GLoc.{ chr = r.chrom ; lo = r.chromStart ; hi = r.chromEnd }
  end

  let from_file = from_file Row.from_fields
end

module Broad_output = struct
  module Row = struct
    type t = {
      chrom : string ;
      chromStart : int ;
      chromEnd : int ;
      name : string ;
      score : int ;
      strand : Maybe_strand.t ;
      signalValue : float ;
      pvalue : float ;
      qvalue : float ;
      localIDR : float ;
      globalIDR : float ;
    }

    let from_fields = function
      | chrom :: chromStart :: chromEnd :: name :: score :: strand :: signalValue ::
        pvalue :: qvalue :: localIDR :: globalIDR :: _ ->
        { chrom ;
          chromStart = Int.of_string chromStart ;
          chromEnd = Int.of_string chromEnd ;
          name ;
          score = Int.of_string score ;
          strand = Maybe_strand.of_string strand ;
          signalValue = Float.of_string signalValue ;
          pvalue = Float.of_string pvalue ;
          qvalue = Float.of_string qvalue ;
          localIDR = Float.of_string localIDR ;
          globalIDR = Float.of_string globalIDR ;
        }
      | l -> failwithf "Expected more fields, got %s" (show_fields l) ()

    let loc r = GLoc.{ chr = r.chrom ; lo = r.chromStart ; hi = r.chromEnd }
  end

  let from_file = from_file Row.from_fields
end
