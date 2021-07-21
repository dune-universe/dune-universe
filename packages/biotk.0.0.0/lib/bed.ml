open Core
open Biocaml_base
open Printf

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

let parse_strand = function
  | "." -> Ok `Not_relevant
  | "?" -> Ok `Unknown
  | "+" -> Ok `Plus
  | "-" -> Ok `Minus
  | s -> Error s


let unparse_strand = function
  | `Not_relevant -> "."
  | `Unknown -> "?"
  | `Plus -> "+"
  | `Minus -> "-"

let parse_item f line =
  match (line : Line.t :> string) with
  | "" -> `Comment ""
  | line ->
    if Char.(line.[0] = '#')
    then `Comment (String.slice line 1 0)
    else
      let fields = String.split ~on:'\t' line in
      `Record (f fields)

let unparse_item f = function
  | `Comment c -> sprintf "#%s" c
  | `Record r -> String.concat ~sep:"\t" (f r)

module type Base = sig
  type t
  val loc : t -> GLoc.t
  val from_fields : string list -> t
  val to_fields : t -> string list
end

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

(* this trick is necessary because otherwise Record cannot be
   extended (it should be possible in 4.08) *)
module Make'(T : Base) = struct
  module Internal_record = struct
    type t = T.t
    let loc = T.loc
    let of_line line =
      line
      |> Line.split ~on:'\t'
      |> T.from_fields
    let to_line r =
      T.to_fields r
      |> String.concat ~sep:"\t"
  end

  module Item = struct
    (* type t = T.t
     * let loc = T.loc *)
    let of_line = parse_item T.from_fields
    let to_line = unparse_item T.to_fields
  end

  let load fn =
    In_channel.read_lines fn
    |> List.map ~f:(fun l -> Item.of_line (Line.of_string_unsafe l))

  let save bed fn =
    Out_channel.with_file fn ~f:(fun oc ->
        List.iter bed ~f:(fun item ->
            Out_channel.output_string oc (Item.to_line item) ;
            Out_channel.output_char oc '\n'
          )
      )

  let load_records fn =
    load fn
    |> List.filter_map ~f:(function
        | `Comment _ -> None
        | `Record r -> Some r
      )

  let save_records rs fn =
    save (List.map rs ~f:(fun r -> `Record r)) fn

  let load_as_lmap fn = (* FIXME: could use stream to read bed file *)
    load fn
    |> Stream.of_list
    |> CFStream.Stream.filter_map ~f:(function
        | `Comment _ -> None
        | `Record x -> Some (T.loc x, x)
      )
    |> GAnnot.LMap.of_stream

end

module Make(T : Base) = struct
  include Make'(T)
  module Record = Internal_record
end

type fields = string list
[@@deriving show]

module Bed3 = struct
  type record = {
    chrom : string ;
    chromStart : int ;
    chromEnd : int ;
  }
  module Base = struct
    type t = record

    let loc r = GLoc.{ chr = r.chrom ; lo = r.chromStart ; hi = r.chromEnd }

    let from_fields = function
      | chrom :: chromStart :: chromEnd :: _ ->
        { chrom ;
          chromStart = Int.of_string chromStart ;
          chromEnd = Int.of_string chromEnd ;
        }
      | l -> failwithf "Expected more fields, got %s" (show_fields l) ()

    let to_fields r = [
      r.chrom ; sprintf "%d" r.chromStart ; sprintf "%d" r.chromEnd
    ]
  end
  include Make'(Base)
  module Record = struct
    include Internal_record
    let of_loc l = {
      chrom = l.GLoc.chr ;
      chromStart = l.lo ;
      chromEnd = l.hi ;
    }
  end

end

module Bed4 = struct
  type record = {
    chrom : string ;
    chromStart : int ;
    chromEnd : int ;
    name : string ;
  }
  module Base = struct
    type t = record
    let loc r = GLoc.{ chr = r.chrom ; lo = r.chromStart ; hi = r.chromEnd }

    let from_fields = function
      | chrom :: chromStart :: chromEnd :: name :: _ ->
        { chrom ;
          chromStart = Int.of_string chromStart ;
          chromEnd = Int.of_string chromEnd ;
          name }
      | l -> failwithf "Expected more fields, got %s" (show_fields l) ()

    let to_fields r = [
      r.chrom ; sprintf "%d" r.chromStart ; sprintf "%d" r.chromEnd ; r.name
    ]
  end
  include Make(Base)
end

module Bed5 = struct
  type record = {
    chrom : string ;
    chromStart : int ;
    chromEnd : int ;
    name : string ;
    score : int ;
  }
  module Base = struct
    type t = record
    let loc r = GLoc.{ chr = r.chrom ; lo = r.chromStart ; hi = r.chromEnd }

    let from_fields = function
      | chrom :: chromStart :: chromEnd :: name :: score :: _ ->
        { chrom ;
          chromStart = Int.of_string chromStart ;
          chromEnd = Int.of_string chromEnd ;
          name ; score = Int.of_string score }
      | l -> failwithf "Expected more fields, got %s" (show_fields l) ()

    let to_fields r = [
      r.chrom ; sprintf "%d" r.chromStart ; sprintf "%d" r.chromEnd ;
      r.name ; sprintf "%d" r.score
    ]
  end
  include Make'(Base)

  module Record = struct
    include Internal_record
    let to_bed4 = function
      | `Comment c -> `Comment c
      | `Record r ->
        `Record { Bed4.chrom = r.chrom ; chromStart = r.chromStart ;
                  chromEnd = r.chromEnd ; name = r.name }
  end
end

module Bed6 = struct
  type record = {
    chrom : string ;
    chromStart : int ;
    chromEnd : int ;
    name : string ;
    score : int ;
    strand : strand ;
  }
  module Base = struct
    type t = record
    let loc r = GLoc.{ chr = r.chrom ; lo = r.chromStart ; hi = r.chromEnd }

    let from_fields = function
      | chrom :: chromStart :: chromEnd :: name :: score :: strand :: _ ->
        { chrom ;
          chromStart = Int.of_string chromStart ;
          chromEnd = Int.of_string chromEnd ;
          name ;
          score = Int.of_string score ;
          strand = (
            match parse_strand strand with
            | Ok s -> s
            | Error msg -> failwith msg
          ) ;
        }
      | l -> failwithf "Expected more fields, got %s" (show_fields l) ()

    let to_fields r = [
      r.chrom ; sprintf "%d" r.chromStart ; sprintf "%d" r.chromEnd ;
      r.name ; sprintf "%d" r.score ;
      (match r.strand with
       | `Not_relevant -> "."
       | `Unknown -> "?"
       | `Plus -> "+"
       | `Minus -> "-" )
    ]
  end
  include Make(Base)
end

type record = GLoc.t * fields
module Base = struct
  type t = record

  let loc = fst
  let from_fields xs = Bed3.Base.(from_fields xs |> loc), xs
  let to_fields = snd
end
include Base
include Make(Base)
