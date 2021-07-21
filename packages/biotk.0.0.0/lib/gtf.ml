open Core
open CFStream
open Biocaml_base

(* https://github.com/The-Sequence-Ontology/Specifications/blob/master/gff3.md *)

type record = Biocaml_base.Gff.record = {
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
[@@ deriving sexp]

(* http://gmod.org/wiki/GFF2 *)
module Parser = struct
  open Angstrom

  let field name =
    take_while1 (function
        | '\t' | '\n' -> false
        | _ -> true
      )
    <?> ("field:" ^ name)

  let to_int fieldnum s =
    match Int.of_string s with
    | n -> return n
    | exception _ ->
      fail (sprintf "failed to convert %s to integer at field %d" s fieldnum)

  let to_float fieldnum s =
    match Float.of_string s with
    | x -> return x
    | exception _ ->
      fail (sprintf "failed to convert %s to float at field %d" s fieldnum)

  let option = function
    | "." -> None
    | s -> Some s

  let maybe_convert f = function
    | "." -> return None
    | s -> f s >>| Option.some

  let strand fieldnum = function
    | "." -> return `Not_stranded
    | "?" -> return `Unknown
    | "+" -> return `Plus
    | "-" -> return `Minus
    | _ -> fail (sprintf "Incorrect strand character at field %d" fieldnum)

  let attribute_id = take_while1 (function
      | 'a'..'z' | 'A'..'Z'
      | '_' -> true
      | _ -> false
    )

  let quoted_attribute_value =
    char '"' *> take_while (function
        | '"' | '\n' -> false
        | _ -> true
      ) <*
    char '"'

  let unquoted_attribute_value =
    take_while (function
        | ' ' | ';' | '\n' -> false
        | _ -> true
      )

  let one_gtf_attributes =
    attribute_id >>= fun id ->
    skip_many1 (char ' ') >>= fun () ->
    (quoted_attribute_value <|> unquoted_attribute_value) >>= fun s ->
    return (id, [ s ])

  let attribute_separator =
    skip_many (char ' ') >>= fun () ->
    char ';' >>= fun _ ->
    skip_many (char ' ')

  let gtf_attributes =
    sep_by1 attribute_separator one_gtf_attributes
    <* Angstrom.option ';' (char ';')
    <?> "gtf_attributes"

  let tab = char '\t'

  let record =
    field "seqname" >>= fun seqname ->
    tab *>
    field "source" >>| option >>= fun source ->
    tab *>
    field "feature" >>| option >>= fun feature ->
    tab *>
    field "start_pos" >>= to_int 4 >>= fun start_pos ->
    tab *>
    field "stop_pos" >>= to_int 5 >>= fun stop_pos ->
    tab *>
    field "score" >>= maybe_convert (to_float 6) >>= fun score ->
    tab *>
    field "strand" >>= strand 7 >>= fun strand ->
    tab *>
    field "phase" >>= maybe_convert (to_int 8) >>= fun phase ->
    tab *>
    gtf_attributes >>| fun attributes ->
    `Record Gff.{
      seqname ;
      source ;
      feature ;
      start_pos ;
      stop_pos ;
      score ;
      strand ;
      phase ;
      attributes ;
    }
  let record = record <?> "record"

  let comment =
    char '#' >>= fun _ ->
    take_while (Char.( <> ) '\n') >>| fun s ->
    `Comment s

  let space =
    skip_while (function
        | ' ' | '\t' -> true
        | _ -> false
      )

  let file =
    let rec line_start acc lno =
      (end_of_input *> return acc)
      <|> (comment >>= fun c -> after_comment (c :: acc) lno )
      <|> (record >>= fun r -> after_record (r :: acc) lno)
    and after_comment acc lno =
      (end_of_line >>= fun () -> line_start acc (lno + 1))
      <|> (end_of_input *> return acc)
    and after_record acc lno =
      (end_of_input *> return acc)
      <|> (space *> end_of_line >>= fun () -> line_start acc (lno + 1))
      <|> (space *> comment >>= fun c -> after_comment (c :: acc) lno)
    in
    line_start [] 1
    >>| List.rev

  let test p s v =
    match parse_string p s ~consume:All with
    | Ok x -> Poly.(x = v)
    | Error msg ->
      print_endline msg ;
      false

  let%test "Gtf.gtf_attributes1" =
    test
      gtf_attributes
      {|gene_id "FBgn0031081"|}
      [ ("gene_id", ["FBgn0031081"]) ]

  let%test "Gtf.gtf_attributes2" =
    test
      gtf_attributes
      {|gene_id "FBgn0031081"; gene_symbol "Nep3"; transcript_id "FBtr0070000"; transcript_symbol "Nep3-RA";|}
      [ ("gene_id", ["FBgn0031081"]) ; ("gene_symbol", ["Nep3"]) ;
        ("transcript_id", ["FBtr0070000"]) ; ("transcript_symbol", ["Nep3-RA"]) ]

  let%test "Gtf.gtf_attributes3" =
    test
      gtf_attributes
      {|Transcript B0273.1; Note "Zn-Finger"|}
      [ "Transcript", ["B0273.1"] ; "Note", ["Zn-Finger"]]

  let%test "Gtf.comment" =
    test comment "#comment" (`Comment "comment")

  type item = [ `Comment of string | `Record of record ]
  [@@ deriving sexp]

  let expect_record s =
    parse_string ~consume:All record s
    |> [%sexp_of: ([`Record of record], string) Result.t]
    |> Sexp.output_hum Stdio.stdout

  let expect s =
    parse_string ~consume:All file s
    |> [%sexp_of: (item list, string) Result.t]
    |> Sexp.output_hum Stdio.stdout

  let ex1 = {|IV	curated	mRNA	5506800	5508917	.	+	.	Transcript B0273.1; Note "Zn-Finger";|}
  let%expect_test "parse GTF record" =
    expect_record ex1;
    [%expect {|
      (Ok
       (Record
        ((seqname IV) (source (curated)) (feature (mRNA)) (start_pos 5506800)
         (stop_pos 5508917) (score ()) (strand Plus) (phase ())
         (attributes ((Transcript (B0273.1)) (Note (Zn-Finger))))))) |}]

  let ex2 = {|X	FlyBase	gene	19961297	19969323	.	+	.	gene_id "FBgn0031081"; gene_symbol "Nep3";|}
  let%expect_test "parse GTF record 2" =
    expect_record ex2;
    [%expect {|
      (Ok
       (Record
        ((seqname X) (source (FlyBase)) (feature (gene)) (start_pos 19961297)
         (stop_pos 19969323) (score ()) (strand Plus) (phase ())
         (attributes ((gene_id (FBgn0031081)) (gene_symbol (Nep3))))))) |}]

  let ex2 = {|IV	curated	mRNA	5506800	5508917	.	+	.	Transcript B0273.1; Note "Zn-Finger" # some comment
IV	curated	mRNA	5506800	5508917	.	+	.	Transcript B0273.1; Note "Zn-Finger"
X	FlyBase	gene	19961297	19969323	.	+	.	gene_id "FBgn0031081"; gene_symbol "Nep3";|}

  let%expect_test "parse GTF file" =
    expect ex2;
    [%expect {|
      (Ok
       ((Record
         ((seqname IV) (source (curated)) (feature (mRNA)) (start_pos 5506800)
          (stop_pos 5508917) (score ()) (strand Plus) (phase ())
          (attributes ((Transcript (B0273.1)) (Note (Zn-Finger))))))
        (Comment " some comment")
        (Record
         ((seqname IV) (source (curated)) (feature (mRNA)) (start_pos 5506800)
          (stop_pos 5508917) (score ()) (strand Plus) (phase ())
          (attributes ((Transcript (B0273.1)) (Note (Zn-Finger))))))
        (Record
         ((seqname X) (source (FlyBase)) (feature (gene)) (start_pos 19961297)
          (stop_pos 19969323) (score ()) (strand Plus) (phase ())
          (attributes ((gene_id (FBgn0031081)) (gene_symbol (Nep3)))))))) |}]
end

module Item = struct
  type t = Gff.item

  let parse line =
    match Gff.gtf_item_of_line line with
    | Ok item -> item
    | Error (`Msg msg) -> failwith msg

  let unparse item =
    (Gff.line_of_item `two item :> string)

  let to_record = function
    | `Record r -> Some r
    | `Comment _ -> None
end

include Line_oriented.Make(Item)

(* Introduced a new load function because it so happens that comment
   items may follow records in gtf files, which is incompatible with
   the assumptions in Line_oriented. *)
let load fn =
  let _unconsumed, result =
    In_channel.with_file fn ~f:(fun ic ->
        Angstrom_unix.parse Parser.file ic
      )
  in
  match result with
  | Ok xs -> xs
  | Error msg -> failwith msg

module Record = struct
  type t = Gff.record
  let loc r = GLoc.{ chr = r.Gff.seqname ; lo = r.start_pos ; hi = r.stop_pos }
  let compare p q =
    match GLoc.compare (loc p) (loc q) with
    | 0 -> Option.compare String.compare p.feature q.feature
    | c -> c
end

module Annotation = struct
  type t = {
    gene_id_label : string ;
    transcript_id_label : string ;
    items : Gff.record list String.Table.t ;
  }

  let of_items ?(gene_id_label = "gene_id") ?(transcript_id_label = "transcript_id") items =
    let items =
      CFStream.Stream.of_list items
      |> CFStream.Stream.filter_map ~f:(function
          | `Comment _ -> None
          | `Record r ->
            match List.Assoc.find r.Biocaml_base.Gff.attributes gene_id_label ~equal:String.equal with
            | Some (id :: _) -> Some (id, r)
            | Some []
            | None -> None
        )
      |> Biocaml_unix.Accu.relation
      |> CFStream.Stream.map ~f:(fun (x, y) -> x, List.rev y)
      |> CFStream.Stream.to_list
      |> String.Table.of_alist_exn
    in
    { gene_id_label ; transcript_id_label ; items }

  let%test_unit "Annotation.of_items tail rec" =
    List.init 1_000_000 ~f:(fun _ -> `Comment "")
    |> of_items
    |> ignore

  let strand_of_entry items =
    let strands =
      List.map items ~f:(fun i -> i.Gff.strand)
      |> List.dedup_and_sort ~compare:Poly.compare
    in
    match strands with
    | [] -> raise (Invalid_argument "strand_of_entry")
    | [ s ] -> Ok s
    | _ -> Or_error.error_string "more than one strand in entry"

  let exons_of_entry items =
    List.filter_map items ~f:(fun r ->
        match r.Gff.feature with
        | Some "exon" -> Some r
        | _ -> None
      )

  let sort_by_attribute ~attr_label items =
    Stream.of_list items
    |> Stream.filter_map ~f:(fun r ->
        match List.Assoc.find r.Biocaml_base.Gff.attributes attr_label ~equal:String.equal with
          | Some (id :: _) -> Some (id, r)
          | Some []
          | None -> None
      )
    |> Biocaml_unix.Accu.relation
    |> Stream.to_list

  let gene_of_entry ~id ~transcript_id_label items =
    match exons_of_entry items with
    | [] -> Ok None
    | exons ->
      let open Or_error.Monad_infix in
      strand_of_entry exons |> Or_error.tag ~tag:id >>= function
      | `Unknown | `Not_stranded -> Or_error.error_string "no defined strand"
      | `Plus | `Minus as strand ->
        let transcripts =
          sort_by_attribute ~attr_label:transcript_id_label exons
          |> List.map ~f:(fun (s, items) -> s, List.map ~f:Record.loc items)
        in
        Gene.make ~strand ~id transcripts
        |> Or_error.map ~f:Option.some

  let genes annot =
    let r = String.Table.create () in
    let errors =
      String.Table.fold annot.items ~init:[] ~f:(fun ~key:id ~data:items errors ->
          match gene_of_entry ~id ~transcript_id_label:annot.transcript_id_label items with
          | Ok (Some g) ->
            String.Table.set r ~key:id ~data:g ;
            errors
          | Ok None -> errors
          | Error e -> (id, e) :: errors
        )
    in
    r, errors

  let utr5' annot =
    String.Table.filter_map annot.items ~f:(fun items ->
        List.find_map items ~f:(fun r ->
            match r.Gff.feature with
            | Some "5UTR" -> Some r
            | _ -> None
          )
      )

  let utr3' annot =
    String.Table.filter_map annot.items ~f:(fun items ->
        List.find_map items ~f:(fun r ->
            match r.Gff.feature with
            | Some "3UTR" -> Some r
            | _ -> None
          )
      )
end
