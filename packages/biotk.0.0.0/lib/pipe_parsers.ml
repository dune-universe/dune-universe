open Base
include Biotk_pipes_unix.Pipe

module Line = Biocaml_base.Line
module Lines = Biocaml_base.Lines

let ( %% ) f g = Fn.(flip compose) f g
let ( >>= ) = bind

let lines () =
  let open Lines.Parser in
  loop step initial_state

let lines_to_strings () =
  let rec loop () =
    await () >>= function
    | None -> return ()
    | Some (l : Line.t) ->
      yield (l :> string) >>= fun () ->
      yield "\n" >>= fun () ->
      loop ()
  in
  loop ()

let bed_parser () =
  lines ()
  $$ map (Biocaml_base.Bed.item_of_line %% Result.ok_or_failwith)

let bed_unparser () =
  map Biocaml_base.Bed.line_of_item
  $$ lines_to_strings ()

let gff3_parser () =
  lines ()
  $$ map (Gff.Item.parse)


let gff_unparser version =
  map (Biocaml_base.Gff.line_of_item version)
  $$ lines_to_strings ()

let table_parser () =
  lines ()
  $$ map (Line.split ~on:'\t')

let table_unparser () =
  map (fun fields -> Line.of_string_unsafe (String.concat ~sep:"\t" fields))
  $$ lines_to_strings ()

let macs_xls_parser =
  map Macs.Xls.parse

let macs_xls_unparser =
  map Macs.Xls.unparse

let fasta_parser () =
  loop
    (fun state data ->
       match Biocaml_base.Fasta.Parser.step state data with
       | Ok (state, items) -> state, items
       | Error (`Fasta_parser_error (lno, msg)) ->
         Core.failwithf "Incorrect FASTA format (L%d): %s" lno msg ())
    (Biocaml_base.Fasta.Parser.initial_state ())
