open Core_kernel

type header = string list

type item = {
  description : string;
  sequence : string;
}
[@@ deriving sexp]

module Parser = struct
  open Angstrom

  let is_not_eol = function
    | '\n' | '\r' -> false
    | _ -> true

  let comment_line =
    choice [ char ';' ; char '#' ] >>= fun comment_char ->
    take_while is_not_eol >>| fun comment_msg ->
    (comment_char, comment_msg)

  let header = sep_by end_of_line comment_line

  let description_line =
    char '>' *> take_while is_not_eol

  let sequence_line =
    peek_char_fail >>= (function
        | '>' -> fail "Expected sequence line, not description"
        | _ -> take_while is_not_eol
      )

  let item =
    description_line <* end_of_line >>= fun description ->
    sep_by end_of_line sequence_line >>| fun seqs ->
    let sequence = String.concat seqs in
    { description ; sequence }

  let fasta =
    let p =
      header >>= fun header ->
      sep_by end_of_line item >>= fun items ->
      option () end_of_line *> end_of_input >>| fun () ->
      List.map ~f:snd header, items
    in
    p <?> "fasta"
end

let from_string s = Angstrom.(parse_string ~consume:All) Parser.fasta s

let from_file fn =
  In_channel.with_file fn ~f:(fun ic ->
      let Angstrom.Buffered.{ buf ; off ; len }, res = Angstrom_unix.parse Parser.fasta ic in
      match res with
      | Ok r -> Ok r
      | Error _ ->
        let snippet = Bigstringaf.substring buf ~off ~len:(Int.min 30 len) in
        let msg = sprintf "Failed to parse: %s" snippet in
        Error msg
    )

let from_file_exn fn =
  from_file fn
  |> Result.ok_or_failwith

let sequences_from_file_exn fn =
  match from_file fn with
  | Ok (_, items) -> List.map items ~f:(fun i -> i.sequence)
  | Error msg -> failwith msg

let to_file fn items =
  Out_channel.with_file fn ~f:(fun oc ->
      List.iter items ~f:(fun it ->
          fprintf oc ">%s\n%s\n" it.description it.sequence
        )
    )
