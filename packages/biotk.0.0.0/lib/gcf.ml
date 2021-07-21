open Core_kernel

type row = GLoc.t * string list

let parse_line l =
  match String.split ~on:'\t' l with
  | [] -> None
  | h :: t ->
    let loc = GLoc.of_string_exn h in
    Some (loc, t)

let unparse_line ((loc, fields) : row) =
  loc.chr :: Int.to_string loc.lo :: Int.to_string loc.hi :: fields
  |> String.concat ~sep:"\t"

let to_bed ?(open_end = false) gcf dest =
  let remap = if open_end then List.map ~f:(fun ((l, fields) : row) -> { l with hi = l.hi + 1 }, fields) else Fn.id in
  In_channel.read_lines gcf
  |> List.filter_map ~f:parse_line
  |> remap
  |> List.map ~f:unparse_line
  |> Out_channel.write_lines dest
