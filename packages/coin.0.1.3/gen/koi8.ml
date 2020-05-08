type code = int
type name = string
type map  = code * Uchar.t * name

let is_white = function
  | ' ' | '\t' .. '\r' -> true | _ -> false

(* (c) @dbuenzli *)
let trim s =
  let drop = is_white in
  let len = String.length s in
  let max_idx = len - 1 in
  if len = 0 then s else
    let rec left_pos i =
      if i > max_idx then len else
      if drop s.[i] then left_pos (i + 1) else i in
    let rec right_pos i =
      if i < 0 then 0 else
      if drop s.[i] then right_pos (i - 1) else (i + 1) in
    let left = left_pos 0 in
    if left = len then "" else
      let right = right_pos max_idx in
      if left = 0 && right = len then s else
        String.sub s left (right - left)

module Map = Map.Make(struct type t = code let compare = compare end)

let extract source =
  let _, maps = List.partition Source.is_comment source in
  let res = List.fold_left (fun map -> function
      | Source.Map { a; b; name; } -> Map.add a (b, (trim name)) map
      | _ -> assert false) Map.empty maps in
  Ok res
