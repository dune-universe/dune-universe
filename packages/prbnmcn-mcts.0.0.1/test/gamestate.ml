type cell = P1 | P2

type outcome = P1_wins | P2_wins | Tie | No_winner

(* A board is a matrix of cell. Here is the frame we're
   using for indexing:
      C0 C1 C2 C3 ...
   R0
   R1 X  X  O
   R2    O
   R3       O
   .
   .
   . *)
type t = cell Sparse_matrix.t

let winning_length = 4

let make = Sparse_matrix.create

let cols = Sparse_matrix.cols

let rows = Sparse_matrix.rows

let get = Sparse_matrix.get

let set = Sparse_matrix.set

let get_row = Sparse_matrix.get_row

let play (board : t) c r cell =
  match get ~row:r ~col:c board with
  | None -> set ~row:r ~col:c board cell
  | _ -> failwith "play: position already occupied"

(* Routines de recherche de position gagnante. *)
let rec check_all_equal board col row next cell i acc =
  if i = winning_length then acc
  else
    match get ~row ~col board with
    | None -> false
    | Some c ->
        let acc = acc && c = cell in
        let (col', row') = next col row in
        check_all_equal board col' row' next cell (i + 1) acc

let search_left (board : t) (col : int) (row : int) =
  match get ~row ~col board with
  | None -> None
  | Some cell ->
      let res =
        check_all_equal board col row (fun c r -> (c + 1, r)) cell 0 true
      in
      if res then Some (cell, col, row) else None

let search_down (board : t) (col : int) (row : int) =
  match get ~row ~col board with
  | None -> None
  | Some cell ->
      let res =
        check_all_equal board col row (fun c r -> (c, r + 1)) cell 0 true
      in
      if res then Some (cell, col, row) else None

let search_diag_up (board : t) (col : int) (row : int) =
  match get ~row ~col board with
  | None -> None
  | Some cell ->
      let res =
        check_all_equal board col row (fun c r -> (c + 1, r - 1)) cell 0 true
      in
      if res then Some (cell, col, row) else None

let search_diag_down (board : t) (col : int) (row : int) =
  match get ~row ~col board with
  | None -> None
  | Some cell ->
      let res =
        check_all_equal board col row (fun c r -> (c + 1, r + 1)) cell 0 true
      in
      if res then Some (cell, col, row) else None

let search (board : t) (col : int) (row : int) =
  match search_left board col row with
  | None -> (
      match search_down board col row with
      | None -> (
          match search_diag_up board col row with
          | None -> (
              match search_diag_down board col row with
              | None -> None
              | res -> res )
          | res -> res )
      | res -> res )
  | res -> res

let search_winning board =
  let cols = cols board in
  let rows = rows board in
  let exception Found of (cell * int * int) in
  try
    for c = 0 to cols - 1 do
      for r = 0 to rows - 1 do
        match search board c r with None -> () | Some res -> raise (Found res)
      done
    done ;
    None
  with Found res -> Some res

let playable_positions (board : t) =
  let cols = cols board in
  let rows = rows board in
  let acc = ref [] in
  for c = 0 to cols - 1 do
    for r = 0 to rows - 1 do
      match get ~row:r ~col:c board with
      | None -> acc := (c, r) :: !acc
      | _ -> ()
    done
  done ;
  !acc
