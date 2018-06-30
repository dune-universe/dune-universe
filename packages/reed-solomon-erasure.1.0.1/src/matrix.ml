open Ops

module Error = struct
  type t = SingularMatrix

  module Exn = struct
    exception SingularMatrix
  end

  let to_exn (err : t) : exn =
    match err with
    | SingularMatrix -> Exn.SingularMatrix

  let unwrap (res : ('a, t) result) : 'a =
    match res with
    | Ok x    -> x
    | Error e -> raise (to_exn e)
end

type t = {
  row_count : int;
  col_count : int;
  data      : bytes array;
}

let ( .%{}   ) = fun m (x,y)   -> m.(x).%(y)
let ( .%{}<- ) = fun m (x,y) v -> m.(x).%(y) <- v

let ( .&{}   ) = fun m (x,y)   -> m.data.%{x,y}
let ( .&{}<- ) = fun m (x,y) v -> m.data.%{x,y} <- v

let make_bytes_array (rows : int) (cols : int) : bytes array =
  let data = Array.make rows (Bytes.empty) in
  Array.map (fun _ -> Bytes.make cols '\x00') data

let make (rows : int) (cols : int) : t =
  let data = make_bytes_array rows cols in

  { row_count = rows;
    col_count = cols;
    data              }

let make_with_data (init_data : bytes array) : t =
  let rows = Array.length init_data in
  let cols = Bytes.length init_data.(0) in

  for r = 0 to (rows) - 1 do
    if Bytes.length init_data.(r) <> cols then
      failwith "Inconsistent row sizes"
  done;

  let data = make_bytes_array rows cols in

  for r = 0 to (rows) - 1 do
    for c = 0 to (cols) - 1 do
      data.%{r,c} <- init_data.(r).%(c)
    done
  done;

  { row_count = rows;
    col_count = cols;
    data              }

let print_debug (m : t) : unit =
  Array.iter
    (fun x ->
       Bytes.iter
         (fun x ->
            Printf.printf "%d, " (int_of_char x)
         )
         x;
       Printf.printf "\n"
    )
    m.data

let identity (size : int) : t =
  let result = make size size in

  for i = 0 to (size) - 1 do
    result.&{i,i} <- 1 |> char_of_int;
  done;

  result

let col_count (m : t) : int =
  m.col_count

let row_count (m : t) : int =
  m.row_count

let get (m : t) (r : int) (c : int) : char =
  m.&{r,c}

let set (m : t) (r : int) (c : int) (v : char) : unit =
  m.&{r,c} <- v

let copy (m : t) : t =
  make_with_data (Array.map (fun x -> Bytes.copy x) m.data)

let multiply (lhs : t) (rhs : t) : t =
  if lhs.col_count <> rhs.col_count then
    failwith (Printf.sprintf "Colomn count on left is different from row count on right, lhs : %d, rhs : %d" lhs.col_count rhs.col_count);

  let result = make lhs.row_count rhs.col_count in

  for r = 0 to (lhs.row_count) - 1 do
    for c = 0 to (rhs.col_count) - 1 do
      let v = ref 0 in
      for i = 0 to (lhs.col_count) - 1 do
        v := (Galois.mul lhs.&{r,i} rhs.&{i,c} |> int_of_char) lxor !v;
      done;
      result.&{r,c} <- !v |> char_of_int;
    done
  done;

  result

let augment (lhs : t) (rhs : t) : t =
  if lhs.row_count <> rhs.row_count then
    failwith (Printf.sprintf "Matrices do not have the same row count, lhs : %d, rhs : %d" lhs.row_count rhs.row_count);

  let result = make lhs.row_count (lhs.col_count + rhs.col_count) in

  for r = 0 to (lhs.row_count) - 1 do
    for c = 0 to (lhs.col_count) - 1 do
      result.&{r,c} <- lhs.&{r,c};
    done;
    let lhs_col_count = lhs.col_count in
    for c = 0 to (rhs.col_count) - 1 do
      result.&{r,lhs_col_count + c} <- rhs.&{r,c};
    done
  done;

  result

let sub_matrix
    (m    : t)
    (rmin : int)
    (cmin : int)
    (rmax : int)
    (cmax : int)
  : t =
  let result = make (rmax - rmin) (cmax - cmin) in

  for r = rmin to (rmax) - 1 do
    for c = cmin to (cmax) - 1 do
      result.&{r - rmin, c - cmin} <- m.&{r,c};
    done
  done;

  result

let get_row (m : t) (row : int) : bytes =
  m.data.(row)

let swap_rows (m : t) (r1 : int) (r2 : int) : unit =
  if r1 <> r2 then (
    let row1 = m.data.(r1) in
    let row2 = m.data.(r2) in

    m.data.(r1) <- row2;
    m.data.(r2) <- row1
  )

let is_square (m : t) : bool =
  m.row_count = m.col_count

let gaussian_elim (m : t) : (unit, Error.t) result =
  let char_0 = char_of_int 0 in
  let char_1 = char_of_int 1 in

  let rec do_swaps (r : int) ?(r_below : int = r + 1) (m : t) : unit =
    if r_below < m.row_count then (
      if m.&{r_below, r} <> char_0 then
        swap_rows m r r_below
      else
        do_swaps r ~r_below:(r_below + 1) m;
    )
  in

  let rec loop ?(r : int = 0) (row_count : int) (m : t) : (unit, Error.t) result =
    if r < row_count then (
      if m.&{r,r} = char_0 then
        do_swaps r m;
      (* If we couldn't find one, the matrix is singular. *)
      if m.&{r,r} = char_0 then
        Error (SingularMatrix)
      else (
        (* Scale to 1. *)
        if m.&{r,r} <> char_1 then (
          let scale = Galois.div char_1 m.&{r,r} in
          for c = 0 to (m.col_count) - 1 do
            m.&{r,c} <- Galois.mul m.&{r,c} scale;
          done
        );
        (* Make everything below the 1 be a 0 by subtracting
         * a multiple of it.  (Subtraction and addition are
         * both exclusive or in the Galois field.) *)
        for r_below = r+1 to (m.row_count) - 1 do
          if m.&{r_below,r} <> char_0 then (
            let scale = m.&{r_below,r} in
            for c = 0 to (m.col_count) - 1 do
              m.&{r_below, c} <- (((Galois.mul scale m.&{r,c}) |> int_of_char)
                                  lxor
                                  (m.&{r_below, c} |> int_of_char))
                                 |> char_of_int;
            done
          )
        done;
        loop ~r:(r+1) row_count m
      )
    ) else (
      Ok(())
    )
  in

  match loop m.row_count m with
  | Error(_) as e -> e
  | Ok(())        -> begin
      (* Now clear the part above the main diagonal. *)
      for d = 0 to (m.row_count) - 1 do
        for r_above = 0 to (d) - 1 do
          if m.&{r_above,d} <> char_0 then (
            let scale = m.&{r_above, d} in
            for c = 0 to (m.col_count) - 1 do
              m.&{r_above,c} <- ((Galois.mul scale m.&{d, c} |> int_of_char)
                                 lxor
                                 (m.&{r_above,c} |> int_of_char))
                                |> char_of_int;
            done
          )
        done
      done;
      Ok(())
    end

let invert (m : t) : (t, Error.t) result =
  if not (is_square m) then
    failwith "Trying to invert a non-square matrix"
  else (
    let row_count = m.row_count in
    let col_count = m.col_count in

    let work = augment m (identity row_count) in

    match gaussian_elim work with
    | Ok(())        -> Ok(sub_matrix work 0 row_count col_count (col_count * 2))
    | Error(_) as e -> e
  )

let vandermonde (rows : int) (cols : int) : t =
  let result = make rows cols in

  for r = 0 to (rows) - 1 do
    for c = 0 to (cols) - 1 do
      result.&{r,c} <- Galois.exp (r |> char_of_int) c;
    done
  done;

  result
