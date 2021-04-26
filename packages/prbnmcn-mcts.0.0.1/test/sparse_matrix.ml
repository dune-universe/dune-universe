module Int_map = Map.Make (struct
  type t = int

  (* Overflows should not occur as these integers correspond
     to column/row indices. *)
  let compare (x : int) (y : int) = x - y
end)

type 'a col = 'a Int_map.t

type 'a row = 'a Int_map.t

type 'a t = { cols : int; rows : int; data : 'a col Int_map.t }

exception Out_of_bounds

exception Dimensions_mismatch

let create ~rows ~cols = { cols; rows; data = Int_map.empty }

let cols { cols; _ } = cols

let rows { rows; _ } = rows

let get_col_exn column (m : 'a t) : 'a col = Int_map.find column m.data

let get_col column (m : 'a t) : 'a col option = Int_map.find_opt column m.data

let set_col column (c : 'a col) (m : 'a t) : 'a t =
  { m with data = Int_map.add column c m.data }

let get_elt_exn row (c : 'a col) : 'a = Int_map.find row c

let set_elt row (c : 'a col) elt : 'a col = Int_map.add row elt c

let column_is_empty column (m : 'a t) : bool = not (Int_map.mem column m.data)

let row_is_empty row (m : 'a t) : bool =
  Int_map.for_all (fun _ c -> not (Int_map.mem row c)) m.data

let get_exn : row:int -> col:int -> 'a t -> 'a =
 fun ~row ~col m ->
  let c = get_col_exn col m in
  get_elt_exn row c

let get : row:int -> col:int -> 'a t -> 'a option =
 fun ~row ~col m -> try Some (get_exn ~row ~col m) with Not_found -> None

let set : row:int -> col:int -> 'a t -> 'a -> 'a t =
 fun ~row ~col m elt ->
  if row < 0 || row >= m.rows then raise Out_of_bounds ;
  if col < 0 || col >= m.cols then raise Out_of_bounds ;
  let c = match get_col col m with None -> Int_map.empty | Some c -> c in
  let c = set_elt row c elt in
  set_col col c m

let get_row : row:int -> 'a t -> 'a row =
 fun ~row m ->
  Int_map.fold
    (fun col c acc ->
      if Int_map.mem row c then
        let elt = Int_map.find row c in
        Int_map.add col elt acc
      else acc)
    m.data
    Int_map.empty

let integers n = List.init n (fun i -> i)

let identity n =
  let empty = { cols = n; rows = n; data = Int_map.empty } in
  List.fold_left (fun acc i -> set ~row:i ~col:i acc 1.0) empty (integers n)

let mul (m1 : float t) (m2 : float t) : float t =
  if m1.cols <> m2.rows then raise Dimensions_mismatch ;
  let rows = m1.rows in
  let cols = m2.cols in
  let empty = { cols; rows; data = Int_map.empty } in
  Int_map.fold
    (fun col2 c2 acc ->
      if column_is_empty col2 m2 then acc
      else
        let rows = integers rows in
        List.fold_left
          (fun acc row1 ->
            if row_is_empty row1 m1 then acc
            else
              let res =
                Int_map.fold
                  (fun row2 elt2 acc ->
                    match get ~row:row1 ~col:row2 m1 with
                    | None -> acc
                    | Some elt1 -> acc +. (elt1 *. elt2))
                  c2
                  0.0
              in
              if res = 0.0 then acc else set ~row:row1 ~col:col2 acc res)
          acc
          rows)
    m2.data
    empty

let pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit =
 fun pp_elt fmtr m ->
  Format.pp_open_tbox fmtr () ;
  (* print header *)
  Format.pp_set_tab fmtr () ;
  Format.fprintf fmtr "   " ;
  for c = 0 to m.cols - 1 do
    Format.pp_set_tab fmtr () ;
    Format.fprintf fmtr "%d  " c
  done ;
  Format.pp_print_tab fmtr () ;
  Format.fprintf fmtr "   " ;
  for _c = 0 to m.cols - 1 do
    Format.pp_print_tab fmtr () ;
    Format.fprintf fmtr "-"
  done ;
  for r = 0 to m.rows - 1 do
    Format.pp_print_tab fmtr () ;
    Format.fprintf fmtr "%d" r ;
    for c = 0 to m.cols - 1 do
      match get ~row:r ~col:c m with
      | None ->
          Format.pp_print_tab fmtr () ;
          Format.fprintf fmtr " "
      | Some elt ->
          Format.pp_print_tab fmtr () ;
          Format.fprintf fmtr "%a" pp_elt elt
    done
  done ;
  Format.pp_close_tbox fmtr ()
