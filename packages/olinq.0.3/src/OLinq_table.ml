
(* This file is free software, part of OLinq. See file "license" for more details. *)

(** {1 Generic-Purpose table} *)

type 'a printer = Format.formatter -> 'a -> unit
type 'a sequence = ('a -> unit) -> unit

(** {2 Scalar Value} *)

type data =
  | S of string
  | I of int
  | B of bool
  | F of float

module Data = struct
  type t = data

  let equal = (=)
  let compare = Pervasives.compare
  let print out a = match a with
    | S s -> Format.pp_print_string out s
    | I i -> Format.pp_print_int out i
    | F f -> Format.pp_print_float out f
    | B b -> Format.pp_print_bool out b

  let to_string = function
    | S s -> s
    | I i -> string_of_int i
    | B b -> string_of_bool b
    | F f -> string_of_float f
end

let string s = S s
let int i = I i
let float f = F f
let bool b = B b

(** {2 A row of values} *)

let pp_arr_ ?stop ~sep pp_item out a =
  let stop = match stop with
    | None -> Array.length a
    | Some i -> i
  in
  for i=0 to stop-1 do
    let x = a.(i) in
    if i > 0 then (
      Format.pp_print_string out sep;
      Format.pp_print_cut out ()
    );
    pp_item out x
  done

let to_string_ pp x =
  let buf = Buffer.create 64 in
  let out = Format.formatter_of_buffer buf in
  pp out x;
  Format.pp_print_flush out ();
  Buffer.contents buf

type row = data array

exception IndexError

module Row = struct
  type t = row

  let empty = [| |]
  let make1 d = [| d |]
  let make2 a b = [| a; b |]

  let of_array a = a
  let of_list = Array.of_list

  let get_exn i a =
    try Array.get a i
    with _ -> raise IndexError

  let get i a =
    try Some (get_exn i a)
    with IndexError -> None

  let size = Array.length

  let map ~f a = Array.map f a

  let append a b =
    let na = size a in
    Array.init (na + size b)
      (fun i -> if i<na then a.(i) else b.(i-na))

  let remove_index i a =
    if i<0 || i>=size a then raise IndexError;
    Array.init (size a-1)
      (fun j-> if j<i then a.(j) else a.(j+1))

  let remove_index_l _l _a = assert false (* TODO *)

  let print out a =
    Format.fprintf out "[@[<hv2>%a@]]" (pp_arr_ ~sep:"; " Data.print) a

  let to_string a =
    to_string_ print a
end

(** {2 A Table, that is, an extensible list of Rows} *)

exception DimError
(** Raised in case dimensions don't match *)

type t = {
  names: string array;
  mutable rows: row array;
  mutable size: int;
}

let create ?(size=256) ~names () =
  { names;
    size=0;
    rows=Array.make size Row.empty;
  }

let init ~names n f =
  { names;
    size=n;
    rows=
      Array.init n
        (fun i ->
           let row = f i in
           if Row.size row <> Array.length names then raise DimError;
           row);
  }

let make ~names n row =
  init ~names n (fun _ -> row)

let num_rows t = t.size
let num_cols t = Array.length t.names
let size = num_rows
let names t = t.names

let get_exn i t =
  if i<0 || i>=t.size then raise IndexError;
  t.rows.(i)

let get i t =
  try Some (get_exn i t)
  with IndexError -> None

let get_cell_exn i j t = Row.get_exn j (get_exn i t)

let get_cell i j t =
  try Some (get_cell_exn i j t)
  with IndexError -> None

let push tbl r =
  if Row.size r <> Array.length tbl.names then raise DimError;
  if Array.length tbl.rows = tbl.size then (
    let size' = tbl.size + tbl.size / 2 + 10 in
    let rows' = Array.make size' Row.empty in
    Array.blit tbl.rows 0 rows' 0 tbl.size;
    tbl.rows <- rows';
  );
  tbl.rows.(tbl.size) <- r;
  tbl.size <- tbl.size + 1

let push_seq tbl seq = seq (push tbl)
let push_l tbl l = List.iter (push tbl) l

let fold ~f ~x:acc tbl =
  let rec aux acc i =
    if i=tbl.size then acc
    else
      let acc = f acc tbl.rows.(i) in
      aux acc (i+1)
  in
  aux acc 0

let iter ~f tbl =
  for i=0 to tbl.size - 1 do f tbl.rows.(i) done

let iteri ~f tbl =
  for i=0 to tbl.size - 1 do f i tbl.rows.(i) done

let to_seq tbl yield = iter ~f:yield tbl

let to_list_rev tbl =
  fold tbl ~f:(fun acc x -> x::acc) ~x:[]

let to_list tbl = List.rev (to_list_rev tbl)

let print out t =
  Format.fprintf out "[@[<hv>%a@]]"
    (pp_arr_ ~sep:"" ~stop:t.size Row.print) t.rows



