open! Core

module Table = String.Table

type t =
  {
    header_index : int Table.t;
    data         : string array;
  }

let equal t1 t2 =
  Table.equal t1.header_index t2.header_index Int.equal
  && Array.equal t1.data t2.data ~equal:String.equal
;;

let compare t1 t2 =
  let data_cmp = Array.compare String.compare t1.data t2.data in
  if data_cmp <> 0
  then data_cmp
  else
    [%compare: (string * int) list]
      (Table.to_alist t1.header_index)
      (Table.to_alist t2.header_index)
;;

let is_empty t =
  let rec loop i =
    if i >= Array.length t.data then true
    else if String.length (String.strip t.data.(i)) > 0 then false
    else loop (i + 1)
  in
  loop 0
;;

let sexp_of_t t =
  let index_table =
    Int.Table.of_alist_exn
      (List.map (Table.to_alist t.header_index) ~f:(fun (k,d) -> (d,k)))
  in
  Sexp.List (Array.to_list (Array.mapi t.data ~f:(fun i v ->
    let k,v =
      match Int.Table.find index_table i with
      | None -> Int.to_string i, v
      | Some k -> k, v
    in
    Sexp.List [Sexp.Atom k; Sexp.Atom v])))
;;

let fold t ~init ~f =
  Hashtbl.fold t.header_index ~init ~f:(fun ~key:header ~data:i acc ->
    f acc ~header ~data:t.data.(i))
;;

let iter t ~f =
  let f () ~header ~data = f ~header ~data in
  fold t ~init:() ~f
;;

let to_string t = Sexp.to_string_hum (sexp_of_t t)

let index_exn t header =
  try
    Table.find_exn t.header_index header
  with
  | _ -> failwithf "header \"%s\" not found" header ()
;;

let get_exn_p t header here =
  let i = index_exn t header in
  try
    t.data.(i)
  with
  | _ ->
    Error.failwithp here "header exists in file but not for this row"
      (`header header, `row t)
      [%sexp_of: [`header of string] * [`row of t]]
;;

let get_exn t header = get_exn_p t header [%here]

let get_conv_exn t header here conv =
  let v = get_exn_p t header here in
  try
    conv v
  with
  | exn ->
    Error.failwithp here "failed to parse"
      (`header header, `row t, `exn exn)
      [%sexp_of: [`header of string] * [`row of t] * [`exn of Exn.t]]
;;

let get t header =
  try
    Some (get_exn t header)
  with
  | _ -> None
;;

let get_opt_exn t header =
  match get t header with
  | None ->
    Error.failwiths "no header in row"
      (`header header, `row t)
      [%sexp_of: [`header of string] * [`row of t]]
  | Some "" ->
    None
  | Some str ->
    Some str

let get_conv_opt_exn t header here conv =
  match get_opt_exn t header with
  | None ->
    None
  | Some v ->
    try
      Some (conv v)
    with
    | exn ->
      Error.failwithp here "failed to parse"
        (`header header, `row t, `exn exn)
        [%sexp_of: [`header of string] * [`row of t] * [`exn of Exn.t]]
;;

let nth_exn t i = t.data.(i)

let nth_conv_exn t i here conv =
  try conv (nth_exn t i) with
  | exn ->
    Error.failwithp here "failed to parse"
      (`nth i, `row t, `exn exn)
      [%sexp_of: [`nth of int] * [`row of t] * [`exn of Exn.t]]
;;

let nth t i =
  try
    Some (nth_exn t i)
  with
  | _ -> None
;;

let nth_conv t i conv =
  try
    Some (conv (nth_exn t i))
  with
  | _ -> None
;;

let create header_index data = { header_index; data }

let to_list t  = Array.to_list t.data
let to_array t = t.data
let length t   = Array.length t.data

let headers t : int String.Table.t = t.header_index
