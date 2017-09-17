module OCSA = OCamlStandard.Array

type 'a t = 'a list

open Functions.Function1.O
open Bool.O

(* @feature Add a 'Small' module with the same interface but non-terminal-recursive implementations *)

let empty = []
let singleton x = [x]
let prepend x xs = x::xs
let of_list = Functions.Function1.identity
let to_list = Functions.Function1.identity
(* @todo Test everything for stack overflow. Including to/of_array. *)
let of_array = OCSA.to_list
let to_array = OCSA.of_list

let is_empty = function
  | [] -> true
  | _ -> false

let try_head = function
  | [] -> None
  | x::_ -> Some x

let try_tail = function
  | [] -> None
  | _::xs -> Some xs

let head xs =
  try_head xs
  |> Option.or_failure "List.head"

let tail xs =
  try_tail xs
  |> Option.or_failure "List.tail"

let reverse xs =
  let rec aux ys = function
    | [] -> ys
    | x::xs -> aux (x::ys) xs
  in
  aux [] xs

let rev_concat xs ys =
  let rec aux ys = function
    | [] -> ys
    | x::xs -> aux (x::ys) xs
  in
  aux ys xs

let concat xs ys =
  rev_concat (reverse xs) ys

module O = struct
  let (@) = concat
end


let map xs ~f =
  let rec aux ys = function
    | [] -> reverse ys
    | x::xs -> let y = f x in aux (y::ys) xs
  in
  aux [] xs

let map_acc ~acc xs ~f =
  let rec aux acc ys = function
    | [] -> reverse ys
    | x::xs -> let (acc, y) = f ~acc x in aux acc (y::ys) xs
  in
  aux acc [] xs

let map_i xs ~f =
  map_acc ~acc:0 xs ~f:(fun ~acc:i x -> (Int.succ i, f ~i x))


let flat_map xs ~f =
  let rec aux ys = function
    | [] -> reverse ys
    | x::xs -> let y = f x in aux (rev_concat y ys) xs
  in
  aux [] xs

let flat_map_acc ~acc xs ~f =
  let rec aux acc ys = function
    | [] -> reverse ys
    | x::xs -> let (acc, y) = f ~acc x in aux acc (rev_concat y ys) xs
  in
  aux acc [] xs

let flat_map_i xs ~f =
  flat_map_acc ~acc:0 xs ~f:(fun ~acc:i x -> (Int.succ i, f ~i x))


let filter xs ~f =
  let rec aux ys = function
    | [] -> reverse ys
    | x::xs ->
      let b = f x in
      aux (if b then x::ys else ys) xs
  in
  aux [] xs

let filter_acc ~acc xs ~f =
  let rec aux acc ys = function
    | [] -> reverse ys
    | x::xs ->
      let (acc, b) = f ~acc x in
      aux acc (if b then x::ys else ys) xs
  in
  aux acc [] xs

let filter_i xs ~f =
  filter_acc ~acc:0 xs ~f:(fun ~acc:i x -> (Int.succ i, f ~i x))


let filter_map xs ~f =
  let rec aux ys = function
    | [] -> reverse ys
    | x::xs ->
      let y = f x in
      let ys = match y with
        | Some y -> y::ys
        | None -> ys
      in
      aux ys xs
  in
  aux [] xs

let filter_map_acc ~acc xs ~f =
  let rec aux acc ys = function
    | [] -> reverse ys
    | x::xs ->
      let (acc, y) = f ~acc x in
      let ys = match y with
        | Some y -> y::ys
        | None -> ys
      in
      aux acc ys xs
  in
  aux acc [] xs

let filter_map_i xs ~f =
  filter_map_acc ~acc:0 xs ~f:(fun ~acc:i x -> (Int.succ i, f ~i x))


let fold ~init xs ~f =
  let rec aux y = function
    | [] -> y
    | x::xs -> let y = f y x in aux y xs
  in
  aux init xs

let fold_acc ~acc ~init xs ~f =
  let rec aux acc y = function
    | [] -> y
    | x::xs -> let (acc, y) = f ~acc y x in aux acc y xs
  in
  aux acc init xs

let fold_i ~init xs ~f =
  fold_acc ~acc:0 ~init xs ~f:(fun ~acc:i y x -> (Int.succ i, f ~i y x))


let try_reduce_acc ~acc xs ~f =
  match xs with
    | [] -> None
    | init::xs -> Some (fold_acc ~acc xs ~init ~f)

let try_reduce xs ~f =
  match xs with
    | [] -> None
    | init::xs -> Some (fold xs ~init ~f)

let try_reduce_i xs ~f =
  match xs with
    | [] -> None
    | init::xs -> Some (fold_i xs ~init ~f)


let reduce_acc ~acc xs ~f =
  try_reduce_acc ~acc xs ~f
  |> Option.or_failure "List.reduce_acc"

let reduce xs ~f =
  try_reduce xs ~f
  |> Option.or_failure "List.reduce"

let reduce_i xs ~f =
  try_reduce_i xs ~f
  |> Option.or_failure "List.reduce_i"


let scan ~init xs ~f =
  let rec aux y ys = function
    | [] -> reverse (y::ys)
    | x::xs -> let y = f y x in aux y (y::ys) xs
  in
  aux init [] xs

let scan_acc ~acc ~init xs ~f =
  let rec aux acc y ys = function
    | [] -> reverse (y::ys)
    | x::xs -> let (acc, y) = f ~acc y x in aux acc y (y::ys) xs
  in
  aux acc init [] xs

let scan_i ~init xs ~f =
  scan_acc ~acc:0 ~init xs ~f:(fun ~acc:i y x -> (Int.succ i, f ~i y x))


let iter xs ~f =
  let rec aux = function
    | [] -> ()
    | x::xs -> let () = f x in aux xs
  in
  aux xs

let iter_acc ~acc xs ~f =
  let rec aux acc = function
    | [] -> ()
    | x::xs -> let acc = f ~acc x in aux acc xs
  in
  aux acc xs

let iter_i xs ~f =
  iter_acc ~acc:0 xs ~f:(fun ~acc:i x -> f ~i x; Int.succ i)


let count xs ~f =
  fold xs ~init:0 ~f:(fun n x -> let b = f x in if b then Int.succ n else n)

let count_acc ~acc xs ~f =
  fold_acc ~acc xs ~init:0 ~f:(fun ~acc n x -> let (acc, b) = f ~acc x in (acc, if b then Int.succ n else n))

let count_i xs ~f =
  fold_i xs ~init:0 ~f:(fun ~i n x -> let b = f ~i x in if b then Int.succ n else n)


let try_find xs ~f =
  let rec aux = function
    | [] -> None
    | x::xs -> let b = f x in if b then Some x else aux xs
  in
  aux xs

let try_find_acc ~acc xs ~f =
  let rec aux acc = function
    | [] -> None
    | x::xs -> let (acc, b) = f ~acc x in if b then Some x else aux acc xs
  in
  aux acc xs

let try_find_i xs ~f =
  try_find_acc ~acc:0 xs ~f:(fun ~acc:i x -> (Int.succ i, f ~i x))


let find xs ~f =
  try_find xs ~f
  |> Option.value ~exc:Exception.NotFound

let find_acc ~acc xs ~f =
  try_find_acc ~acc xs ~f
  |> Option.value ~exc:Exception.NotFound

let find_i xs ~f =
  try_find_i xs ~f
  |> Option.value ~exc:Exception.NotFound


let there_exists xs ~f =
  try_find xs ~f
  |> Option.is_some

let there_exists_acc ~acc xs ~f =
  try_find_acc ~acc xs ~f
  |> Option.is_some

let there_exists_i xs ~f =
  try_find_i xs ~f
  |> Option.is_some


let for_all xs ~f =
  not (there_exists xs ~f:(not % f))

let for_all_acc ~acc xs ~f =
  not (there_exists_acc ~acc xs ~f:(fun ~acc x -> let (acc, b) = f ~acc x in (acc, not b)))

let for_all_i xs ~f =
  not (there_exists_i xs ~f:(fun ~i x -> not (f ~i x)))


let try_find_map xs ~f =
  let rec aux = function
    | [] -> None
    | x::xs -> let b = f x in if Option.is_some b then b else aux xs
  in
  aux xs

let try_find_map_acc ~acc xs ~f =
  let rec aux acc = function
    | [] -> None
    | x::xs -> let (acc, b) = f ~acc x in if Option.is_some b then b else aux acc xs
  in
  aux acc xs

let try_find_map_i xs ~f =
  try_find_map_acc ~acc:0 xs ~f:(fun ~acc:i x -> (Int.succ i, f ~i x))


let find_map xs ~f =
  try_find_map xs ~f
  |> Option.value ~exc:Exception.NotFound

let find_map_acc ~acc xs ~f =
  try_find_map_acc ~acc xs ~f
  |> Option.value ~exc:Exception.NotFound

let find_map_i xs ~f =
  try_find_map_i xs ~f
  |> Option.value ~exc:Exception.NotFound


let fold_short ~init xs ~f =
  let rec aux y = function
    | [] -> y
    | x::xs ->
      let (s, y) = f y x in
      match s with
        | Shorten.GoOn -> aux y xs
        | Shorten.ShortCircuit -> y
  in
  aux init xs

let fold_short_acc ~acc ~init xs ~f =
  let rec aux acc y = function
    | [] -> y
    | x::xs ->
      let (acc, s, y) = f ~acc y x in
      match s with
        | Shorten.GoOn -> aux acc y xs
        | Shorten.ShortCircuit -> y
  in
  aux acc init xs

let fold_short_i ~init xs ~f =
  fold_short_acc ~acc:0 ~init xs ~f:(fun ~acc:i y x -> let (s, y) = f ~i y x in (Int.succ i, s, y))


let try_reduce_short_acc ~acc xs ~f =
  match xs with
    | [] -> None
    | init::xs -> Some (fold_short_acc ~acc xs ~init ~f)

let try_reduce_short xs ~f =
  match xs with
    | [] -> None
    | init::xs -> Some (fold_short xs ~init ~f)

let try_reduce_short_i xs ~f =
  match xs with
    | [] -> None
    | init::xs -> Some (fold_short_i xs ~init ~f)


let reduce_short_acc ~acc xs ~f =
  try_reduce_short_acc ~acc xs ~f
  |> Option.or_failure "List.reduce_short_acc"

let reduce_short xs ~f =
  try_reduce_short xs ~f
  |> Option.or_failure "List.reduce_short"

let reduce_short_i xs ~f =
  try_reduce_short_i xs ~f
  |> Option.or_failure "List.reduce_short_i"


let scan_short ~init xs ~f =
  let rec aux y ys = function
    | [] -> reverse (y::ys)
    | x::xs ->
      let (s, y) = f y x in
      match s with
        | Shorten.GoOn -> aux y (y::ys) xs
        | Shorten.ShortCircuit -> reverse (y::ys)
  in
  aux init [] xs

let scan_short_acc ~acc ~init xs ~f =
  let rec aux acc y ys = function
    | [] -> reverse (y::ys)
    | x::xs ->
      let (acc, s, y) = f ~acc y x in
      match s with
        | Shorten.GoOn -> aux acc y (y::ys) xs
        | Shorten.ShortCircuit -> reverse (y::ys)
  in
  aux acc init [] xs

let scan_short_i ~init xs ~f =
  scan_short_acc ~acc:0 ~init xs ~f:(fun ~acc:i y x -> let (s, y) = f ~i y x in (Int.succ i, s, y))


let iter_short xs ~f =
  let rec aux = function
    | [] -> ()
    | x::xs ->
      match f x with
        | Shorten.GoOn -> aux xs
        | Shorten.ShortCircuit -> ()
  in
  aux xs

let iter_short_acc ~acc xs ~f =
  let rec aux acc = function
    | [] -> ()
    | x::xs ->
      let (acc, s) = f ~acc x in
      match s with
        | Shorten.GoOn -> aux acc xs
        | Shorten.ShortCircuit -> ()
  in
  aux acc xs

let iter_short_i xs ~f =
  iter_short_acc ~acc:0 xs ~f:(fun ~acc:i x -> let s = f ~i x in (Int.succ i, s))


let contains xs x ~equal_a =
  there_exists xs ~f:(equal_a x)

module Poly = struct
  let contains xs x =
    let rec aux = function
      | [] -> false
      | x'::_ when Equate.Poly.equal x' x -> true
      | _::xs -> aux xs
    in
    aux xs
end

let size xs =
  let rec aux s = function
    | [] -> s
    | _::xs -> aux (Int.succ s) xs
  in
  aux 0 xs

let join_string_list ?(sep="") xs =
  xs
  |> try_reduce ~f:(fun a b -> Format.apply "%s%s%s" a sep b)
  |> Option.value_def ~def:""

let repr xs ~repr_a =
  xs
  |> map ~f:repr_a
  |> join_string_list ~sep:"; "
  |> Format.apply "[%s]"


module Two = struct
  let to_pair_list xs ys =
    let rec aux zs = function
      | ([], []) ->
        reverse zs
      | (x::xs, y::ys) ->
        aux ((x, y)::zs) (xs, ys)
      | _ ->
        Exception.invalid_argument "List.Two.to_pair_list"
    in
    aux [] (xs, ys)
end


(* @todo Remove *)
module OCLL = OCamlStandard.ListLabels

let equal xs ys ~equal_a =
  try
    OCLL.for_all2 ~f:equal_a xs ys
  with
    | Invalid_argument _ -> false

(* @todo Make terminal recursive *)
let rec cartesian_product xs ys =
  match xs with
    | [] -> []
    | x::xs -> OCLL.append (OCLL.map ys ~f:(fun y -> (x, y))) (cartesian_product xs ys)
