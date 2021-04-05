(* Copyright 2016 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: Apache-2.0 *)

let ( !! ) = Lazy.force

type 'a t = 'a node Lazy.t

and 'a node = Nil | Cons of 'a * 'a t

let rec iter f = function
  | (lazy Nil) -> ()
  | (lazy (Cons (x, t))) -> f x ; iter f t

let pp ~sep pp_item ppf t =
  let is_first = ref true in
  let print ppf v =
    if !is_first then is_first := false else sep ppf () ;
    pp_item ppf v in
  iter (print ppf) t

let rec fold_right f t z =
  match t with
  | (lazy Nil) -> !!z
  | (lazy (Cons (x, t))) -> f x (lazy (fold_right f t z))

let strict t = fold_right (fun x t -> x :: !!t) t (lazy [])
let unit x = lazy (Cons (x, lazy Nil))
let head = function (lazy Nil) -> None | (lazy (Cons (x, _))) -> Some x
let tail = function (lazy Nil) -> lazy Nil | (lazy (Cons (_, t))) -> t
let items xs = List.fold_right (fun x t -> lazy (Cons (x, t))) xs (lazy Nil)

let rec concat t1 t2 =
  match t1 with
  | (lazy Nil) -> t2
  | (lazy (Cons (x, t))) -> lazy (Cons (x, concat t t2))

let rec continually x = lazy (Cons (x, continually x))

let fold_left f z t =
  let rec loop acc = function
    | (lazy Nil) -> acc
    | (lazy (Cons (x, t))) -> loop (f acc x) t in
  loop z t

let rec take n t =
  if n <= 0 then lazy Nil
  else
    match t with
    | (lazy Nil) -> lazy Nil
    | (lazy (Cons (x, t))) -> lazy (Cons (x, take (n - 1) t))

let rec take_while p = function
  | (lazy Nil) -> lazy Nil
  | (lazy (Cons (x, t))) ->
      if p x then lazy (Cons (x, take_while p t)) else lazy Nil

let rec iterate z f = lazy (Cons (z, iterate (f z) f))
let exists f t = fold_right (fun a lb -> f a || !!lb) t (lazy false)

let rec for_all f = function
  | (lazy Nil) -> true
  | (lazy (Cons (x, t))) -> f x && for_all f t

let rec drop n t =
  if n <= 0 then t
  else
    match t with
    | (lazy Nil) -> lazy Nil
    | (lazy (Cons (_, t))) -> drop (n - 1) t

let rec drop_while p = function
  | (lazy Nil) -> lazy Nil
  | (lazy (Cons (x, t))) as tt -> if p x then drop_while p t else tt

let fill n x = continually x |> take n

let rec map f = function
  | (lazy Nil) -> lazy Nil
  | (lazy (Cons (x, t))) -> lazy (Cons (f x, map f t))

let rec zip_with f t1 t2 =
  match (t1, t2) with
  | (lazy Nil), _ | _, (lazy Nil) -> lazy Nil
  | (lazy (Cons (x, t1))), (lazy (Cons (y, t2))) ->
      lazy (Cons (f x y, zip_with f t1 t2))

let zip t1 t2 = zip_with (fun x y -> (x, y)) t1 t2

let flatten t =
  let lazy_concat t lt = fold_right (fun x lt -> lazy (Cons (x, !!lt))) t lt in
  fold_right lazy_concat t (lazy (lazy Nil))

let cycle t = continually t |> flatten

let filter p t =
  fold_right
    (fun x lt -> if p x then lazy (Cons (x, !!lt)) else !!lt)
    t
    (lazy (lazy Nil))

let of_array = function
  | [||] -> lazy Nil
  | arr ->
      let length = Array.length arr in
      let rec loop i =
        if i = length then lazy Nil else lazy (Cons (arr.(i), loop (i + 1)))
      in
      loop 0

let enum_from z = iterate z (fun x -> x + 1)
let enum_from_to low high = enum_from low |> take_while (fun x -> x <= high)

let unfold s f =
  let rec loop s =
    match f s with Some (s, x) -> lazy (Cons (x, loop s)) | None -> lazy Nil
  in
  loop s

let zip_all_with f t1 t2 =
  unfold (t1, t2) (function
    | (lazy Nil), (lazy Nil) -> None
    | (lazy (Cons (x, t1))), (lazy Nil) -> Some ((t1, lazy Nil), f (Some x) None)
    | (lazy Nil), (lazy (Cons (y, t2))) -> Some ((lazy Nil, t2), f None (Some y))
    | (lazy (Cons (x, t1))), (lazy (Cons (y, t2))) ->
        Some ((t1, t2), f (Some x) (Some y)))

let zip_all t1 t2 = zip_all_with (fun x y -> (x, y)) t1 t2

let equal f t1 t2 =
  zip_all_with
    (fun xo yo ->
      match (xo, yo) with
      | Some _, None | None, Some _ -> false
      | Some x, Some y -> f x y
      | None, None -> assert false)
    t1 t2
  |> for_all (fun x -> x)

let rec find p = function
  | (lazy Nil) -> None
  | (lazy (Cons (x, t))) -> if p x then Some x else find p t

let flat_map f t = map f t |> flatten

let map_filter f t =
  fold_right
    (fun x lt ->
      match f x with Some y -> lazy (Cons (y, !!lt)) | None -> !!lt)
    t
    (lazy (lazy Nil))

let length t = fold_left (fun n _ -> n + 1) 0 t
