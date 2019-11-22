(* Range library for making easy folding on a sequence of integers
Copyright (C) 2018,2019 Aldrik KLEBER

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version. *)

open Base

type limit = {start: int; stop: int}

type t = Natural of limit | Modified of limit * (int -> int option)

type elt = int

let no_common_area_msg = "There is no common area between the two ranges."

let get_limit_from = function Modified (r, _) -> r | Natural r -> r

let implode f p =
  let r = get_limit_from p in
  f r.start r.stop

let from start stop = Natural {start= min start stop; stop= max start stop}

let filter f = function
  | Natural r ->
      Modified (r, fun n -> Option.some_if (f n) n)
  | Modified (r, f_prev) ->
      Modified (r, Fn.compose (Option.filter ~f) f_prev)

let is_natural = function Natural _ -> true | Modified _ -> false

let reset r = Natural (get_limit_from r)

let rec fold_by_loop r step f acc n =
  if n > r.stop then acc
  else if n = r.stop then f acc n
  else fold_by_loop r step f (f acc n) (min r.stop (n + step))

let fold_by step f acc = function
  | Natural r ->
      fold_by_loop r step f acc r.start
  | Modified (r, f_filter) ->
      let f_with_filter acc n =
        n |> f_filter |> Option.value_map ~default:acc ~f:(f acc)
      in
      fold_by_loop r step f_with_filter acc r.start

let rec gen_fold_loop f_test f_next r f acc n =
  if f_test n then acc
  else gen_fold_loop f_test f_next r f (f acc n) (f_next n)

let fold_loop r f acc n = gen_fold_loop (( < ) r.stop) Int.succ r f acc n

let fold_right_loop r f acc n =
  gen_fold_loop (( > ) r.start) Int.pred r f acc n

let fold_right f acc = function
  | Natural r ->
      fold_right_loop r f acc r.stop
  | Modified (r, f_filter) ->
      let f_agg acc n =
        n |> f_filter |> Option.value_map ~default:acc ~f:(f acc)
      in
      fold_right_loop r f_agg acc r.stop

let fold f acc = function
  | Natural r ->
      fold_loop r f acc r.start
  | Modified (r, f_filter) ->
      let f_agg acc n =
        n |> f_filter |> Option.value_map ~default:acc ~f:(f acc)
      in
      fold_loop r f_agg acc r.start

let to_list = fold (Fn.flip List.cons) []

let equal a b =
  match (a, b) with
  | Natural ra, Natural rb ->
      ra.start = rb.start && ra.stop = rb.stop
  | Modified _, Modified _ ->
      List.equal Int.( = ) (to_list a) (to_list b)
  | _ ->
      false

let rec iter_loop r f n =
  if n > r.stop then ()
  else (
    f n ;
    iter_loop r f (Int.succ n) )

let iter f = function
  | Natural r ->
      iter_loop r f r.start
  | Modified (r, f_filter) ->
      let f_with_filter n = n |> f_filter |> Option.value_map ~default:() ~f in
      iter_loop r f_with_filter r.start

let length = implode (fun start stop -> stop - start)

let split minimal n r =
  let big_enough minimal n size = n >= minimal && size > minimal in
  let diff = length r in
  let pack_size = Float.(of_int diff / of_int n |> round_up |> Int.of_float) in
  if not (big_enough minimal pack_size diff) then [r]
  else
    let f acc n =
      match acc with
      | Some (next_start, result) ->
          Some (Int.succ n, from next_start n :: result)
      | None ->
          Some (n, [])
    in
    r |> fold_by pack_size f None |> Option.value_map ~default:[] ~f:snd

let contain e = function
  | Natural r ->
      r.start <= e && e <= r.stop
  | Modified _ as data ->
      fold (fun acc n -> n = e || acc) false data

let pair_map f (a, b) = (f a, f b)

let agg_exn f a b = Option.value_exn ~message:no_common_area_msg (f a b)

let gen_agg flow fhigh a b =
  let ra, rb = pair_map get_limit_from (a, b) in
  if ra.stop < rb.start || rb.stop < ra.start then None
  else Some (from (flow ra.start rb.start) (fhigh ra.stop rb.stop))

let cross = gen_agg max min

let cross_exn = agg_exn cross

let join = gen_agg min max

let join_exn = agg_exn join

let map f = function
  | Natural r ->
      Modified (r, fun n -> Some (f n))
  | Modified (r, f_filter) ->
      let new_f n = Option.(f_filter n >>= fun n -> f n |> some) in
      Modified (r, new_f)

let limit_to_string r = Int.(to_string r.start ^ ":" ^ to_string r.stop)

let export_string r prefix = prefix ^ limit_to_string r

let to_string = function
  | Natural r ->
      export_string r "Nat:"
  | Modified (r, _) ->
      export_string r "Mod:"

let of_string s =
  Option.value_exn ~message:"Unrecognized string format"
    (String.split ~on:':' s |> List.tl)
  |> List.map ~f:Int.of_string
  |> function [start; stop] -> from start stop | _ -> assert false
