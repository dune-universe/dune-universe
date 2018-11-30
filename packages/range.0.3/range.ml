(* Range library for making easy folding on a sequence of integers
Copyright (C) 2018 Aldrik KLEBER

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version. *)

type t = {start: int; stop: int}

let implode f r = f r.start r.stop

let from start stop = {start= min start stop; stop= max start stop}

let fold_by step f acc {start; stop} =
  let rec loop acc n =
    if n > stop then acc
    else if n = stop then f acc n
    else loop (f acc n) (min stop (n + step))
  in
  loop acc start

let fold f acc {start; stop} =
  let rec loop acc n = if n > stop then acc else loop (f acc n) (succ n) in
  loop acc start

let iter f {start; stop} =
  let rec loop n =
    if n > stop then ()
    else (
      f n ;
      loop (succ n) )
  in
  loop start

let length = implode (fun start stop -> stop - start)

let split minimal n r =
  let range_big_enough minimal n size = n >= minimal && size > minimal in
  let diff = length r in
  let packet_size =
    float_of_int diff /. float_of_int n |> ceil |> int_of_float
  in
  if range_big_enough minimal packet_size diff = false then [r]
  else
    let f acc n =
      match acc with
      | Some (next_start, result) -> Some (succ n, from next_start n :: result)
      | None -> Some (n, [])
    in
    r |> fold_by packet_size f None |> function None -> [] | Some (_, l) -> l

let contain e r = r.start <= e && e <= r.stop

let cross a b = {start= max a.start b.start; stop= min a.stop b.stop}

let join a b = {start= min a.start b.start; stop= max a.stop b.stop}

let map f {start; stop} = {start= f start; stop= f stop}

let aggregate f a b = {start= f a.start b.start; stop= f a.stop b.stop}

let to_string =
  implode (fun start stop -> string_of_int start ^ ":" ^ string_of_int stop)
