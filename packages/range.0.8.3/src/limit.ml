(* SPDX-License-Identifier: GPL-3.0-or-later *)
open Base

type t = { start : Int.t; stop : Int.t }

let from start stop = { start = min start stop; stop = max start stop }

let rec fold_by_loop r step f acc n =
  if n > r.stop then acc
  else if n = r.stop then f acc n
  else fold_by_loop r step f (f acc n) (n + step)

let rec gen_fold_loop f_test f_next r f acc n =
  if f_test n then acc else gen_fold_loop f_test f_next r f (f acc n) (f_next n)

let fold_loop r f acc n = gen_fold_loop (( < ) r.stop) Int.succ r f acc n

let fold_right_loop r f acc n = gen_fold_loop (( > ) r.start) Int.pred r f acc n

let rec iter_loop r f n =
  if n > r.stop then ()
  else (
    f n;
    iter_loop r f (Int.succ n) )

let to_string r = Int.(to_string r.start ^ ":" ^ to_string r.stop)

let implode f p = f p.start p.stop

let length = implode (fun start stop -> stop - start + 1)
