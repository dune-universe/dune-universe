
(* a queue so that results coming in random order are put back in input order
   before being muxed out (for the Mux.Sort_cat_into output mode) *)

(* we could remove this file, but it is so cute... *)

module Ht = Hashtbl

type 'a t = { mutable next_rank: int;
              q: (int, 'a) Ht.t }

let create () =
  let next_rank = 0 in
  let q = Ht.create 1021 in
  { next_rank; q }

let insert q (rank, elt) =
  assert(not (Ht.mem q.q rank)); (* no two elements can have same rank *)
  Ht.add q.q rank elt

let pop q =
  try
    let x = Ht.find q.q q.next_rank in
    Ht.remove q.q q.next_rank; (* do not leak mem *)
    q.next_rank <- q.next_rank + 1;
    Some x
  with Not_found -> None

let pop_all q =
  let rec loop acc =
    match pop q with
    | None -> List.rev acc
    | Some x -> loop (x :: acc) in
  loop []
