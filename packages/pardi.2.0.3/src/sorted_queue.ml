
(* a queue so that results coming in random order are put back in input order
   before being muxed out (for the Mux.Sort_cat_into output mode) *)

module Ht = BatHashtbl

type 'a t = { mutable next_rank: int;
              q: (int, 'a) Ht.t }

let create () =
  let next_rank = 0 in
  let q = Ht.create 1021 in
  { next_rank; q }

let insert (rank, elt) q =
  assert(not (Ht.mem q.q rank)); (* no two elements can have same rank *)
  Ht.add q.q rank elt

let pop q =
  match Ht.find_option q.q q.next_rank with
  | None -> None
  | Some x ->
    begin
      q.next_rank <- q.next_rank + 1;
      Some x
    end

let pop_all q =
  let rec loop acc =
    match pop q with
    | None -> List.rev acc
    | Some x -> loop (x :: acc) in
  loop []
