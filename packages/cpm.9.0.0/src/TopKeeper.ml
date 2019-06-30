
(* Keep only the N top scoring elements in memory.
   WARNING: we will have several elements with equal scores when screening
            a huge database *)

module L = List

type 'a t = { max_size: int; (* max number of (top scoring) elements *)
              mutable curr_size: int; (* how many elts currently *)
              mutable min_score: float;
              (* For a given score, elements are in LIFO order *)
              mutable elements: (float * 'a list) list }

(* this does not update the count, on purpose because drop_lowest_score
 * is called when there is one score too much *)
let drop_lowest_score t =
  match t.elements with
  | [] -> assert(false)
  | (score, elts) :: rest ->
    match elts with
    | [] -> assert(false)
    | [_x] ->
      (* this whole score class is dropped, since it has no more members *)
      t.elements <- rest
    | (_x :: y :: zs) ->
      (* just drop the last element that came in with that score *)
      t.elements <- (score, y :: zs) :: rest

(* peek at the currently known min score *)
let peek_score t = match t.elements with
  | [] -> assert(false)
  | (score, _elts) :: _rest -> score

let insert t score x =
  let rec loop acc = function
    | [] -> L.rev_append acc [(score, [x])]
    | (score', elts) :: rest ->
      if score' < score then
        loop ((score', elts) :: acc) rest
      else if score' = score then
        L.rev_append acc ((score', x :: elts) :: rest)
      else (* score' > score *)
        L.rev_append acc ((score, [x]) :: (score', elts) :: rest)
  in
  t.elements <- loop [] t.elements

let get_min_score t =
  t.min_score

let get_curr_size t =
  t.curr_size

let get_max_size t =
  t.max_size

(* when we insert an element *)
let update_bound t score =
  if score < t.min_score then
    t.min_score <- score

(* after we drop one *)
let recompute_bound t =
  t.min_score <- peek_score t

let create (max_size: int): 'a t =
  assert(max_size > 0);
  let curr_size = 0 in
  let min_score = max_float in
  let elements = [] in
  { max_size; curr_size; elements; min_score }

let add (t: 'a t) (score: float) (x: 'a): unit =
  if t.curr_size < t.max_size then
    begin
      (* don't filter, as long as there are not enough elements *)
      insert t score x;
      t.curr_size <- t.curr_size + 1;
      update_bound t score
    end
  else
    begin
      (* enforce data structure invariant *)
      assert(t.curr_size = t.max_size);
      if score > t.min_score then
        begin
          insert t score x;
          drop_lowest_score t;
          recompute_bound t
        end
    end

let high_scores_first (t: 'a t): (float * 'a) list =
  (* put scores in decreasing order *)
  L.fold_left (fun acc1 (score, elts) ->
      (* put back elements in FIFO order *)
      L.fold_left (fun acc2 x ->
          (score, x) :: acc2
        ) acc1 elts
    ) [] t.elements
