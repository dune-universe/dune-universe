
include BatList

(* count elements of [l] satisfying [p] *)
let filter_count p l =
  fold_left (fun acc x ->
      if p x then acc + 1
      else acc
    ) 0 l

(* split [l] in two parts,
   the first one being the longest prefix of [l] where [p] holds *)
let partition_while p l =
  let rec loop acc = function
    | [] -> (rev acc, [])
    | x :: xs ->
      if p x then loop (x :: acc) xs
      else (rev acc, x :: xs) in
  loop [] l
