open Types

exception Removing_nonexistent

type bag = {
  letters : int array;
  mutable n_letters : int;
  mutable blanks : int;
  mutable star : bool;
}

let empty () = {
  letters = Array.make 26 0;
  blanks = 0;
  n_letters = 0;
  star = false
}

let inc bag letter =
  let ix = letter in
  bag.letters.(ix) <- bag.letters.(ix) + 1;
  bag.n_letters <- bag.n_letters + 1

let dec bag letter =
  let ix = letter in
  bag.letters.(ix) <- bag.letters.(ix) - 1;
  bag.n_letters <- bag.n_letters - 1;
  (* this should never happen, but add a guard *)
  if bag.letters.(ix) < 0 then
    raise Removing_nonexistent

let count bag letter =
  let ix = letter in
  bag.letters.(ix)

let add bag letter = match letter with
| Star -> bag.star <- true
| Dot -> bag.blanks <- bag.blanks + 1
| Letter c -> inc bag c
| Group _ -> raise Unsupported_feature

let has_letter bag letter = match letter with
| Star -> bag.star
| Dot -> bag.blanks > 0
| Letter c -> count bag c > 0
| Group _ -> raise Unsupported_feature

let remove bag letter =
  let ret = has_letter bag letter in
  if ret then (
    match letter with
    | Star -> () (* for completeness *)
    | Dot -> bag.blanks <- bag.blanks - 1
    | Letter c -> dec bag c
    | Group _ -> raise Unsupported_feature
  );
  ret

(* Remove a letter from a bag
 * if the letter doesn't exist, try removing a blank *)
let play bag c =
  if remove bag (Letter c) then
    Some (Letter c)
  else if remove bag Dot then
    Some Dot
  else if remove bag Star then
    Some Star
  else
    None

let of_rack rack =
  let bag = empty () in
  List.iter (fun i -> add bag i) rack;
  bag

let is_empty bag =
  bag.blanks == 0 && bag.n_letters == 0

let has_wildcards bag =
  bag.blanks > 0 || bag.star
