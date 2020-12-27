
module L = BatList
module P = Printf

include BatSet.Int

let to_string s =
  let buff = Buffer.create 11 in
  Buffer.add_char buff '[';
  iter (fun x ->
      Buffer.add_string buff
        (if Buffer.length buff = 1
         then P.sprintf "%d" x
         else P.sprintf ";%d" x)
    ) s;
  Buffer.add_char buff ']';
  Buffer.contents buff

let of_string s =
  assert(MyUtils.string_is_a_list_of_integers s);
  let chopped = BatString.chop ~l:1 ~r:1 s in
  let int_strings = BatString.split_on_string ~by:";" chopped in
  L.fold_left (fun acc int_str ->
      add (int_of_string int_str) acc
    ) empty int_strings

let to_bitv size s =
  let res = Bitv.create size false in
  iter (fun i -> Bitv.set res i true) s;
  res

let sum s =
  fold (fun x acc -> x + acc) s 0

let tanimoto a b =
  float (cardinal (inter a b)) /.
  float (cardinal (union a b))
