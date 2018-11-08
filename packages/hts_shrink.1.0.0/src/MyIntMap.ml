(* used to decode unfolded fingerprint mol. desc. (J-L Faulont) *)

(* Copyright (C) 2018, Francois Berenger

   Yamanishi laboratory,
   Department of Bioscience and Bioinformatics,
   Faculty of Computer Science and Systems Engineering,
   Kyushu Institute of Technology,
   680-4 Kawazu, Iizuka, Fukuoka, 820-8502, Japan. *)

open Printf

include BatMap.Int

let of_string s =
  let of_pair p =
    Scanf.sscanf p "%d:%d" (fun x y -> x, y)
  in
  let pairs = MyList.of_string of_pair s in
  MyList.fold_left (fun acc (x, y) ->
      add x y acc
    ) empty pairs

let to_string str_of_value m =
  let buff = Buffer.create 80 in
  Buffer.add_char buff '[';
  let started = ref false in
  iter (fun k v ->
      if !started then Buffer.add_char buff ';'
      else started := true;
      bprintf buff "%d:%s" k (str_of_value v)
    ) m;
  Buffer.add_char buff ']';
  Buffer.contents buff

let to_array max_len def_val conv m =
  let res = Array.make max_len def_val in
  iter (fun k v ->
      Array.unsafe_set res k (conv v)
    ) m;
  res
