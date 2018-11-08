(* Copyright (C) 2018, Francois Berenger

   Yamanishi laboratory,
   Department of Bioscience and Bioinformatics,
   Faculty of Computer Science and Systems Engineering,
   Kyushu Institute of Technology,
   680-4 Kawazu, Iizuka, Fukuoka, 820-8502, Japan. *)

include BatString

let count_char str char =
  fold_left (fun acc c ->
      if c = char then
        acc + 1
      else
        acc
    ) 0 str

let count_string str sub =
  if sub = "" then raise (Invalid_argument "String.count_string");
  let m = length str in
  let n = length sub in
  let rec loop acc i =
    if i >= m then
      acc
    else
      try
        let j = find_from str i sub in
        loop (acc + 1) (j + n)
      with Not_found -> acc
  in
  loop 0 0
