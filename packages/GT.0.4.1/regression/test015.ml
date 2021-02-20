@type 'a tree = Node of 'a * 'a tree GT.list with show, foldl, foldr

let _ =
  let x  = Node (1, [Node (2, [Node (5, [])]); Node (3, []); Node (4, [Node (6, [])])]) in
  let n  = GT.transform(tree) (new @tree[foldl] (+)) 0 x in

  let fa s x = if s = "" then string_of_int x else s ^ ", " ^ string_of_int x in
  let sl = GT.transform(tree) (new @tree[foldl] fa) "" x in
  let sr = GT.transform(tree) (new @tree[foldr] fa) "" x in
  Printf.printf "%s\n" @@
  GT.transform(tree) (new @tree[show] (GT.lift string_of_int)) () x;
  Printf.printf "%d\n" n;
  Printf.printf "%s\n" sl;
  Printf.printf "%s\n" sr
