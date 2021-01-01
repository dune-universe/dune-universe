type 'a t = C of 'a
[@@deriving gt ~options:{ show }]


let () =
  print_endline @@ GT.show(t) (GT.show GT.int) (C 1)


type 'a tree = Leaf | Node of 'a * 'a tree GT.list
[@@deriving gt ~options:{ show }]
