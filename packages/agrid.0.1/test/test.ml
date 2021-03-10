open Agrid

let grid = empty (* [] *)

let pred = [ height grid = 0; width grid = 0; dim grid = (0, 0) ]

let () = List.iter (fun p -> assert p) pred

let grid = of_list [ [ 1; 2; 3 ]; [ 4; 5; 6 ] ]

(* 1; 2; 3 | 4; 5; 6 *)

let pred =
  [ height grid = 2
  ; width grid = 3
  ; dim grid = (3, 2)
  ; get grid ~x:0 ~y:0 = 1
  ; get grid ~x:2 ~y:1 = 6
  ; get (set grid ~x:0 ~y:0 7) ~x:0 ~y:0 = 7
  ]

let () = List.iter (fun p -> assert p) pred

let grid = of_array [| [| 1; 2; 3 |]; [| 4; 5; 6 |] |]

(* 1; 2; 3 | 4; 5; 6 *)

let pred =
  [ height grid = 2
  ; width grid = 3
  ; dim grid = (3, 2)
  ; get grid ~x:0 ~y:0 = 1
  ; get grid ~x:2 ~y:1 = 6
  ; get (set grid ~x:0 ~y:0 7) ~x:0 ~y:0 = 7
  ; get_row grid 0 = Flex_array.of_list [ 1; 2; 3 ]
  ; get_col grid 0 = Flex_array.of_list [ 1; 4 ]
  ]

let () = List.iter (fun p -> assert p) pred

let row = Flex_array.of_list [ 7; 8; 9 ]

let grid = cons_row row grid (* 7; 8; 9 | 1; 2; 3 | 4; 5; 6 *)

let pred =
  [ height grid = 3
  ; width grid = 3
  ; get grid ~x:0 ~y:0 = 7
  ; get grid ~x:1 ~y:0 = 8
  ]

let () = List.iter (fun p -> assert p) pred

let grid = snoc_row (tail_row grid) row (* 1; 2; 3 | 4; 5; 6 | 7; 8; 9 *)

let pred =
  [ height grid = 3
  ; width grid = 3
  ; get grid ~x:0 ~y:0 = 1
  ; get grid ~x:0 ~y:2 = 7
  ]

let () = List.iter (fun p -> assert p) pred

let grid = liat_row grid (* 1; 2; 3; | 4; 5; 6] *)

let pred =
  [ height grid = 2
  ; width grid = 3
  ; get grid ~x:0 ~y:0 = 1
  ; get grid ~x:2 ~y:1 = 6
  ; (let buf = Buffer.create 6 in
     let fmt = Format.formatter_of_buffer buf in
     let () = iter (fun x -> Format.fprintf fmt "%d@." x) grid in
     String.equal (Buffer.contents buf) "1\n2\n3\n4\n5\n6\n")
  ; (let buf = Buffer.create 6 in
     let fmt = Format.formatter_of_buffer buf in
     let () = iteri (fun ~x ~y -> Format.fprintf fmt "%d%d%d@." x y) grid in
     String.equal (Buffer.contents buf) "001\n102\n203\n014\n115\n216\n")
  ]

let () = List.iter (fun p -> assert p) pred

let grid = map (fun x -> x + 1) grid (* 2; 3; 4 | 5; 6; 7 *)

let pred =
  [ height grid = 2
  ; width grid = 3
  ; get grid ~x:0 ~y:0 = 2
  ; get grid ~x:2 ~y:1 = 7
  ]

let () = List.iter (fun p -> assert p) pred

let grid = mapi (fun ~x ~y v -> x + y + v - 1) grid (* 1; 3; 5 | 5; 7; 9 *)

let pred =
  [ height grid = 2
  ; width grid = 3
  ; get grid ~x:0 ~y:0 = 1
  ; get grid ~x:2 ~y:1 = 9
  ]

let () = List.iter (fun p -> assert p) pred

let grid = tail_col grid (* 3; 5 | 7; 9 *)

let pred =
  [ height grid = 2
  ; width grid = 2
  ; get grid ~x:0 ~y:0 = 3
  ; get grid ~x:1 ~y:1 = 9
  ]

let () = List.iter (fun p -> assert p) pred

let grid = liat_col grid (* 3; 7 *)

let pred =
  [ height grid = 2
  ; width grid = 1
  ; get grid ~x:0 ~y:0 = 3
  ; get grid ~x:0 ~y:1 = 7
  ]

let () = List.iter (fun p -> assert p) pred

let grid = cons_col (Flex_array.of_list [ 2; 6 ]) grid (* 2; 3 | 6; 7 *)

let pred =
  [ height grid = 2
  ; width grid = 2
  ; get grid ~x:0 ~y:0 = 2
  ; get grid ~x:1 ~y:1 = 7
  ]

let () = List.iter (fun p -> assert p) pred

let grid = snoc_col grid (Flex_array.of_list [ 4; 8 ]) (* 2; 3; 4 | 6; 7; 8 *)

let pred =
  [ height grid = 2
  ; width grid = 3
  ; get grid ~x:0 ~y:0 = 2
  ; get grid ~x:2 ~y:1 = 8
  ; String.equal
      (Format.asprintf "%a" (pp Format.pp_print_int) grid)
      "2;3;4\n6;7;8"
  ]

let () = List.iter (fun p -> assert p) pred

let grid = cons_col (Flex_array.of_list [ 1; 2 ]) empty

let pred =
  [ height grid = 2
  ; width grid = 1
  ; get grid ~x:0 ~y:0 = 1
  ; get grid ~x:0 ~y:1 = 2
  ]

let () = List.iter (fun p -> assert p) pred

let grid = snoc_col empty (Flex_array.of_list [ 1; 2 ])

let pred =
  [ height grid = 2
  ; width grid = 1
  ; get grid ~x:0 ~y:0 = 1
  ; get grid ~x:0 ~y:1 = 2
  ]

let () = List.iter (fun p -> assert p) pred

let grid = of_list [ [ 1; 2; 3 ]; [ 4; 5; 6 ] ]

let pred = [ fold (fun acc el -> acc + el) 0 grid = 21 ]

let () = List.iter (fun p -> assert p) pred
