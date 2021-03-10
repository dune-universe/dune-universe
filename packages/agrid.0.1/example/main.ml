let grid = Agrid.of_list [ [ 1; 2; 3 ]; [ 5; 6; 7 ]; [ 9; 10; 11 ] ]

let () = Format.printf "%a@.@." (Agrid.pp Format.pp_print_int) grid

(* prints:
 * 1;  2;  3
 * 5;  6;  7
 * 8; 10; 11
 *)

let grid = Agrid.snoc_col grid (Flex_array.of_list [ 4; 8; 12 ])

let () = Format.printf "%a@.@." (Agrid.pp Format.pp_print_int) grid

(* prints:
 * 1;  2;  3;  4
 * 5;  6;  7;  9
 * 8; 10; 11; 12
 *)

let grid = Agrid.cons_row (Flex_array.of_list [ -3; -2; -1; 0 ]) grid

let () = Format.printf "%a@.@." (Agrid.pp Format.pp_print_int) grid

(* prints:
 * -3; -2; -1;  0
 *  1;  2;  3;  4
 *  5;  6;  7;  9
 *  8; 10; 11; 12
 *)

let () = Format.printf "sum = %d@." (Agrid.fold (fun sum n -> sum + n) 0 grid)

(* prints 72 *)
