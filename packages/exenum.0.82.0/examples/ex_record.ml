(* Consider these two types. *)
type foo = Foo | Bar of int * int | Moo of string

type bar =
    { these: foo ; 
      mist: string ;
      covered: bool list ;
      mountains: int option }

(* As usual, we need function to print values from these types. *)
let string_of_foo = function
  | Foo -> "Foo"
  | Bar (n1, n2) -> Printf.sprintf "Bar (%d, %d)" n1 n2
  | Moo s -> Printf.sprintf "Moo (\"%s\")" s

(* Helper function that builds a string from a list of items *)
let sep map sp l = List.fold_left (fun acu x -> if acu = "" then map x else acu ^ sp ^ (map x)) "" l

let string_of_bar bar =
  Printf.sprintf "\n  { these = %s,\n    mist = \"%s\",\n    covered = [%s],\n    mountains = %s }\n"
    (string_of_foo bar.these) bar.mist (sep string_of_bool ", " bar.covered)
    (match bar.mountains with None -> "None" | Some i -> "Some " ^ (string_of_int i))

open Exenum

(* Building an enumeration for type foo is straightforward. 
 * We just have to read the type definition: foo is a sum type, that is, an union. *)
let enum_foo = 
  union [ single Foo ;
	  map (pair e_int e_int) (fun (a,b) -> Bar (a,b)) ;
	  map e_string (fun s -> Moo s)	]

(* Building an enumeration for type bar is no more difficult. 
 * bar is a record type equivalent to a 4-tuple. *)
let enum_bar =
  map (tuple4 enum_foo e_string (e_list e_bool) (e_option e_int))
    (fun (these, mist, covered, mountains) -> { these ; mist ; covered ; mountains })

(* Let us show some values. *)
let () = show enum_bar string_of_bar 0 10

let index = Big_int.power_int_positive_int 10 100 (* This is 10^100. *)
let () = bigshow enum_bar string_of_bar index 2
