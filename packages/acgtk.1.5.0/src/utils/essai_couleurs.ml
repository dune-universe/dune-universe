let tag_functions = Format.(pp_get_formatter_tag_functions std_formatter ())

let () = Format.set_tags true

type color =
  | Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White

let color_code = function
  | Black -> 30
  | Red -> 31
  | Green -> 32
  | Yellow -> 33
  | Blue -> 34
  | Magenta -> 35
  | Cyan -> 36
  | White -> 37

let open_color c = Printf.sprintf "\027[1;%im" (color_code c)

let close_color = "\027[0m"

let () = Printf.printf "Ceci est un %sessai%s pour voir la couleur\n%!" (open_color Red) close_color

let new_print_open_tag t = 
  match t with
  | "sig" -> Format.printf "SIGNATURE ("
  | "lex" -> Format.printf "LEXICON ("
  | "tag" -> Format.printf "TAG ("
  | _ -> ()

let new_print_close_tag = function
  | "sig" -> Format.printf ")"
  | "lex" -> Format.printf ")"
  | "tag" -> Format.printf ")"
  | _ -> ()

let new_mark_open_tag t = 
  match t with
  | "sig" -> open_color Green
  | "lex" -> open_color Red
  | "tag" -> open_color Blue
  | _ -> ""

let new_mark_close_tag = function _ -> close_color

let () = Format.(pp_set_formatter_tag_functions std_formatter
						{tag_functions with
						  (*print_open_tag=new_print_open_tag;
						  print_close_tag=new_print_close_tag; *)
						  mark_open_tag=new_mark_open_tag;
						  mark_close_tag=new_mark_close_tag})


let () = Format.fprintf Format.std_formatter "@[<v5>@[Voici@ un@ example@ avec :@]@,@[<2>Plusieurs@ @{<tag>tags@}@ pour@ bien@ illustrer@ le@ phénomène@ et@ vérifier@ que@ ça@ marche@ bien@ avec@ une@ phrase@ assez@ longue@ pour@ tenir@ sur@ plusieurs@ lignes@]@,@[une@ @{<sig>signature@}@ tout@ d'abord@]@,@[puis@ un@ @{<lex>lexique@}@ ensuite@]@.@?"
