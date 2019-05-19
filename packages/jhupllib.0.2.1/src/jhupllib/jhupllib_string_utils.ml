open Batteries;;

let concat_sep sep strs =
  Enum.fold
    (fun acc s ->
       acc ^
       (if String.is_empty acc then "" else sep) ^
       s
    ) "" strs
;;

let concat_sep_delim start stop sep strs =
  start ^ concat_sep sep strs ^ stop
;;

let string_of_list : 'a. ('a -> string) -> 'a list -> string =
  fun pp_el lst ->
    concat_sep_delim "[" "]" ";" @@ List.enum @@ List.map pp_el lst
;;

let string_of_tuple :
  'a 'b. ('a -> string) -> ('b -> string) -> 'a * 'b -> string =
  fun pp_a pp_b (a,b) ->
    "(" ^ pp_a a ^ ", " ^ pp_b b ^ ")"
;;

let indent n s =
  String.replace_chars
    (fun x -> if x = '\n' then "\n" ^ String.make n ' ' else String.of_char x)
    s
;;

let rec whitespace_split ?max:(n=max_int) s =
  if n <= 1
  then [s]
  else
    try
      let (s1,s2) = String.split (String.trim s) ~by:" " in
      s1::(whitespace_split ~max:(n-1) s2)
    with
    | Not_found -> [s]
;;
