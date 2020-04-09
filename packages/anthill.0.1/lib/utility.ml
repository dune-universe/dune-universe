let from_upper c = (Core.Char.to_int c) - 65

let from_lower c = (Core.Char.to_int c) - 97

let caps_in str =
  let lst = ref [] in
  String.iter 
  (fun c -> if ((c <= 'Z') && (c >= 'A')) then lst := (c :: !lst) else ()) 
  str;
  List.sort compare (List.rev !lst);;

let sort_by f l = 
  let open List in
  map snd (sort compare (map (fun x -> f x, x) l));;
