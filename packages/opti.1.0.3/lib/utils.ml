let nub_list list =
  let already_seen = Hashtbl.create 10 in
  let is_first_time_seen x =
    not (Hashtbl.mem already_seen x) && (Hashtbl.add already_seen x (); true)
  in
  List.filter is_first_time_seen list

let rec group : ('a * 'b) list -> ('a * 'b list) list = function
  | [] -> []
  | (a,b) :: rest ->
     match group rest with
     | [] -> [a,[b]]
     | (a', bs') :: rest ->
        if a = a' then
          (a, b :: bs') :: rest
        else
          (a, [b]) :: (a',bs') :: rest


let subst old_x new_x x
    =
  if x = old_x then
    new_x
  else
    x

let subst_assoc list x =
  try List.assoc x list with Not_found -> x

let rec fix f x =
  let x' = f x in if x = x' then x else fix f x'
