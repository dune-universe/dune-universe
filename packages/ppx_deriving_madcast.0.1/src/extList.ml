
let foldi_left f x l =
  snd (
      List.fold_left
        (fun (i, x) e ->
          (i+1, f i x e))
        (0, x) l
    )

let foldi_right f l x =
  snd (
      List.fold_right
        (fun e (i, x) ->
          (i-1, f i e x))
        l
        (List.length l - 1, x)
    )

let rec bd = function
  | [] -> failwith "bd"
  | [e] -> []
  | h ::q -> h :: bd q

let rec ft = function
  | [] -> failwith "ft"
  | [e] -> e
  | _ :: q -> ft q
