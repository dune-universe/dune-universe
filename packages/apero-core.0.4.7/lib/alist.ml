include List

let drop = Acommon.drop

let take = Acommon.take

let contains xs x = match List.find_opt  ((=) x) xs with 
| Some _ -> true 
| None -> false

let rec substract xs ys = match ys with
| [] -> xs
| h::tl -> substract (List.filter ((=) h) xs) tl

let zip = combine

let unzip = split

let  to_string ?(sep=", ") xs f =
  let s = match xs with
      | h::tl -> List.fold_left (fun a v -> Printf.sprintf "%s%s%s" a sep @@ f v) (f h) tl
      | [] -> ""
  in Printf.sprintf "(%s)" s          
