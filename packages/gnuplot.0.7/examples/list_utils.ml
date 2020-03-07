open Base

let sample l ~size =
  let source = Array.of_list l in
  let len = List.length l in
  List.init size ~f:(fun _ -> source.(Random.int len))
;;

let group_by l ~f =
  List.sort l ~compare:(fun x1 x2 -> Poly.compare (f x1) (f x2))
  |> List.group ~break:(fun x1 x2 -> not (Poly.equal (f x1) (f x2)))
;;

let scan l ~init ~f =
  let rec loop l s acc =
    match l with
    | [] -> List.rev acc
    | a :: t ->
      let s = f s a in
      loop t s (s :: acc)
  in
  loop l init [init]
