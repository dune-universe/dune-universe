open Core_kernel

type 'a t = Cons of 'a * 'a list

let init n ~f =
  if n < 1 then invalid_arg "Non_empty_list should be at least of length 1" ;
  Cons (f 0, List.init (n - 1) ~f:(fun i -> f (i + 1)))

let cons h t = Cons (h, t)

let fold (Cons (h, t)) ~init ~f =
  List.fold t ~f ~init:(f init h)

let map (Cons (h, t)) ~f =
  cons (f h) (List.map t ~f)

let iter (Cons (h, t)) ~f =
  f h ; List.iter t ~f

let to_list (Cons (h, t)) =
  h :: t

let filter_map (Cons (h, t)) ~f =
  match f h, List.filter_map t ~f with
  | None, [] -> None
  | None, h :: t -> Some (Cons (h, t))
  | Some h, l -> Some (Cons (h, l))

let for_all (Cons (h, t)) ~f =
  f h && List.for_all t ~f

let exists (Cons (h, t)) ~f =
  f h || List.exists t ~f

let hd (Cons (h, _)) = h

let singleton x = Cons (x, [])

let of_list_exn = function
  | [] -> failwith "empty list"
  | h :: t -> Cons (h, t)
