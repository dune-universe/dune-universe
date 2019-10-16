type 'a t = Zero | Item of 'a | Plus of 'a t * 'a t

let zero = Zero

let of_item x = Item x

let plus a b =
  match a, b with
  | Zero, x -> x
  | x, Zero -> x
  | _ -> Plus (a, b)

let of_list list =
  List.fold_left (fun a b -> plus a (of_item b)) zero list

let flatten list =
  List.fold_left (fun a b -> plus a b) zero list

let rec fold_left f v x =
  match x with
  | Zero -> v
  | Item x -> f v x
  | Plus (a, b) -> fold_left f (fold_left f v a) b

let rec fold_right f x v =
  match x with
  | Zero -> v
  | Item x -> f x v
  | Plus (a, b) -> fold_right f a (fold_right f b v)

let rec hd x =
  match x with
  | Zero -> invalid_arg "Free_monoid.hd"
  | Item x -> x
  | Plus (a, _) -> hd a

class ['self] free_monoid = object (_ : 'self)
  method zero = zero

  method plus a b = plus a b
end
