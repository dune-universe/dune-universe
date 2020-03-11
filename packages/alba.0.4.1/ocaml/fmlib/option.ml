type 'a t = 'a option

let use (o:'a t) (b:'b) (f:'a -> 'b): 'b =
  match o with
  | None -> b
  | Some a -> f a

let fold (none:'z) (some:'a -> 'z): 'a t -> 'z = function
  | None -> none
  | Some a -> some a

let return (a:'a): 'a t =
  Some a

let (>>=) (m:'a t) (f:'a -> 'b t): 'b t =
  match m with
  | None -> None
  | Some a -> f a


let map (f:'a -> 'b) (o:'a t): 'b t =
  match o with
  | None -> None
  | Some a -> Some (f a)

let (>=>) (f:'a -> 'b t) (g:'b -> 'c t) (a:'a): 'c t =
  f a >>= g

let (<*>) (fo: ('a -> 'b) t) (o:'a t): 'b t =
  fo >>= fun f -> map f o


let join (oo:'a option option): 'a option =
  match oo with
  | None -> None
  | Some o -> o


let to_list (o: 'a t): 'a list =
  match o with
  | None ->
     []
  | Some v ->
     [v]


let has (o: 'a t): bool =
  match o with
  | None -> false
  | Some _ -> true


let value (o: 'a t): 'a =
  match o with
  | None ->
     assert false (* illegal call *)
  | Some x ->
     x


let of_bool (b:bool): unit t =
  if b then
    Some ()
  else
    None

let iter (f:'a -> unit) (m:'a t): unit =
  ignore (map f m)


let fold_interval (f:'a->int->'a t) (a0:'a) (start:int) (beyond:int): 'a t =
  assert (start <= beyond);
  let rec fold i a =
    if i = beyond then
      Some a
    else
      match f a i with
      | None ->
         None
      | Some a ->
         fold (i+1) a
  in
  fold start a0





let fold_array (f:'a->'b->int->'a t) (start:'a) (arr:'b array): 'a t =
  let len = Array.length arr
  in
  let rec fold a i =
    if i = len then
      Some a
    else
      match f a arr.(i) i with
       | Some a ->
          fold a (i+1)
       | None ->
          None
  in
  fold start 0
