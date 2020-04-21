include Stdlib.List
include Module_types

type 'a t = 'a list

let return (a:'a): 'a t = [a]

let rec (>>=) (l:'a t) (f:'a -> 'b t): 'b t =
  match l with
  | [] ->
     []
  | hd :: tl ->
     f hd @ (tl >>= f)

let (>=>) (f:'a -> 'b t) (g:'b -> 'c t) (a:'a): 'c t =
  f a >>= g


let (<*>) (flst: ('a -> 'b) t) (lst:'a t): 'b t =
  flst >>= fun f -> map f lst

let join = concat

let find (p:'a -> bool) (l:'a t): 'a option =
  try
    Some (find p l)
  with Not_found ->
    None



let nth (i: int) (lst: 'a t): 'a option =
    nth_opt lst i


let nth_strict (i: int) (lst: 'a t): 'a =
    match nth i lst with
    | None ->
        assert false (* Illegal call! *)
    | Some e ->
        e



let map_and_filter (f:'a -> 'b option) (l:'a list): 'b list =
  let rec map = function
    | [] ->
       []
    | hd :: tl ->
       match f hd with
       | None ->
          map tl
       | Some b ->
          b :: map tl
  in
  map l



let split_at (p:'a -> bool) (l: 'a t): 'a t * 'a t =
  let rec split prefix rest =
    match rest with
    | [] ->
       rev prefix, rest
    | hd :: tl  ->
       if p hd then
         rev prefix, rest
       else
         split (hd :: prefix) tl
  in
  split [] l




module Monadic_fold (M:MONAD) =
  struct
    let foldi_left (f:int -> 'a -> 'b -> 'b M.t) (l:'a t) (start:'b)
        : 'b M.t =
      let rec foldi i l start =
        match l with
        | [] ->
           M.return start
        | hd :: tl ->
           M.(f i hd start >>= foldi (i+1) tl)
      in
      foldi 0 l start

    let fold_left (f:'a -> 'b -> 'b M.t) (l:'a t) (start:'b): 'b M.t =
      foldi_left (fun _ -> f) l start

    let fold_right (f:'a -> 'b -> 'b M.t) (l:'a t) (start:'b): 'b M.t =
      fold_left f (rev l) start
  end
