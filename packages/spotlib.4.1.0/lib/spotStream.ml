open Base
open Xlazy

type 'a t = 'a desc lazy_t

and 'a desc =
  | Cons of 'a * 'a t
  | Null

let null = from_val Null
let cons v t = from_val (Cons (v, t))
let (^^) = cons
let singleton v = cons v null

let peek = function
  | lazy Null -> None
  | lazy (Cons (v, t)) -> Some (v, t)

let is_null = function
  | lazy Null -> true
  | _ -> false

let rec create f st = lazy (match f st with
  | Some (v, st) -> Cons (v, create f st)
  | None -> Null)

let rec of_list = function
  | [] -> null
  | x::xs -> cons x (of_list xs)

let to_list t = 
  let rec to_list st = function
    | lazy Null -> List.rev st
    | lazy (Cons (v, t)) -> to_list (v :: st) t
  in
  to_list [] t
  
let hd = function
  | lazy Null -> failwith "hd"
  | lazy (Cons (x, _)) -> x

let tl = function
  | lazy Null -> failwith "tl"
  | lazy (Cons (_, xs)) -> xs

let rec nth t n = 
  if n < 0 then invalid_arg "Stream.nth"
  else
    match t with
    | lazy Null -> failwith "Stream.nth"
    | lazy (Cons (x,xs)) ->
        if n = 0 then x
        else nth xs (n-1)

let rec init t = lazy (match t with
  | lazy Null -> failwith "Stream.init"
  | lazy (Cons (_, lazy Null)) -> Null
  | lazy (Cons (x, xs)) -> Cons (x, init xs))

let rec length = function
  | lazy Null -> 0
  | lazy (Cons (_, xs)) -> length xs + 1

let rec iter f = function
  | lazy Null -> ()
  | lazy (Cons (v, t)) -> f v; iter f t

let rec fold_left f st t = lazy (match t with
  | lazy Null -> !!st
  | lazy (Cons (v, t)) -> !!(fold_left f (f st v) t))

let rec fold_right f xs st =
  lazy (match xs with
  | lazy Null -> Lazy.force st
  | lazy (Cons (x,xs)) -> Lazy.force (f x (fold_right f xs st)))

let rec map f lst = lazy (match lst with
  | lazy Null -> Null
  | lazy (Cons (v, lst')) -> Cons (f v, map f lst'))

let rec append xs ys = lazy (match xs with
  | lazy Null -> !!ys
  | lazy (Cons (x, xs)) -> Cons (x, append xs ys))
  
  
let rev t = fold_left (fun st x -> x ^^ st) null t

let intersparse a t = lazy (match t with
  | lazy Null -> Null
  | lazy (Cons (_, lazy Null) as singleton) -> singleton
  | lazy (Cons (x, xs)) -> Cons (x, from_val (Cons (a, xs)))) 

let rec concat xss = lazy (match xss with
  | lazy Null -> Null
  | lazy (Cons (x, xs)) -> !! (append x (concat xs)))

let intercalate xs xss = concat (intersparse xs xss)

(*
  transpose :: [[a]] -> [[a]]Source

  The transpose function transposes the rows and columns of its argument. For example,

  transpose [[1,2,3],[4,5,6]] == [[1,4],[2,5],[3,6]]
  subsequences :: [a] -> [[a]]Source

  The subsequences function returns the list of all subsequences of the argument.

  subsequences "abc" == ["","a","b","ab","c","ac","bc","abc"]
  permutations :: [a] -> [[a]]
*)

let rec fold_left' f st = function
  | lazy Null -> st
  | lazy (Cons (v, t)) -> fold_left' f (f v st) t

let fold_left1 f t = match t with
  | lazy Null -> failwith "fold_left1"
  | lazy (Cons (v, t)) -> fold_left f v t

let rec fold_right1 f lst st = lazy (match lst with
  | lazy Null -> failwith "fold_right1"
  | lazy (Cons (v, lazy Null)) -> v
  | lazy (Cons (v, lst)) -> Lazy.force (f v (fold_right1 f lst st)))

let rec mem k t = match t with
  | lazy Null -> false
  | lazy (Cons (v, t)) -> if k = v then true else mem k t

let concat tss = lazy (match tss with
  | lazy Null -> Null
  | lazy (Cons (ts,tss)) -> !! (append ts (concat tss)))

let filter p xs = fold_right (fun x st -> if p x then cons x st else st) xs null

let filter_map p xs = 
  fold_right (fun x st -> 
    match  p x with 
    | None -> st
    | Some x -> cons x st) 
    xs null

let rec take n xs = lazy (
  if n <= 0 then Null
  else match xs with
  | lazy Null -> Null
  | lazy (Cons (x,xs)) -> Cons (x, take (n-1) xs)
)

(* [t2] must be a postfix of [t1] otherwise, it loops forever *)
let rev_between t1 t2 =
  let rec loop st t =
    if t == t2 then st (* CR jfuruse: we cannot always use pointer eq *)
    else 
      match t with
      | lazy (Cons (v, t')) -> loop (v::st) t'
      | lazy Null -> st
  in
  loop [] t1

let between t1 t2 = List.rev (rev_between t1 t2)

let split_at len t = 
  let rec split rev_list len t = 
    if len <= 0 then List.rev rev_list, t
    else 
      match t with
      | lazy Null -> List.rev rev_list, null
      | lazy (Cons (v, t)) -> split (v::rev_list) (len-1) t
  in
  split [] len t
    
(*
  let rec split_at' : int -> 'a t -> 'a t * 'a t = fun len t ->
  let ztuple : ('a t * 'a t) Lazy.t = lazy (
  if len <= 0 then null, t
  else match t with
  | lazy Null -> null, null
  | lazy (Cons (v, t)) -> 
  let pref, post = split_at' (len-1) t in
  v^^pref, post
  )
  in
  lazy(!!(fst !!ztuple)),
  lazy(!!(snd !!ztuple))
*)

let rec split_at' : int -> 'a t -> 'a t * 'a t = fun len t ->
  let ztuple : ('a t * 'a t) Lazy.t = lazy (
    if len <= 0 then null, t
    else match t with
    | lazy Null -> null, null
    | lazy (Cons (v, t)) -> 
        let pref, post = split_at' (len-1) t in
        v^^pref, post
  )
  in
  lazy(!!(fst !!ztuple)),
  lazy(!!(snd !!ztuple))

let _test_split_at' () = 
  let rec list = 
    function 
      | 0 -> null
      | i -> lazy (print_int i; print_newline (); Cons (i, list (i-1)))
  in
  let my = split_at' 3 (list 10) in
  print_endline "forcing fst";
  ignore & Lazy.force (fst my);
  print_endline "forcing snd";
  ignore & Lazy.force (snd my);
  ()

(** {6 Monadic interface} *)
include Monad.Make(struct
  type 'a _t = 'a t
  type 'a t = 'a _t
  let return a = singleton a
  let bind t f = concat (map f t)
end)

let %TEST fold_right_and_map_ =
  let zeros = create (fun () -> Some (0, ())) () in
  let ones = fold_right (fun z st -> (z+1)^^st) zeros null in
  let ones' = map (fun z -> z + 1) zeros in
  assert (to_list (take 3 ones) = [1; 1; 1]);
  assert (to_list (take 3 ones') = [1; 1; 1])
