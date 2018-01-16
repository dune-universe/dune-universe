open List

type 'a t = 'a list

(** {2 Renaming of non tail recursive functions} *)

let (@.) = (@)
let append_ntr = append
let concat_ntr = concat
let flatten_ntr = flatten
let map_ntr = map
let mapi_ntr = mapi
let fold_right_ntr = fold_right
let map2_ntr = map2
let fold_right2_ntr = fold_right2
let remove_assoc_ntr = remove_assoc
let remove_assq_ntr = remove_assq
let split_ntr = split
let combine_ntr = combine
let merge_ntr = merge

include struct
  (* This module is to stop using non tail recursive versions
     in this module by mistake. *)

  [@@@ ocaml.warning "-32"]

  let err = `Dont_use_this_non_tail_recursive_version

  let (@) = err
  let append = err
  let concat = err
  let flatten = err
  let map = err
  let mapi = err
  let fold_right = err
  let map2 = err
  let fold_right2 = err
  let remove_assoc = err
  let remove_assq = err
  let split = err
  let combine = err
  let merge = err
end

(** {2 Construction } *)

let empty = []

let singleton x = [x]

let make n x =
  if n < 0 then invalid_arg "List.make";
  let rec loop st = function
    | 0 -> st
    | n -> loop (x::st) (n-1)
  in
  loop [] n

let %TEST make = make 5 1 = [1;1;1;1;1]

let init n f =
  if n < 0 then invalid_arg "List.init";
  let rec loop st = function
    | -1 -> st
    | i -> loop (f i :: st) (i-1)
  in
  loop [] (n-1)

let %TEST init = init 5 (fun x -> x) = [0;1;2;3;4]

(** {2 Conversions} *)

let to_list t = t

let to_array = Array.of_list

let of_list t = t

let of_array = Array.to_list

(** {2 Find and assoc} *)

let rec find_opt f = function
  | [] -> None
  | x::_ when f x -> Some x
  | _::xs -> find_opt f xs

[%%TEST
  find_opt (fun x -> x = 3) [1;2;3;4;5] = Some 3;;
  find_opt (fun x -> x = 9) [1;2;3;4;5] = None;;
]

let rec find_map_opt f = function
  | [] -> None
  | x::xs ->
      match f x with
      | Some v -> Some v
      | None -> find_map_opt f xs

[%%TEST
  find_map_opt (fun x -> if x = 3 then Some "three" else None) [1;2;3;4;5] = Some "three";;
  find_map_opt (fun x -> if x = 3 then Some "three" else None) [5;6;7;8;9] = None;;
]

let assoc_all k =
  let rec aux rev = function
    | [] -> List.rev rev
    | (k',v)::kvs when k = k' -> aux (v::rev) kvs
    | _::kvs -> aux rev kvs
  in
  aux []

[%%TEST
  assoc_all 1 [(0,'a'); (1,'b'); (2,'c')] = ['b'];;
  assoc_all 1 [(0,'a'); (1,'b'); (1,'c')] = ['b'; 'c'];;
  assoc_all 3 [(0,'a'); (1,'b'); (2,'c')] = [];;
]

let rec assoc_opt k = function
  | [] -> None
  | (k',v)::_ when k = k' -> Some v
  | _::kvs -> assoc_opt k kvs

[%%TEST
  assoc_opt 1 [(0,'a'); (1,'b'); (2,'c')] = Some 'b';;
  assoc_opt 1 [(0,'a'); (1,'b'); (1,'c')] = Some 'b';;
  assoc_opt 3 [(0,'a'); (1,'b'); (2,'c')] = None;;
]

let remove_first_match p xs =
  let rec f st = function
    | [] -> xs
    | x::xs when p x -> rev_append st xs
    | x::xs -> f (x::st) xs
  in
  f [] xs

[%%TEST
  remove_first_match (fun x -> x = 3) [1;2;3;4;5]   = [1;2;4;5];;
  remove_first_match (fun x -> x = 3) [1;2;3;4;5;3] = [1;2;4;5;3];;
  remove_first_match (fun x -> x = 9) [1;2;3;4;5]   = [1;2;3;4;5];;
]

let remove_assoc k kvs = remove_first_match (fun (k',_) -> k = k') kvs

let remove_assq k kvs = remove_first_match (fun (k',_) -> k == k') kvs

(** {2 Folding} *)

let map f l = rev @@ rev_map f l

[%%TEST
  map (fun x -> x) [1;2;3] = [1;2;3]
]

let rev_mapi f xs =
  let rec loop st i = function
    | [] -> st
    | x::xs -> loop (f i x :: st) (i+1) xs
  in
  loop [] 0 xs

let mapi f l = rev @@ rev_mapi f l

[%%TEST
  mapi (fun i x -> (i,x)) [1;2;3] = [(0,1);(1,2);(2,3)]
]

let map2 f xs ys = rev @@ rev_map2 f xs ys

[%%TEST
  map2 (fun x y -> x + y) [1;2;3] [4;5;6] = [5;7;9]
]

let fold_right f xs st =
  let rec aux st = function
    | [] -> st
    | x::xs -> aux (f x st) xs
  in
  aux st @@ rev xs

[%%TEST
  fold_right (fun x xs -> x :: xs) [1;2;3;4;5] [6;7;8;9;10] = [1;2;3;4;5;6;7;8;9;10]
]

let fold_left1 f = function
  | [] -> invalid_arg "List.fold_left1"
  | x::xs -> fold_left f x xs

[%%TEST
  fold_left1 (-) [10;1;2;3;4] = 0 
]

let fold_right1 f = function
  | [] -> invalid_arg "List.fold_right1"
  | xs ->
      match rev xs with
      | [] -> assert false
      | x::xs -> fold_left f x xs

[%%TEST
  fold_right1 (-) [1;2;3;4;10] = 0 
]

let map_accum_left f acc xs =
  let rec loop acc ys = function
    | [] -> acc, rev ys
    | x::xs ->
        let acc, y = f acc x in
        loop acc (y::ys) xs
  in
  loop acc [] xs
    
(** {2 Transformation} *)

let append t1 t2 = rev_append (rev t1) t2

let (@) = append

[%%TEST
  [1;2;3] @ [4;5;6] = [1;2;3;4;5;6]      
]

let concat = function
  | [] -> []
  | ts -> fold_left1 append ts

[%%TEST
  concat [ [1]; [2;3]; [4;5;6] ] = [1;2;3;4;5;6]          
]
    
let flatten = concat

(** {2 Iteration} *)

(* Deprecated *)
let iter_until f xs =
  let g = function
    | [] -> `Break None
    | x::xs ->
        match f x with
        | `Break st -> `Break (Some st)
        | `Continue -> `Continue xs
  in
  Base.loop g xs

let rev_filter_map f lst =
  fold_left (fun st x ->
    match f x with
    | Some v -> v :: st
    | None -> st) [] lst

(** mapMaybe of Haskell *)
let filter_map f lst = rev (rev_filter_map f lst)

[%%TEST
  filter_map (fun x -> if x mod 2 = 0 then Some (x+1) else None) [1;2;3;4;5] = [3;5]
]
  
(** concatMap of Haskell *)
let rev_concat_map f l =
  let rec loop st = function
    | [] -> st
    | x::xs -> loop (rev_append (f x) st) xs
  in
  loop [] l

[%%TEST
  rev_concat_map (fun x -> [x; x+1]) [1;2;3] = [4;3;3;2;2;1]
]

let concat_map f l = rev (rev_concat_map f l)

[%%TEST
  concat_map (fun x -> [x; x+1]) [1;2;3] = [1;2;2;3;3;4]
]

let take n xs =
  let rec take_ n st xs =
    if n <= 0 then st
    else match xs with
    | [] -> st
    | x::xs -> take_ (n-1) (x::st) xs
  in
  rev (take_ n [] xs)

[%%TEST
  take (-1) [1;2;3] = [];;
  take 0 [1;2;3]    = [];;
  take 1 [1;2;3]    = [1];;
  take 3 [1;2;3]    = [1;2;3];;
  take 4 [1;2;3]    = [1;2;3];;
]

let rec drop n xs =
  if n <= 0 then xs
  else match xs with
  | [] -> []
  | _::xs -> drop (n-1) xs

[%%TEST
  drop (-1) [1;2;3] = [1;2;3];;
  drop 0 [1;2;3]    = [1;2;3];;
  drop 1 [1;2;3]    = [2;3];;
  drop 3 [1;2;3]    = [];;
  drop 4 [1;2;3]    = [];;
]

let sub_default xs pos len =
  (* This is not simple [take len @@ drop pos xs] *)
  let pos, len =
    if pos < 0 then 0, len + pos else pos, len
  in
  take len @@ drop pos xs

[%%TEST
  sub_default [0;1;2;3;4;5]   1    2  = [1;2];;
  sub_default [0;1;2;3;4;5] (-1)   3  = [0;1];; (* tricky *)
  sub_default [0;1;2;3;4;5]   1  (-1) = [];;
  sub_default [0;1;2;3;4;5] (-1) (-1) = [];;
  sub_default [0;1;2;3;4;5]   3    4  = [3;4;5];;
  sub_default [0;1;2;3;4;5]   9    4  = [];;
  sub_default [0;1;2;3;4;5]   9  (-1) = [];;
] 
    
let take_exn len xs =
  if len < 0 then invalid_arg "List.take_strict";
  let rec loop st len xs =
    if len = 0 then rev st
    else match xs with
    | [] -> invalid_arg "List.take_strict"
    | x::xs -> loop (x::st) (len-1) xs
  in
  loop [] len xs

[%%TEST
  must_raise @@ fun () -> take_exn (-1) [1;2;3];;
  take_exn 0 [1;2;3]    = [];;
  take_exn 1 [1;2;3]    = [1];;
  take_exn 3 [1;2;3]    = [1;2;3];;
  must_raise @@ fun () -> take_exn 4 [1;2;3];;
]

let drop_exn len xs =
  if len < 0 then invalid_arg "List.drop_strict";
  let rec loop len xs =
    if len = 0 then xs
    else match xs with
    | [] -> invalid_arg "List.drop_strict"
    | _::xs -> loop (len-1) xs
  in
  loop len xs

[%%TEST
  must_raise @@ fun () -> drop_exn (-1) [1;2;3];;
  drop_exn 0 [1;2;3]    = [1;2;3];;
  drop_exn 1 [1;2;3]    = [2;3];;
  drop_exn 3 [1;2;3]    = [];;
  must_raise @@ fun () -> drop_exn 4 [1;2;3];;
]

let sub xs pos len = take_exn len @@ drop_exn pos xs

[%%TEST
  sub [0;1;2;3;4;5]   1    2  = [1;2];;
  must_raise @@ fun () -> sub [0;1;2;3;4;5] (-1) 2;;
  must_raise @@ fun () -> sub [0;1;2;3;4;5]   1  (-1);;
  must_raise @@ fun () -> sub [0;1;2;3;4;5] (-1) (-1);;
  must_raise @@ fun () -> sub [0;1;2;3;4;5]   3    4;;
  must_raise @@ fun () -> sub [0;1;2;3;4;5]   9    4;;
  must_raise @@ fun () -> sub [0;1;2;3;4;5]   9  (-1);;
] 

  
let split_at n xs =
  let rec split_at_ n st xs =
    if n <= 0 then st, xs
    else match xs with
    | [] -> st, []
    | x::xs -> split_at_ (n-1) (x::st) xs
  in
  let r, dropped = split_at_ n [] xs in
  rev r, dropped

[%%TEST
  split_at 2    []      = ([], []);;
  split_at 2    [1]     = ([1], []);;
  split_at 2    [1;2]   = ([1;2], []);;
  split_at 2    [1;2;3] = ([1;2], [3]);;
  split_at (-1) [1;2;3] = ([], [1;2;3]);;
  split_at 10   [1;2;3] = ([1;2;3], []);;
]

let replicate xs n =
  if n < 0 then invalid_arg "List.replicate";
  match n with
  | 0 -> []
  | 1 -> xs
  | n ->
      let xs' = rev xs in
      let rec loop st = function
        | 0 -> st
        | n -> loop (rev_append xs' st) (n-1)
      in
      loop [] n

[%%TEST
  replicate [1;2;3] 3 = [1;2;3;1;2;3;1;2;3];;
  replicate [1;2;3] 0 = [];;
  replicate [1;2;3] 1 = [1;2;3];;
]  
    
let span p xs =
  let rec span_ st = function
    | [] -> rev st, []
    | x::xs when p x -> span_ (x::st) xs
    | l -> rev st, l
  in
  span_ [] xs

[%%TEST
  span (fun x -> x < 3) [1;2;3;4;1;2;3;4] = ([1;2], [3;4;1;2;3;4]);;
  span (fun x -> x < 9) [1;2;3] = ([1;2;3], []);;
  span (fun x -> x < 0) [1;2;3] = ([], [1;2;3]);;    
]  
    
let partition_map f xs =
  let rec part left right = function
    | [] -> rev left, rev right
    | x::xs ->
        match f x with
        | `Left v -> part (v::left) right xs
        | `Right v -> part left (v::right) xs
  in
  part [] [] xs

[%%TEST
  partition_map (fun x ->
      if x mod 2 = 0 then `Left (string_of_int x) else `Right x) [1;2;3;4;5]
    = (["2"; "4"], [1;3;5])
]  
    
let uniq_dup eq fs =
  let rec loop firsts dups = function
    | [] -> rev firsts, rev dups
    | x::xs ->
        let firsts', dups' =
          match find (eq x) firsts with
          | found -> firsts, (x,found)::dups
          | exception Not_found -> x::firsts, dups
        in
        loop firsts' dups' xs
  in
  loop [] [] fs

let %TEST uniq_dup =
  uniq_dup (fun x y -> x mod 3 = y mod 3) [1;2;3;4;5;6;7;8;9]
  = ([1;2;3], [(4,1); (5,2); (6,3); (7,1); (8,2); (9,3)])

let unique_by f xs =
  let rec unique st = function
    | [] -> rev st
    | x::xs ->
        let st' = match find_opt (f x) st with
          | Some _ -> st
          | None -> x::st
        in
        unique st' xs
  in
  unique [] xs

let unique xs = unique_by (=) xs

[%%TEST
  unique [1;2;3;4;5;6;1;2;3] = [1;2;3;4;5;6]
]    
    
let uniq_dup_sorted cmp xs =
  let rec f rev_st rev_dups = function
    | [] -> rev rev_st, rev rev_dups
    | [x] -> rev (x::rev_st), rev rev_dups
    | x1::(x2::xs as x2xs) ->
        begin match cmp x1 x2 with
        | 0 -> f rev_st ((x1,x2)::rev_dups) (x1::xs)
        | -1 -> f (x1::rev_st) rev_dups x2xs
        | _ -> invalid_arg "unique_sorted: list is not sorted"
        end
  in
(*
    let us, ds = f [] [] xs in
    assert (length us + length ds = length xs);
    us, ds
*)
  f [] [] xs

let is_unique key xs =
  let rec is_unique st = function
    | [] -> None
    | x::xs ->
        let k = key x in
        try
          let x' = assoc k st in
          Some (x,x')
        with Not_found -> is_unique ((k,x)::st) xs
  in
  is_unique [] xs
    
let has_dup f xs =
  let rec loop st = function
    | [] -> None
    | x::xs ->
        match find_opt (f x) st with
        | Some y -> Some (y, x)
        | None -> loop (x::st) xs
  in
  loop [] xs

[%%TEST
  has_dup (fun x y -> fst x = fst y) [(1,2); (2,3); (4,5)] = None;;
  has_dup (fun x y -> fst x = fst y) [(1,2); (2,3); (2,5)] = Some ( (2,3), (2,5) );;
]
    
let intersperse x ys =
  let rec f st = function
    | [] -> st
    | y::[] -> y::st
    | y::ys -> f (x::y::st) ys
  in
  f [] @@ rev ys

[%%TEST
  intersperse "," [] = [];;
  intersperse "," ["1"; "2"; "3"] = ["1"; ","; "2"; ","; "3"];;
]
    
let rec last = function
  | [] -> failwith "Xlist.last"
  | [x] -> x
  | _::xs -> last xs

[%%TEST
  must_raise @@ fun () -> last [];;
  last [1;2;3] = 3;;
]

(* Deprecated *)
let scani_left f acc xs =
  let rec scan acc pos xs = match pos, xs with
    | _pos, [] -> acc
    | pos, x::xs ->
        match f pos acc x with
        | `Continue acc -> scan acc (pos+1) xs
        | `Stop acc -> acc
  in
  scan acc 0 xs

let sum xs = fold_left (+) 0 xs

[%%TEST
  sum [] = 0;;
  sum [1;2;3;4;5] = 15;;
]
  
let rev_group_by eq rev_xs =
  let rec grouping gs cur_group = function
    | [] ->
        begin match cur_group with
        | [] -> gs
        | _ -> cur_group :: gs
        end
    | x::xs ->
        match cur_group with
        | [] -> grouping gs [x] xs
        | y::_ ->
            if eq x y then grouping gs (x::cur_group) xs
            else grouping (cur_group::gs) [x] xs
  in
  grouping [] [] rev_xs

let group_by eq xs = rev_group_by eq (rev xs)

let group xs = group_by (=) xs

[%%TEST
  group [1;2;3] = [[1];[2];[3]];;
  group [1;2;3;3;4;4;5;5;6] = [[1];[2];[3;3];[4;4];[5;5];[6]];;
  group [1;2;3;3;4;4;5;5;6;6] = [[1];[2];[3;3];[4;4];[5;5];[6;6]];;
  group [1;1;2;3;3;4;4;5;5;6;6] = [[1;1];[2];[3;3];[4;4];[5;5];[6;6]];;
  group [1;1;2;3;3;4;4;5;5;1;1;6;6] = [[1;1];[2];[3;3];[4;4];[5;5];[1;1];[6;6]];;
]

let sort_then_group_by compare xs =
  rev_group_by (fun x y -> compare x y = 0) (sort (fun x y -> - (compare x y)) xs)

(* Deprecated *)
let sort_then_group = sort_then_group_by
    
let merge cmp xs ys =
  let rec f st xs ys =
    match xs, ys with
    | [], [] -> rev st
    | [], ys -> rev_append st ys
    | xs, [] -> rev_append st xs
    | (x::xs as xxs), (y::ys as yys) ->
        match cmp x y with
        | -1 (* x < y *) -> f (x::st) xs yys
        | 1  (* x > y *) -> f (y::st) xxs ys
        | 0  (* x = y. x comes first *) -> f (y::x::st) xs ys
        | _ -> assert false
  in
  f [] xs ys

[%%TEST
  merge compare [1;3;5] [2;4;6] = [1;2;3;4;5;6];;
  merge compare [1;3;5] [] = [1;3;5];;
  merge compare [] [1;3;5] = [1;3;5];;
  merge compare [] [1;3;5] = [1;3;5];;

  merge (fun (x,_) (y,_) -> compare x y) [(1,1); (2,0);] [(2,1); (3,3)]
    = [(1,1); (2,0); (2,1); (3,3)];;
]
    
let splits_by max items =
  assert (max > 0);
  let rec loop rev items =
    match split_at max items with
    | [], [] -> rev
    | [], _ -> assert false
    | xs, [] -> xs::rev
    | xs, ys -> loop (xs::rev) ys
  in
  rev (loop [] items)

[%%TEST
  splits_by 2 [] = [];;
  splits_by 2 [1] = [[1]];;
  splits_by 2 [1;2] = [[1;2]];;
  splits_by 2 [1;2;3] = [[1;2]; [3]];;
]
    
let accum xsref x = xsref := x :: !xsref
let (+::=) = accum

let zip xs ys =
  let rec zip st xs ys =
    match xs, ys with
    | [], _ | _ ,[] -> rev st
    | x::xs, y::ys -> zip ((x,y)::st) xs ys
  in
  zip [] xs ys

[%%TEST
  zip [] [] = [];;
  zip [1;2;3] [4;5;6]   = [(1,4);(2,5);(3,6)];;
  zip [1;2;3;4] [4;5;6] = [(1,4);(2,5);(3,6)];;
  zip [1;2;3] [4;5;6;7] = [(1,4);(2,5);(3,6)];;
]
    
let split xys =
  let rec f xs ys = function
    | [] -> rev xs, rev ys
    | (x,y)::xys -> f (x::xs) (y::ys) xys
  in
  f [] [] xys

[%%TEST
  split [] = ([], []);;
  split [(1,4);(2,5);(3,6)] = ([1;2;3], [4;5;6]);;
]

(** {2 Integer ranges} *)

let from_to f t =
  (* CR jfuruse: we should build from 'to' *)
  let rec from_to st f t =
    if f > t then rev st
    else from_to (f::st) (f+1) t
  in
  from_to [] f t

let (--) = from_to

[%%TEST
  0--9 = [0;1;2;3;4;5;6;7;8;9];;
  0--0 = [0];;
  9--0 = [];;
]

let init_from_to f t fn =
  let rec loop st i =
    if i > t then rev st
    else loop (fn i :: st) (i+1)
  in
  loop [] f

(** {2 Modules} *)

module Infix = struct
  let (--) = (--)
  let (+::=) = (+::=)
end

module Pervasives = Infix
