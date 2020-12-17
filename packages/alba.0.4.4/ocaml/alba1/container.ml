(* Copyright (C) Helmut Brandl  <helmut dot brandl at gmx dot net>

   This file is distributed under the terms of the GNU General Public License
   version 2 (GPLv2) as published by the Free Software Foundation.
*)

module Search: sig
  val binsearch_max: 'a -> 'a array -> int
  val binsearch:     'a -> 'a array -> int
  val array_find_min: ('a -> bool) -> 'a array -> int
end = struct
  let binsearch_max (el:'a) (arr: 'a array) =
    (** The maximal index where [el] can be inserted into the array [arr]
        without disturbing the order.

        The algorithm assumes that the array is sorted *)
    let len = Array.length arr
    in
    (* all k: 0<=k<=i => arr.(k)<=el   *)
    (*        j<=k<len => el < arr.(k) *)
    let rec search i j =
      assert (0<=i); assert (i<=j); assert (j<=len);
      if (j-i) <= 1 then
        if i<j && el<arr.(i) then i else j
      else
        let m = i + (j-i)/2
        in
        if arr.(m)<=el then search m j
        else                search i m
    in
    let idx = search 0 len
    in
    assert (0<=idx && idx<=Array.length arr);
    idx


  let binsearch (el:'a) (arr: 'a array) =
    let insert_pos = binsearch_max el arr in
    if insert_pos = 0 || arr.(insert_pos-1) <> el then
      raise Not_found
    else
      insert_pos - 1


  let array_find_min (p:'a -> bool) (arr: 'a array) =
    let len = Array.length arr in
    let rec search i =
      if i=len then raise Not_found
      else begin
        assert (0<=i); assert (i<len);
        if p arr.(i)  then i
        else search (i+1)
      end
    in
    search 0
end




module IntSet = Set.Make(struct
  let compare = Stdlib.compare
  type t = int
end)

module IntMap = Map.Make(struct
  let compare = Stdlib.compare
  type t = int
end)


let string_of_intset (set:IntSet.t): string =
  "{"
  ^ String.concat
      ","
      (List.map string_of_int (IntSet.elements set))
  ^ "}"



let string_of_intlist (lst:int list): string =
  "["
  ^ String.concat
      ","
      (List.map string_of_int lst)
  ^ "]"


let string_of_intarray (arr:int array): string =
  string_of_intlist (Array.to_list arr)


let interval_fold (f:'a->int->'a) (a0:'a) (start:int) (beyond:int): 'a =
  assert (start <= beyond);
  let rec fold i a =
    if i = beyond then
      a
    else
      fold (i+1) (f a i)
  in
  fold start a0



let interval_find (f:int -> bool) (start:int) (beyond:int): int =
  assert (start <= beyond);
  let rec find i =
    if beyond <= i then
      raise Not_found
    else if f i then
      i
    else
      find (i+1)
  in
  find start


let interval_exist (f: int -> bool) (start:int) (beyond:int): bool =
  assert (start <= beyond);
  try ignore (interval_find f start beyond); true
  with Not_found -> false


let interval_for_all (f: int -> bool) (start:int) (beyond:int): bool =
  assert (start <= beyond);
  not (interval_exist (fun i -> not (f i)) start beyond)


let interval_iter (f:int -> unit) (start:int) (beyond:int): unit =
  assert (start <= beyond);
  let rec iter i =
    if i = beyond then
      ()
    else begin
      f i;
      iter (i+1)
    end
  in
  iter start




module StringSet = Set.Make(struct
  let compare = Stdlib.compare
  type t = string
end)

module StringMap = Map.Make(struct
  let compare = Stdlib.compare
  type t = string
end)



module Mylist: sig

  val is_empty:     'a list -> bool
  val is_singleton: 'a list -> bool
  val iteri:        (int -> 'a -> unit) -> 'a list -> unit
  val mapi:         (int -> 'a -> 'b) -> 'a list -> 'b list

  val sum:          ('a -> int) -> int -> 'a list -> int

  val has: ('a -> bool) -> 'a list -> bool
  val has_duplicates: 'a list -> bool

  val combine:      'a list -> 'b list -> ('a*'b) list
  val split_at:     int -> 'a list -> 'a list * 'a list
  val split_condition: (int -> 'a -> bool) -> 'a list -> 'a list * 'a list

  val find2: ('a -> 'b -> bool) -> 'a list -> 'b list -> 'a * 'b

  val take: int -> 'a list -> 'a list
  val take_rev: int -> 'a list -> 'a list
  val filter_rev: ('a -> bool) -> 'a list -> 'a list
end = struct

  let is_empty (l:'a list): bool = match l with [] -> true | _ -> false

  let is_singleton (l: 'a list): bool =
    match l with
      [_] -> true
    | _   -> false

  let iteri (f:int->'a->unit) (l:'a list): unit =
    let pos = ref 0 in
    let rec itrec l =
      match l with
        [] -> ()
      | h::t -> begin
          f !pos h;
          pos := !pos + 1;
          itrec t
      end
    in
    itrec l

  let mapi (f:int->'a->'b) (l:'a list): 'b list =
    let rec maprec l i =
      match l with
        [] -> []
      | h::tl -> (f i h)::(maprec tl (i+1))
    in
    maprec l 0

  let sum (f:'a->int) (start:int) (l:'a list): int =
    List.fold_left (fun cum e -> cum + f e) start l

  let has (f:'a->bool)  (lst:'a list): bool =
    try
      ignore(List.find f lst);
      true
    with Not_found ->
      false

  let rec has_duplicates (l:'a list): bool =
    match l with
      [] -> false
    | h::t ->
        List.mem h t || has_duplicates t

  let combine (l1: 'a list) (l2: 'b list): ('a*'b) list =
    let rec comb l1 l2 res =
      match l1, l2 with
        [], _  -> res
      | _ , [] -> res
      | a::l1, b::l2 -> comb l1 l2 ((a,b)::res)
    in
    List.rev (comb l1 l2 [])

  let split_at (i:int) (l:'a list): 'a list * 'a list =
    let rec split i l1_rev l2 =
      if i = 0 then
        List.rev l1_rev, l2
      else
        match l2 with
        | hd::tl ->
           split (i-1) (hd::l1_rev) tl
        | _ ->
           assert false (* i out of bound *)
    in
    split i [] l


  let split_condition (f:int -> 'a -> bool) (l:'a list): 'a list * 'a list =
    (* split the list into [l1] and [l2] so that [rev l1 @ l2 = l] and the
       first element [hd] of [l2] (if exists) is at position [i] and [f i hd]
       is satisfied. *)
    let l1 = ref []
    and l2 = ref l
    and i = ref 0
    and go_on = ref true
    in
    while !go_on do
      match !l2 with
      | hd :: tl when not (f !i hd) ->
         l1 := hd :: !l1;
         l2 := tl;
         i := !i + 1
      | _ ->
         go_on := false
    done;
    !l1, !l2


  let find2 (f:'a -> 'b -> bool) (l1:'a list) (l2:'b list): 'a * 'b =
    let rec find l1 l2 =
      match l1, l2 with
      | [], _ | _, [] ->
         raise Not_found
      | h1::t1, h2::t2 ->
         if f h1 h2 then
           h1, h2
         else
           find t1 t2
    in
    find l1 l2

  let take_rev (n:int) (lst:'a list): 'a list =
    let rec take n lst acc =
      if n = 0 then
        acc
      else
        match lst with
        | hd :: tl ->
           take (n-1) tl (hd::acc)
        | _ ->
           assert false (* called with illegal argument, list must have
                           at least n elements *)
    in
    take n lst []

  let take (n:int) (lst:'a list): 'a list =
    List.rev (take_rev n lst)

  let filter_rev (f:'a->bool) (lst:'a list): 'a list =
    List.fold_left
      (fun res e ->
        if f e then
          e :: res
        else
          res
      )
      []
      lst
end




module Myarray: sig

  val combine: 'a array -> 'b array -> ('a*'b) array
  val split:   ('a*'b) array -> 'a array * 'b array
  val sum: int -> int array -> int
  val remove_duplicates: 'a array -> 'a array

end = struct

  let combine (a: 'a array) (b: 'b array): ('a*'b) array =
    let n = Array.length a in
    assert (n = Array.length b);
    Array.init n (fun i -> a.(i),b.(i))

  let split (c: ('a*'b) array): 'a array * 'b array =
    let a = Array.map (fun (x,_) -> x) c
    and b = Array.map (fun (_,y) -> y) c in
    a,b

  let sum (start:int) (arr:int array): int =
    Array.fold_left (fun sum n -> sum + n) start arr

  let remove_duplicates (arr:'a array): 'a array =
    (Array.fold_left
       (fun lst e ->
         if List.mem e lst then
           lst
         else
           e :: lst
       )
       []
       arr)
    |> List.rev |> Array.of_list
end




module Mystring: sig
  val rev_split: string -> char -> string list
  val split: string -> char -> string list
  val for_all: (char -> bool) -> string -> bool
  val to_list: string -> char list
  val of_list: char list -> string
end = struct
  let rev_split (str:string) (sep:char): string list =
    let start    = ref 0
    and lst      = ref []
    and len      = String.length str
    in
    while !start < len do
      try
        let nxt = String.index_from str !start sep in
        if !start < nxt then
          lst := (String.sub str !start (nxt - !start)) :: !lst;
        start := nxt + 1
      with Not_found ->
        lst := (String.sub str !start (len - !start)) :: !lst;
        start := len
    done;
    !lst

  let split (str:string) (sep:char): string list =
    List.rev (rev_split str sep)

  let for_all (f:char->bool) (s:string): bool =
    let n = String.length s in
    let rec forall i =
      if i = n then
        true
      else
        f s.[i] && forall (i+1)
    in
    forall 0

  let of_char (c:char): string =
    String.make 1 c

  let to_list (s:string): char list =
    let rec list cs i =
      if i = 0 then
        cs
      else
        let j = i - 1 in
        list (s.[j]::cs) j
    in
    list [] (String.length s)

  let of_list (cs:char list): string =
    let rec str cs i =
      match cs with
      | [] ->
         Bytes.create i
      | c::cs ->
         let bs = str cs (i+1) in
         Bytes.set bs i c;
         bs
    in
    let bs = str cs 0 in
    Bytes.unsafe_to_string bs
end



module Array2:
sig
  type ('a, 'b) t
  val empty: ('a,'b) t
  val make: 'a array -> 'b array -> ('a,'b) t
  val from_pair: ('a array * 'b array) -> ('a,'b) t
  val copy: ('a,'b) t -> ('a,'b) t
  val count: ('a,'b) t -> int
  val first: ('a,'b) t -> 'a array
  val second: ('a,'b) t -> 'b array
  val map1:   ('a->'c) -> ('a,'b) t -> ('c,'b) t
  val map2:   ('b->'c) -> ('a,'b) t -> ('a,'c) t
  val elem1: int -> ('a,'b) t -> 'a
  val elem2: int -> ('a,'b) t -> 'b
  val sub: int -> int -> ('a,'b) t -> ('a,'b) t
  val pair: ('a,'b) t -> 'a array * 'b array
end =
  struct
    type ('a, 'b) t =
      { arr1: 'a array; arr2: 'b array}
    let make (arr1: 'a array) (arr2:'b array): ('a,'b) t =
      assert (Array.length arr1 = Array.length arr2);
      {arr1; arr2}
    let from_pair ((a,b):'a array * 'b array): ('a,'b) t =
      make a b
    let copy (a:('a,'b) t): ('a,'b) t =
      {arr1 = Array.copy a.arr1;
       arr2 = Array.copy a.arr2}
    let empty: ('a,'b) t =
      {arr1 = [||]; arr2 =  [||]}
    let count (a:('a,'b) t): int =
      Array.length a.arr1
    let first (a:('a,'b) t): 'a array =
      a.arr1
    let second (a:('a,'b) t): 'b array =
      a.arr2
    let elem1 (i:int) (a:('a,'b) t): 'a =
      assert (i < count a);
      a.arr1.(i)
    let elem2 (i:int) (a:('a,'b) t): 'b =
      assert (i < count a);
      a.arr2.(i)
    let map1 (f:'a -> 'c) (a:('a,'b) t): ('c,'b) t =
      {arr1 = Array.map f a.arr1;
       arr2 = a.arr2}
    let map2 (f:'b -> 'c) (a:('a,'b) t): ('a,'c) t =
      {arr2 = Array.map f a.arr2;
       arr1 = a.arr1}
    let sub (start:int) (n:int) (a:('a,'b) t): ('a,'b) t =
      assert (start <= count a);
      assert (start + n <= count a);
      make
        (Array.sub a.arr1 start n)
        (Array.sub a.arr2 start n)
    let pair (a:('a,'b) t): 'a array * 'b array =
      a.arr1, a.arr2
  end


module Seq: sig
  type 'a t
  val empty: unit -> 'a t
  val singleton: 'a -> 'a t
  val make:  int -> 'a -> 'a t
  val count: 'a t -> int
  val is_empty: 'a t -> bool
  val elem:  int -> 'a t -> 'a
  val first: 'a t -> 'a
  val last:  'a t -> 'a
  val copy:  'a t -> 'a t
  val put:   int -> 'a -> 'a t -> unit
  val push:  'a -> 'a t -> unit
  val pop:   int -> 'a t -> unit
  val pop_last: 'a t -> 'a
  val keep:  int -> 'a t -> unit
  val remove: int -> 'a t -> unit
  val iter:  ('a->unit) -> 'a t -> unit
  val iteri: (int->'a->unit) -> 'a t -> unit
  val fold:  ('b->'a->'b) -> 'b -> 'a t -> 'b
  val find_min: ('a -> bool) -> 'a t -> int
end = struct
  type 'a t = {mutable cnt:int;
               mutable arr: 'a array}

  let empty () = {cnt=0; arr=[||]}

  let singleton (e:'a) = {cnt=1; arr=Array.make 1 e}

  let make (n:int) (e:'a) = {cnt=n; arr=Array.make n e}

  let count (seq:'a t): int  = seq.cnt

  let is_empty (seq:'s t): bool =  (count seq = 0)

  let elem (i:int) (seq:'a t): 'a =
    assert (i<seq.cnt);
    seq.arr.(i)

  let first (seq:'a t): 'a =
    assert (0 < count seq);
    elem 0 seq

  let last (seq:'a t): 'a =
    assert (0 < count seq);
    elem (count seq - 1) seq

  let copy (s:'a t): 'a t =
    {cnt = s.cnt; arr = Array.copy s.arr}

  let put (i:int) (e:'a) (seq:'a t): unit =
    assert (i<seq.cnt);
    seq.arr.(i) <- e

  let push (elem:'a) (seq:'a t): unit =
    let cnt = seq.cnt
    in
    let _ =
      if cnt = Array.length seq.arr then
        let narr =
          Array.make (1+2*cnt) elem
        in
        Array.blit seq.arr 0 narr 0 cnt;
        seq.arr <- narr
      else
        seq.arr.(cnt) <- elem
    in
    assert (cnt < Array.length seq.arr);
    seq.cnt <- cnt+1

  let pop (n:int) (seq: 'a t): unit =
    assert (n <= count seq);
    seq.cnt <- seq.cnt - n

  let keep (n:int) (seq: 'a t): unit =
    assert (n <= count seq);
    seq.cnt <- n


  let pop_last (seq:'a t): 'a =
    assert (0 < count seq);
    let e = last seq in
    pop 1 seq; e


  let iter (f:'a->unit) (s:'a t) =
    let rec iter0 i =
      if i=s.cnt then ()
      else begin
        f (elem i s);
        iter0 (i+1)
      end
    in iter0 0

  let iteri (f:int->'a->unit) (s:'a t) =
    let rec iter0 i =
      if i=s.cnt then ()
      else begin
        f i (elem i s);
        iter0 (i+1)
      end
    in iter0 0

  let fold (f:'b -> 'a -> 'b) (start:'b) (seq:'a t): 'b =
    interval_fold
      (fun res i -> f res (elem i seq))
      start
      0
      (count seq)

  let find_min (f:'a->bool) (s:'a t): int =
    let cnt = count s in
    let rec find (start:int): int =
      if start = cnt then
        raise Not_found
      else if f (elem start s) then
        start
      else
        find (start+1)
    in
    find 0

  let remove (i:int) (s:'a t): unit =
    assert (i < count s);
    s.arr <-
      Array.init
        (Array.length s.arr - 1)
        (fun j ->
          if j < i then s.arr.(j)
          else s.arr.(j+1));
    s.cnt <- s.cnt - 1
end

type 'a seq = 'a Seq.t





module Key_table: sig
  type 'a t
  val empty:  unit -> 'a t
  val count:  'a t -> int
  val index:  'a  -> 'a t -> int
  val key:    int -> 'a t -> 'a
  val find:   'a -> 'a t -> int
  val iter:   ('a -> unit) -> 'a t -> unit
  val iteri:  (int->'a->unit) -> 'a t -> unit
end = struct
  type 'a t = {seq: 'a Seq.t;
               map: ('a,int) Hashtbl.t}

  let empty () = {seq=Seq.empty (); map=Hashtbl.create 53}

  let count (st:'a t)   = Seq.count st.seq

  let added (elem:'a) (st:'a t): int =
    let cnt = Seq.count st.seq
    in
    Seq.push elem st.seq;
    Hashtbl.add st.map elem cnt;
    cnt

  let find (elem:'a)  (st:'a t): int = Hashtbl.find st.map elem

  let index (elem:'a) (st:'a t): int =
    try
      Hashtbl.find st.map elem
    with
      Not_found ->
        added elem st

  let key (s:int) (st:'a t): 'a =
    assert (0 <= s);
    assert (s < Seq.count st.seq);
    Seq.elem s st.seq

  let iter (f:'a->unit) (t:'a t) =
    Seq.iter f t.seq

  let iteri (f:int->'a->unit) (t:'a t) =
    Seq.iteri f t.seq
end

module Permutation: sig
  val is_valid: int array -> bool
  val invert:   int array -> int array
end = struct
  let is_valid (p:int array): bool =
    let n = Array.length p in
    let flags = Array.make n false in
    try
      for i = 0 to n - 1 do
        if not (0 <= p.(i) && p.(i) < n) then
          raise Not_found;
        if flags.(p.(i)) then
          raise Not_found;  (* duplicate element *)
        flags.(p.(i)) <- true
      done;
      true
    with Not_found ->
      false

  let invert (p:int array): int array =
    let n = Array.length p in
    let inv = Array.make n 0 in
    for i = 0 to n - 1 do
      inv.(p.(i)) <- i
    done;
    inv
end


let levenshtein_distance (s:string) (t:string) =
  let minimum a b c =
    min a (min b c)
  in
  let m = String.length s
  and n = String.length t in
  (* for all i and j, d.(i).(j) will hold the Levenshtein distance between
     the first i characters of s and the first j characters of t *)
  let d = Array.make_matrix (m+1) (n+1) 0 in

  for i = 0 to m do
    d.(i).(0) <- i  (* the distance of any first string to an empty second string *)
  done;
  for j = 0 to n do
    d.(0).(j) <- j  (* the distance of any second string to an empty first string *)
  done;

  for j = 1 to n do
    for i = 1 to m do

      if s.[i-1] = t.[j-1] then
        d.(i).(j) <- d.(i-1).(j-1)  (* no operation required *)
      else
        d.(i).(j) <- minimum
                       (d.(i-1).(j) + 1)   (* a deletion *)
                       (d.(i).(j-1) + 1)   (* an insertion *)
                       (d.(i-1).(j-1) + 1) (* a substitution *)
    done;
  done;

  d.(m).(n)



module type Set = sig
  type 'a t
  val empty:      unit -> 'a t
  val mem:        'a -> 'a t -> bool
  val plus_elem:  'a -> 'a t -> 'a t
  val plus_set:   'a t -> 'a t -> 'a t
  val test:       unit -> unit
end


module ArrayedSet: Set = struct
  type 'a t = 'a array

  let empty () = Array.init 0 (fun _ -> assert false)

  let mem (el:'a) (set:'a t) =
    let idx = Search.binsearch_max el set
    in
    0<idx && set.(idx-1)=el

  let plus_elem (el:'a) (set:'a t): 'a t =
    let i = Search.binsearch_max el set;
    and len = Array.length set
    in
    if 0<i  && set.(i-1)=el then
      set
    else
      Array.init (len+1)
        (fun j ->
          if j<i then set.(j)
          else if j=i then el
          else set.(j-1))

  let plus_set (s1:'a t) (s2:'a t): 'a t =
    let rec plus i =
      if i=0 then s1
      else plus_elem s2.(i-1) s1
    in
    plus (Array.length s2)

  let test () =
    let len = 10
    in
    let rec ins n =
      if n=0 then empty ()
      else plus_elem (len-n) (plus_elem (len-n) (ins (n-1)))
    in
    let set = ins len
    in
    let rec check n =
      if n=0 then true
      else ((n-1)=set.(n-1)) && mem (n-1) set && check (n-1)
    in
    assert (check len);
    assert (not (mem len set))
end
