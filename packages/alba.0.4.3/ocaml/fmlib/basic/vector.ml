type 'a t = {mutable cnt:int;
             mutable arr: 'a array}

let make_empty () = {cnt=0; arr=[||]}

let singleton (e:'a) = {cnt=1; arr=Array.make 1 e}

let make (n:int) (e:'a) = {cnt=n; arr=Array.make n e}

let count (v:'a t): int  = v.cnt

let is_empty (v:'s t): bool =  (count v = 0)

let elem (v:'a t) (i:int): 'a =
  assert (i<v.cnt);
  v.arr.(i)

let first (v:'a t): 'a =
  assert (0 < count v);
  elem v 0

let last (v:'a t): 'a =
  assert (0 < count v);
  elem v (count v - 1)

let copy (s:'a t): 'a t =
  {cnt = s.cnt; arr = Array.copy s.arr}

let put (v:'a t) (i:int) (e:'a): unit =
  assert (i<v.cnt);
  v.arr.(i) <- e

let push_rear (v:'a t) (elem:'a): unit =
  let cnt = v.cnt
  in
  let _ =
    if cnt = Array.length v.arr then
      let narr =
        Array.make (1+2*cnt) elem
      in
      Array.blit v.arr 0 narr 0 cnt;
      v.arr <- narr
    else
      v.arr.(cnt) <- elem
  in
  assert (cnt < Array.length v.arr);
  v.cnt <- cnt+1


let remove_n_last (v: 'a t) (n:int): unit =
  assert (n <= count v);
  v.cnt <- v.cnt - n


let remove_last (v:'a t): 'a =
  assert (0 < count v);
  let e = last v in
  remove_n_last v 1; e


let keep (v: 'a t) (n:int): unit =
  assert (n <= count v);
  v.cnt <- n

let remove (s:'a t) (i:int): unit =
  assert (i < count s);
  s.arr <-
    Array.init
      (Array.length s.arr - 1)
      (fun j ->
        if j < i then s.arr.(j)
        else s.arr.(j+1));
  s.cnt <- s.cnt - 1
