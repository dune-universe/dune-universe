include Stdlib.Array
open Module_types


let is_empty (arr: 'a array): bool =
    length arr = 0


let first (arr: 'a array): 'a =
    assert (not (is_empty arr));
    get arr 0


let last (arr: 'a array): 'a =
    let len = length arr in
    assert (0 < len);
    get arr (len - 1)



let find (p: 'a -> bool) (arr: 'a array): int =
  let len = length arr
  in
  let rec find i =
    if i = len || p (get arr i) then
      len
    else
      find (i + 1)
  in
  find 0


let take (n:int) (arr:'a array): 'a array =
  assert (n <= length arr);
  sub arr 0 n


let remove_last (n: int) (arr: 'a array): 'a array =
  let len = length arr in
  assert (n <= len);
  take (len - n) arr


let put (i:int) (a:'a) (arr:'a array): 'a array =
  assert (i < length arr);
  let res = copy arr in
  set res i a;
  res

let push (a:'a) (arr:'a array): 'a array =
  let len = length arr in
  init
    (len + 1)
    (fun i ->
      if i < len then
        get arr i
      else
        a)

let fill (n: int) (e: 'a) (arr: 'a array): 'a array =
    let len = length arr in
    let arr_new = make (len + n) e in
    blit arr 0 arr_new 0 len;
    arr_new


let foldi_left (f: 'a -> int -> 'b -> 'a) (a: 'a) (arr: 'b array): 'a =
    let len = length arr
    in
    let rec fold a i =
        if i = len then
            a
        else
            fold (f a i (get arr i)) (i + 1)
    in
    fold a 0


let foldi_right (f: int -> 'a -> 'b -> 'b) (arr: 'a array) (b: 'b): 'b =
    let len = length arr
    in
    let rec fold i b =
        if i = 0 then
            b
        else
            let i = i - 1 in
            fold i (f i (get arr i) b)
    in
    fold len b




module Monadic (M: MONAD) =
struct
    let mapi
        (f: int -> 'a -> 'b M.t)
        (arr: 'a array)
        : 'b array M.t
    =
        let len = length arr in
        if 0 < len then
            let open M in
            f 0 (get arr 0)
            >>= fun b ->
            let arr2 = make len b in
            let rec make_from i =
                if i = len then
                    return arr2
                else
                    (
                        f i (get arr i) >>= fun b ->
                        set arr2 i b;
                        make_from (i + 1)
                    )
            in
            make_from 1
        else
            M.return [||]



    let map
        (f: 'a -> 'b M.t)
        (arr: 'a array)
        : 'b array M.t
    =
        mapi (fun _ -> f) arr



    let foldi_left
        (f: int -> 'a -> 'b -> 'b M.t)
        (arr: 'a array)
        (b: 'b)
        : 'b M.t
    =
        let len = length arr
        in
        let rec fold_from i b =
            if i = len then
                M.return b
            else
                M.(f i (get arr i) b >>= fold_from (i + 1))
        in
        fold_from 0 b


    let fold_left
        (f: 'a -> 'b -> 'b M.t)
        (arr: 'a array)
        (b: 'b)
        : 'b M.t
    =
        foldi_left
            (fun _ -> f)
            arr
            b

end
