include Stdlib.Array


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
