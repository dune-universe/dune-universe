open Array

let foldi_left f st a =
  let len = length a in
  let rec loop st i = 
    if i = len then st
    else
      loop (f st i (unsafe_get a i)) (i+1)
  in
  loop st 0

let foldi_right f a st =
  let len = length a in
  let rec loop st i = 
    if i = -1 then st
    else
      loop (f i (unsafe_get a i) st) (i+1)
  in
  loop st (len-1)

let shuffle ?(random=Random.int) a =
  let len = length a - 1 in
  for i = 0 to len - 1 do
    let d = len - i in
    let j = random d + i in
    let x = unsafe_get a i in
    let y = unsafe_get a j in
    unsafe_set a i y;
    unsafe_set a j x;
  done
