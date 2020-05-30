type t = int * int

let empty = (0, -1)
            
let is_empty (a, b) = a > b

let make a b = if a > b then empty else (a, b)

let iter f (a, b) =
  let rec loop i =
    if i > b then ()
    else begin
      f i;
      loop (i+1)
    end
  in
  loop a

let map f (a, b) =
  let rec loop acc i =
    if i > b then List.rev acc
    else loop (f i :: acc) (i+1)
  in
  loop [] a

let fold_left f init (from, to_) =
  let rec loop acc i =
    if i > to_ then acc
    else loop (f acc i) (i+1)
  in
  loop init from

let range r =
  if is_empty r then None
  else Some r
