type ns = N | S;;
type ew = E | W;;
type lat = float * ns;;
type lng = float * ew;;

type t = lat * lng;;

let dm_to_d v = 
  let dgr = floor @@ v /. 100. in
  let min = v -. (dgr *. 100.) *. (1.0 /. 60.) in 
  dgr +. min
;;

let parse_lat v ns = (dm_to_d v, ns);;
let parse_lng v ew = (dm_to_d v, ew);;

let lat = fst;;
let lng = snd;;

let to_string c = 
  let nstos l = match l with 
  | N -> 'N'
  | S -> 'S'
  in let ewtos l = match l with 
  | E -> 'E'
  | W -> 'W'
  in
  Printf.sprintf "%f %c %f %c" (lat c |> fst) (lat c |> snd |> nstos) 
    (lng c |> fst) (lng c |> snd |> ewtos)
;;