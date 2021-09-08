
module L = BatList

(* population standard deviation *)
let stddev (l: float list) =
  let n, sx, sx2 =
    List.fold_left (fun (n, sx, sx2) x ->
        (n +. 1., sx +. x, sx2 +. (x *.x))
      ) (0., 0., 0.) l
  in
  sqrt ((sx2 -. (sx *. sx) /. n) /. n)
(* stddev [2.; 4.; 4.; 4.; 5.; 5.; 7.; 9.] = 2.0 *)

(* abort if condition is not met *)
let enforce (condition: bool) (err_msg_fun: unit -> string): unit =
  if not condition then
    failwith (err_msg_fun ())

let prepend2 a (b, c) =
  (a, b, c)

let prepend3 a (b, c, d) =
  (a, b, c, d)

let fst4 (a, _b, _c, _d) =
  a

let trd4 (_a, _b, c, _d) =
  c

let frt4 (_a, _b, _c, d) =
  d

let combine3 l1 l2 l3 =
  let rec loop acc = function
    | ([], [], []) -> L.rev acc
    | (x :: xs, y :: ys, z :: zs) -> loop ((x, y, z) :: acc) (xs, ys, zs)
    | _ -> failwith "Utls.combine3: incompatible list lengths" in
  loop [] (l1, l2, l3)
