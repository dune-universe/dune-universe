(* geometry utilities with floatting point precision *)
type point = float * float
type range = float * float
type hull = point list

(* 3d stuff *)
type point3d = float * float * float
type vertex3d = point3d list
type triangle3D = point3d * point3d * point3d

let to_int_point (x,y) = (int_of_float x),(int_of_float y)

let sq_dist (a,b) (c,d) =
  let dx = c-.a and dy = d-.b in
  dx*.dx +. dy*.dy

let print fmt (x,y) = Format.fprintf fmt "(%f,%f)" x y

(* convex hull computation *)
let hull : point list -> hull =
  let det (dx1,dy1) (dx2,dy2) = dx1 *. dy2 -. dy1 *. dx2 in
  function
  | [] -> []
  | ([_] as l) | ([_;_] as l) -> l
  | h::t as l ->
     let p = List.fold_left min h t in
     let ccw (px,py) (ax,ay) (bx,by) = det (ax-.px,ay-.py) (bx-.px,by-.py) in
     let cmp p1 p2 =
       if p1 = p then 1
       else if p2 = p then -1
       else
         let ccw = ccw p p1 p2 in
         if ccw < 0. then 1
         else if ccw = 0. then 0
         else -1
     in
     let rec graham_aux cl conv =
       match cl,conv with
       | ([],_) -> conv
       | (h::t, a::b::tl) ->
          let p = ccw b a h in
          if p <= 0. then graham_aux cl (b::tl)
          else graham_aux t (h::conv)
       | (h::t,_) -> graham_aux t (h::conv)
     in graham_aux (List.sort cmp l) [p]

type line =  float * float * float

(* Line that goes through two points *)
let line_of_points ((x1,y1) as p1) ((x2,y2) as p2) : line =
  if p1 <> p2 then
    if x1 = x2 then (1.,0.,x1)
    else
      let coeff = (y2 -. y1) /. (x2 -. x1) in
      let ord = y1 -. coeff *. (x1)
      in (coeff,-1.,ord)
  else failwith "cant build line with one point"

let orth_of_line (a,b,_) (x,y) =
  if b = 0. then (0.,1.,y)
  else
    let coeff = (-1./.a) in
    (coeff),-1.,(y -. coeff *. x)
