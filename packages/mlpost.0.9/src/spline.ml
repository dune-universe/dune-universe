open Point_lib
open Point_lib.Infix
module P = Point_lib

type point = Ctypes.point

type abscissa = float

type 'a t' = { sa : 'a; sb : 'a; sc : 'a; sd : 'a }

type t = point t'

let inter_depth = ref 15

let debug = false

let pt_f fmt p = Format.fprintf fmt "{@[ %.20g,@ %.20g @]}" p.x p.y

let print fmt pt =
  Format.fprintf fmt "@[{ %a,@ %a,@ %a,@ %a }@]@." pt_f pt.sa pt_f pt.sb pt_f
    pt.sc pt_f pt.sd

let create a b c d = { sa = a; sb = b; sc = c; sd = d }

let create_with_offset _offs a b c d = create a b c d

let explode s = (s.sa, s.sb, s.sc, s.sd)

let reverse _conv { sa; sb; sc; sd } = { sa = sd; sb = sc; sc = sb; sd = sa }

let right_control_point t = t.sc

let right_point t = t.sd

let left_point t = t.sa

let left_control_point t = t.sb

let cubic a b c d t =
  t
  *. ( t
       *. ((t *. (d +. (3. *. (b -. c)) -. a)) +. (3. *. (c -. (2. *. b) +. a)))
     +. (3. *. (b -. a)) )
  +. a

(*  ((t^3)*(d - (3*c) + (3*b) - a)) + (3*(t^2)*(c - (2*b) + a)) +
 *  (3*t*(b - a)) + a*)
(*  d *. (t**3.) +. 3. *. c *. (t**2.) *. (1. -. t) +. 3. *. b *. (t**1.)
 *  *.(1. -. t)**2. +. a *. (1. -. t)**3.*)

let point_of s t =
  {
    x = cubic s.sa.x s.sb.x s.sc.x s.sd.x t;
    y = cubic s.sa.y s.sb.y s.sc.y s.sd.y t;
  }

let point_of_s s t =
  assert (0. <= t && t <= 1.);
  point_of s t

let direction s t =
  (* An expression as polynomial:
     short but lots of point operations
     (d-3*c+3*b-a)*t^2+(2*c-4*b+2*a)*t+b-a *)
  (*
  t */ (t */ (s.sd -/ 3. */ (s.sc +/ s.sb) -/ s.sa) +/
  2. */ (s.sc +/ s.sa -/ 2. */ s.sb)) +/ s.sb -/ s.sa
*)
  (* This expression is longer, but has less operations on points: *)
  ((t ** 2.) */ s.sd)
  +/ (((2. *. t) -. (3. *. (t ** 2.))) */ s.sc)
  +/ ((1. -. (4. *. t) +. (3. *. (t ** 2.))) */ s.sb)
  +/ (-.((1. -. t) ** 2.) */ s.sa)

(*
                         sqrt((a - b) d + c  + (- b - a) c + b ) + c - 2 b + a
                   t = - -----------------------------------------------------]
                                           d - 3 c + 3 b - a
*)

(*
let extremum a b c d =
  (* denominator *)
  let eqa = (d -. a) +. (3.*.(b -. c)) in
  (* *)
  let eqb = 2.*.(c +. a -. (2.*.b)) in
  let eqc = (b -. a) in
  (*Format.printf "eqa : %f; eqb : %f; eqc : %f@." eqa eqb eqc;*)
  let test s l = if 0.<=s && s<=1. then s::l else l in
  if eqa = 0. then if eqb = 0. then []
  else test (-. eqc /. eqb) []
  else
  (*let sol delta = (delta -. (2.*.b) +. a +. c)/.
    (a -. d +. (3.*.(c -. b))) in*)
  (*let delta = ((b*.b) -. (c*.(b +. a -. c)) +. (d*.(a -. b))) in*)
  let sol delta = (delta -. c +. 2. *. b -. a) /. eqa in
  let delta = (a -. b) *. d +. c *. c -. ( b +. a) *. c +. b *. b in
  let delta2 = (eqb*.eqb) -. (4.*.eqa*.eqc) in
  Format.printf "delta : %f delta2 : %f@." delta delta2;
  match compare delta 0. with
    | x when x<0 -> []
    | 0 -> test (sol 0.) []
    | _ ->
        let delta = delta**0.5 in
        test (sol delta) (test (sol (-.delta)) [])

(* let extremum a b c d =
 *   (\* denominator *\)
 *   let eqa = (d -. a) +. (3.*.(b -. c)) in
 *   (\* *\)
 *   let eqb = 2.*.(c +. a -. (2.*.b)) in
 *   let eqc = (b -. a) in
 *   (\*Format.printf "eqa : %f; eqb : %f; eqc : %f@." eqa eqb eqc;*\)
 *   let test s l = if 0.<=s && s<=1. then s::l else l in
 *   if eqa = 0. then if eqb = 0. then []
 *   else test (-. eqc /. eqb) []
 *   else
 *   (\*let sol delta = (delta -. (2.*.b) +. a +. c)/.
 *     (a -. d +. (3.*.(c -. b))) in*\)
 *   (\*let delta = ((b*.b) -. (c*.(b +. a -. c)) +. (d*.(a -. b))) in*\)
 *   let sol delta = (delta +. eqb) /. (-.2.*.eqa) in
 *   let delta = (eqb*.eqb) -. (4.*.eqa*.eqc) in
 *   (\*Format.printf "delta2 : %f; delta : %f@." delta2 delta;*\)
 *   match compare delta 0. with
 *     | x when x<0 -> []
 *     | 0 -> test (sol 0.) []
 *     | _ ->
 *         let delta = delta**0.5 in
 *         test (sol delta) (test (sol (-.delta)) []) *)


let remarkable a b c d =
  let res = 0.::1.::(extremum a b c d) in
  Format.printf "remarquable : %a@."
    (fun fmt -> List.iter (Format.fprintf fmt "%f;")) res;
    res

let precise_bounding_box s =
  (*Format.printf "precise : %a@." print_spline s;*)
  let x_remarq = List.map (apply_x cubic s) (apply_x remarkable s) in
  let y_remarq = List.map (apply_y cubic s) (apply_y remarkable s) in
  let x_max = List.fold_left Stdlib.max neg_infinity x_remarq in
  let y_max = List.fold_left Stdlib.max neg_infinity y_remarq in
  let x_min = List.fold_left Stdlib.min infinity x_remarq in
  let y_min = List.fold_left Stdlib.min infinity y_remarq in
  List.iter
    (fun f ->
       let x = apply_x cubic s f in
       let y = apply_y cubic s f in
       if not (x_min <= x && x <= x_max && y_min <= y && y <= y_max)
       then begin
         Format.eprintf "Error at %f(%f,%f): %a@." f x y print s;
         List.iter (Format.eprintf "   x:%f@.") x_remarq;
         List.iter (Format.eprintf "   y:%f@.") y_remarq;
       end
    )
    (0.1::0.2::0.3::0.4::0.5::0.6::0.7::0.8::0.9::[]);
  x_min,y_min,x_max,y_max
 *)

let apply_x f s = f s.sa.x s.sb.x s.sc.x s.sd.x

let apply_y f s = f s.sa.y s.sb.y s.sc.y s.sd.y

let apply4 f s = f s.sa s.sb s.sc s.sd

let f4 f a b c d = f (f a b) (f c d)

let bounding_box s =
  let x_max = apply_x (f4 Stdlib.max) s in
  let y_max = apply_y (f4 Stdlib.max) s in
  let x_min = apply_x (f4 Stdlib.min) s in
  let y_min = apply_y (f4 Stdlib.min) s in
  (x_min, y_min, x_max, y_max)

let middle a b =
  { x = (a.x *. 0.5) +. (b.x *. 0.5); y = (a.y *. 0.5) +. (b.y *. 0.5) }

let bisect middle a =
  let b = a in
  (*D\leftarrow (C+D)/2*)
  let b = { b with sd = middle b.sd b.sc } in
  (*C\leftarrow (B+C)/2, D\leftarrow (C+D)/2*)
  let b = { b with sc = middle b.sc b.sb } in
  let b = { b with sd = middle b.sd b.sc } in
  (*B\leftarrow (A+B)/2, C\leftarrow (B+C)/2, D\leftarrow(C+D)/2*)
  let b = { b with sb = middle b.sb b.sa } in
  let b = { b with sc = middle b.sc b.sb } in
  let b = { b with sd = middle b.sd b.sc } in
  let c = a in
  let c = { c with sa = middle c.sa c.sb } in
  let c = { c with sb = middle c.sb c.sc } in
  let c = { c with sa = middle c.sa c.sb } in
  let c = { c with sc = middle c.sc c.sd } in
  let c = { c with sb = middle c.sb c.sc } in
  let c = { c with sa = middle c.sa c.sb } in
  (b, c)

let bisect_p a = bisect (fun a b -> (a *. 0.5) +. (b *. 0.5)) a

let bisect a = bisect middle a

let rec precise_bounding_box_aux ~min ~max ~cmp cmin s =
  let cmin' = min s.sb s.sc in
  if cmp cmin cmin' <= 0 then cmin
  else
    let cmin' = min (min s.sa s.sd) cmin' in
    let cmax' = apply4 (f4 max) s in
    if cmp cmin' cmax' = 0 then cmin
    else
      let sa, sb = bisect_p s in
      let cmin = min sa.sd cmin in
      precise_bounding_box_aux ~min ~max ~cmp
        (precise_bounding_box_aux ~min ~max ~cmp cmin sa)
        sb

let precise_bounding_box s =
  let f ~min ~max ~cmp ~map s =
    let s = { sa = map s.sa; sb = map s.sb; sc = map s.sc; sd = map s.sd } in
    let cmin = min s.sa s.sd in
    precise_bounding_box_aux ~min ~max ~cmp cmin s
  in
  ( f ~min ~max ~cmp:(fun a b -> compare a b) s ~map:(fun p -> p.x),
    f ~min ~max ~cmp:(fun a b -> compare a b) s ~map:(fun p -> p.y),
    f ~min:max ~max:min ~cmp:(fun a b -> -compare a b) s ~map:(fun p -> p.x),
    f ~min:max ~max:min ~cmp:(fun a b -> -compare a b) s ~map:(fun p -> p.y) )

let test_in amin amax bmin bmax = amin <= bmax && bmin <= amax

let is_intersect a b =
  let ax_min, ay_min, ax_max, ay_max = bounding_box a in
  let bx_min, by_min, bx_max, by_max = bounding_box b in
  test_in ax_min ax_max bx_min bx_max && test_in ay_min ay_max by_min by_max

let _is_intersect_precise a b =
  let ax_min, ay_min, ax_max, ay_max = precise_bounding_box a in
  let bx_min, by_min, bx_max, by_max = precise_bounding_box b in
  test_in ax_min ax_max bx_min bx_max && test_in ay_min ay_max by_min by_max

let intersect_fold f acc a b =
  let rec aux acc a b t1 t2 dt = function
    | 0 ->
        if is_intersect a b then f (t1 + (dt / 2), t2 + (dt / 2)) acc else acc
    | n ->
        if is_intersect a b then
          let n = n - 1 and dt = dt / 2 in
          let a1, a2 = bisect a and b1, b2 = bisect b in
          let acc = aux acc a1 b1 t1 t2 dt n in
          let acc = aux acc a1 b2 t1 (t2 + dt) dt n in
          let acc = aux acc a2 b1 (t1 + dt) t2 dt n in
          let acc = aux acc a2 b2 (t1 + dt) (t2 + dt) dt n in
          acc
        else acc
  in
  let nmax = int_of_float (2. ** float_of_int (!inter_depth + 1)) in
  aux acc a b 0 0 nmax !inter_depth

exception Found of int * int

let one_intersection a b =
  let nmax = 2. ** float_of_int (!inter_depth + 1) in
  let f_from_i x = float_of_int x *. (1. /. nmax) in
  try
    intersect_fold (fun (x, y) () -> raise (Found (x, y))) () a b;
    raise Not_found
  with Found (t1, t2) -> (f_from_i t1, f_from_i t2)

module UF = Unionfind

let intersection a b =
  if a = b then []
  else
    let rem_noise delta mdelta = function
      | [] -> []
      | noisy ->
          let uf = UF.init noisy in
          let link sel msel =
            let sorted =
              List.fast_sort (fun x y -> compare (sel x) (sel y)) noisy
            in
            let rec pass bef = function
              | [] -> ()
              | e :: l ->
                  if sel bef - sel e <= delta then (
                    if abs (msel e - msel bef) <= mdelta then UF.union e bef uf;
                    pass bef l )
                  else ()
            in
            ignore
              (List.fold_left
                 (fun acc bef ->
                   pass bef acc;
                   bef :: acc)
                 [] sorted)
          in
          link fst snd;
          link snd fst;
          UF.fold_classes (fun x acc -> x :: acc) [] uf
    in
    let nmax = 2. ** float_of_int (!inter_depth + 1) in
    let l = intersect_fold (fun x acc -> x :: acc) [] a b in
    if debug then
      Format.printf "@[%a@]@."
        (fun fmt ->
          List.iter (fun (f1, f2) -> Format.fprintf fmt "%i,%i" f1 f2))
        l;
    let l = rem_noise (2 * !inter_depth) (16 * !inter_depth) l in
    let f_from_i x = x *. (1. /. nmax) in
    let res = List.rev_map (fun (x, y) -> (f_from_i x, f_from_i y)) l in
    if debug then
      Format.printf "@[%a@]@."
        (fun fmt -> List.iter (pt_f fmt))
        (List.map (fun (t1, t2) -> point_of a t1 -/ point_of b t2) res);
    res

type split = Min | Max | InBetween of t * t

let split s t =
  assert (0. <= t && t <= 1.);
  if t = 1. then Max
  else if t = 0. then Min
  else
    let t0 = (*_01_of_s s*) t in
    let _1t0 = 1. -. t0 in
    let b1 = (t0 */ s.sb) +/ (_1t0 */ s.sa) in
    let c1 =
      (t0 *. t0 */ s.sc) +/ (2. *. t0 *. _1t0 */ s.sb) +/ (_1t0 *. _1t0 */ s.sa)
    in
    let d1 = point_of s t0 in
    let a2 = d1 in
    let c2 = (_1t0 */ s.sc) +/ (t0 */ s.sd) in
    let b2 =
      (_1t0 *. _1t0 */ s.sb) +/ (2. *. _1t0 *. t0 */ s.sc) +/ (t0 *. t0 */ s.sd)
    in
    InBetween
      ( { s with sb = b1; sd = d1; sc = c1 },
        { s with sa = a2; sb = b2; sc = c2 } )

let norm2 a b = (a *. a) +. (b *. b)

let is_possible (axmin, aymin, axmax, aymax) (bxmin, bymin, bxmax, bymax) =
  match (axmin > bxmax, aymin > bymax, axmax < bxmin, aymax < bymin) with
  | true, true, _, _ -> norm2 (axmin -. bxmax) (aymin -. bymax)
  | _, _, true, true -> norm2 (axmax -. bxmin) (aymax -. bymin)
  | true, _, _, true -> norm2 (axmin -. bxmax) (aymax -. bymin)
  | _, true, true, _ -> norm2 (axmax -. bxmin) (aymin -. bymax)
  | false, true, false, _ -> norm2 0. (aymin -. bymax)
  | false, _, false, true -> norm2 0. (aymax -. bymin)
  | true, false, _, false -> norm2 (axmin -. bxmax) 0.
  | _, false, true, false -> norm2 (axmax -. bxmin) 0.
  | false, false, false, false -> 0.

let dist_min_point ({ x = px; y = py } as p) s =
  (* TODO simplify *)
  let is_possible_at a = is_possible (bounding_box a) (px, py, px, py) in
  let nmax = 2. ** float_of_int (!inter_depth + 1) in
  let rec aux a ((min, _) as pmin) t1 dt = function
    | 0 ->
        let t1 = float_of_int (t1 + (dt / 2)) /. nmax in
        let pt1 = point_of s t1 in
        let dist = P.dist2 pt1 p in
        if dist < min then (dist, t1) else pmin
    | n ->
        let dt = dt / 2 in
        let af, al = bisect a in
        let dist_af = is_possible_at af in
        let dist_al = is_possible_at al in
        let doit ((min, _) as pmin) dist am t =
          if dist < min then aux am pmin t dt (n - 1) else pmin
        in
        if dist_af < dist_al then
          let pmin = doit pmin dist_af af t1 in
          doit pmin dist_al al (t1 + dt)
        else
          let pmin = doit pmin dist_al al (t1 + dt) in
          doit pmin dist_af af t1
  in
  let pmin = (P.dist2 (left_point s) p, 0.) in
  aux s pmin 0 (int_of_float nmax) !inter_depth

let dist_min_spline s1 s2 =
  let is_possible_at a b = is_possible (bounding_box a) (bounding_box b) in
  let nmax = 2. ** float_of_int (!inter_depth + 1) in
  let rec aux a b ((min, _) as pmin) t1 t2 dt = function
    | 0 ->
        let t1 = float_of_int (t1 + (dt / 2)) /. nmax in
        let t2 = float_of_int (t2 + (dt / 2)) /. nmax in
        let ap = point_of s1 t1 in
        let bp = point_of s2 t2 in
        let dist = norm2 (ap.x -. bp.x) (ap.y -. bp.y) in
        if dist < min then (dist, (t1, t2)) else pmin
    | n ->
        let n = n - 1 in
        let dt = dt / 2 in
        let af, al = bisect a in
        let bf, bl = bisect b in
        let doit dist am bm t1 t2 ((min, _) as pmin) =
          if dist < min then aux am bm pmin t1 t2 dt n else pmin
        in
        let l =
          [
            (af, bf, t1, t2);
            (af, bl, t1, t2 + dt);
            (al, bf, t1 + dt, t2);
            (al, bl, t1 + dt, t2 + dt);
          ]
        in
        let l =
          List.map
            (fun (am, bm, t1, t2) ->
              let dist = is_possible_at am bm in
              (dist, doit dist am bm t1 t2))
            l
        in
        let l = List.fast_sort (fun (da, _) (db, _) -> compare da db) l in
        List.fold_left (fun pmin (_, doit) -> doit pmin) pmin l
  in
  let pmin = (P.dist2 (left_point s1) (left_point s2), (0., 0.)) in
  aux s1 s2 pmin 0 0 (int_of_float nmax) !inter_depth

let translate t a =
  { sa = a.sa +/ t; sb = a.sb +/ t; sc = a.sc +/ t; sd = a.sd +/ t }

let transform t a =
  {
    sa = P.transform t a.sa;
    sb = P.transform t a.sb;
    sc = P.transform t a.sc;
    sd = P.transform t a.sd;
  }
