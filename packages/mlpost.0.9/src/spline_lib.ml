(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Johannes Kanig, Stephane Lescuyer                       *)
(*  Jean-Christophe Filliatre, Romain Bardou and Francois Bobot           *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

open Format

exception Not_implemented of string

let not_implemented s = raise (Not_implemented s)

module Error = struct
  let max_absc t f =
    invalid_arg
      ( f ^ ": the abscissa given is greater than max_abscissa : "
      ^ string_of_float t )

  let min_absc ?value f =
    let value =
      match value with None -> "" | Some f -> ": " ^ string_of_float f
    in
    invalid_arg (f ^ ": the abscissa given is smaller than min_abscissa" ^ value)

  let absc_point f = invalid_arg (f ^ ": a point has only the abscissa 0.")

  let dir_point f = invalid_arg (f ^ ": a point has no direction.")
end

module P = Point_lib

type point = P.t

open Point_lib
open Point_lib.Infix

let one_to_one2 f acc a b =
  List.fold_left
    (fun acc ea -> List.fold_left (fun acc eb -> f acc ea eb) acc b)
    acc a

let debug = Spline.debug

type spline = Spline.t

type abscissa = Spline.abscissa

type path_ = { pl : spline list; cycle : bool }

type path = Point of point | Path of path_

let is_closed = function Point _ -> false | Path p -> p.cycle

let is_a_point = function Point p -> Some p | Path _ -> None

let rec print_list sep prf fmt = function
  | [] -> ()
  | [ x ] -> prf fmt x
  | x :: xs ->
      prf fmt x;
      sep fmt ();
      print_list sep prf fmt xs

let semicolon fmt () = Format.fprintf fmt ";@ "

let print_splines = print_list semicolon Spline.print

let print fmt = function
  | Point p -> fprintf fmt "@[Point %a@]" P.print p
  | Path p -> fprintf fmt "@[cycle : %b; %a@]" p.cycle print_splines p.pl

let create_point p = Point p

let create a b c d = Path { pl = [ Spline.create a b c d ]; cycle = false }

let create_line a d = create a a d d

let create_lines = function
  | [] -> assert false
  | [ a ] -> Point a
  | l ->
      let rec aux = function
        | [] | [ _ ] -> []
        | a :: (d :: _ as l) -> Spline.create a a d d :: aux l
      in
      Path { pl = aux l; cycle = false }

let min_abscissa = function Path _ -> 0. | Point _ -> 0.

let length = function Point _ -> 0 | Path p -> List.length p.pl

let max_abscissa p = float (length p)

let with_last f p acc =
  let rec aux = function
    | [] -> assert false
    | [ e ] ->
        let sd = Spline.right_point e and sc = Spline.right_control_point e in
        e :: f sc sd :: acc
    | a :: l -> a :: aux l
  in
  { p with pl = aux p.pl }

let add_end p c d =
  match p with
  | Point p -> create p c c d
  | Path p ->
      Path (with_last (fun mb a -> Spline.create a ((2. */ a) -/ mb) c d) p [])

let add_end_line p d =
  match p with
  | Point p -> create_line p d
  | Path p -> Path (with_last (fun _ a -> Spline.create a a d d) p [])

let add_end_spline p sb sc d =
  match p with
  | Point p -> create p sb sc d
  | Path p -> Path (with_last (fun _ a -> Spline.create a sb sc d) p [])

let abscissa_to f pl t =
  let tn, tf = (truncate t, t -. floor t) in
  let rec aux tn l =
    match (tn, l) with
    | _, [] -> Error.max_absc t "abscissa_to"
    | 1, [ a ] when tf = 0. -> f a 1.
    | 0, a :: _ -> f a tf
    | _, _ :: l -> aux (pred tn) l
  in
  if 0. > t then Error.min_absc "abscissa_to" else aux tn pl

let abscissa_to_point p0 t =
  match p0 with
  | Path p -> abscissa_to Spline.point_of_s p.pl t
  | Point p when t = 0. -> p
  | Point _ -> Error.absc_point "abscissa_to_point"

let direction_of_abscissa p0 t =
  match p0 with
  | Point _ -> Error.dir_point "direction_of_abscissa"
  | Path p -> abscissa_to Spline.direction p.pl t

let unprecise_bounding_box = function
  | Path s ->
      let x_min, y_min, x_max, y_max =
        P.list_min_max_float Spline.bounding_box s.pl
      in
      ({ x = x_min; y = y_min }, { x = x_max; y = y_max })
  | Point s -> (s, s)

let bounding_box = function
  | Path s ->
      let x_min, y_min, x_max, y_max =
        P.list_min_max_float Spline.precise_bounding_box s.pl
      in
      ({ x = x_min; y = y_min }, { x = x_max; y = y_max })
  | Point s -> (s, s)

exception Found of (float * float)

let one_intersection a b =
  match (a, b) with
  | Path a, Path b -> (
      try
        one_to_one2
          (fun () a b ->
            try raise (Found (Spline.one_intersection a b))
            with Not_found -> ())
          () a.pl b.pl;
        if debug then Format.printf "one_intersection : Not_found@.";
        raise Not_found
      with Found a -> a )
  | _ ->
      if debug then Format.printf "one_intersection : Not_found not two paths@.";
      raise Not_found

let intersection a b =
  match (a, b) with
  | Path a, Path b ->
      one_to_one2 (fun acc a b -> acc @ Spline.intersection a b) [] a.pl b.pl
  | _ -> []

let fold_left f acc = function
  | Path p -> List.fold_left (fun acc s -> Spline.apply4 (f acc) s) acc p.pl
  | Point _ -> acc

let iter f = function
  | Path p -> List.iter (Spline.apply4 f) p.pl
  | Point _ -> ()

let ext_list = function [] -> assert false | a :: _ as l -> (a, l)

let append ap0 sb sc bp0 =
  match bp0 with
  | Path bp -> (
      (* let conv x = append_conv ap0 bp0 x +. 1. in *)
      (* let l = List.map (fun b -> Spline.set_min_max conv conv b) bp.pl in *)
      let fbpconv, bpconv = ext_list bp.pl in
      match ap0 with
      | Path ap ->
          let spl =
            with_last
              (fun _ sa -> Spline.create sa sb sc (Spline.left_point fbpconv))
              ap bpconv
          in
          Path { spl with cycle = false }
      | Point p1 ->
          Path
            {
              bp with
              pl = Spline.create p1 sb sc (Spline.left_point fbpconv) :: bp.pl;
            } )
  | Point p2 -> (
      match ap0 with
      | Point p1 -> create p1 sb sc p2
      | Path _ -> add_end_spline ap0 sb sc p2 )

let reverse x =
  match x with
  | Path p as p0 ->
      let conv =
        let max = max_abscissa p0 in
        let min = min_abscissa p0 in
        let sum = max +. min in
        fun x -> sum -. x
      in
      let rec aux acc = function
        | [] -> acc
        | a :: l -> aux (Spline.reverse conv a :: acc) l
      in
      Path { p with pl = aux [] p.pl }
  | Point _ as p -> p

(*left ((t^3)*(d + (3*(b - c)) - a)) +
 *     ((t^2)*(d - (3*b) + (2*a))) + (t*((2*c) - b - a)) + b *)
(*right 3*d - c *)
let cast_path_to_point p = function Path { pl = []; _ } -> Point p | x -> x

(*
(((t0*tt)^3)*(d + (3*(b - c)) - a)) + (3*((((t0*tt)^2)*
  (c + a - (2*b))) + (t0*tt*(b - a)))) + a
*)

let split_aux s t l =
  match Spline.split s t with
  | Spline.Min -> ([], Path { pl = s :: l; cycle = false })
  | Spline.Max ->
      let p =
        cast_path_to_point (Spline.right_point s)
          (Path { pl = l; cycle = false })
      in
      ([ s ], p)
  | Spline.InBetween (s1, s2) -> ([ s1 ], Path { pl = s2 :: l; cycle = false })

let split p0 t =
  match p0 with
  | Path p ->
      let tn, tf = (truncate t, t -. floor t) in
      let rec aux tn l =
        match (tn, l) with
        | _, [] -> Error.max_absc t "split"
        | 1, [ a ] when tf = 0. -> split_aux a 1. l
        | 0, a :: l -> split_aux a tf l
        | _, a :: l ->
            let p1, p2 = aux (pred tn) l in
            (a :: p1, p2)
      in
      if 0. > t then Error.min_absc "split"
      else
        let p1, p2 = aux tn p.pl in
        ( cast_path_to_point
            (Spline.left_point (List.hd p.pl))
            (Path { pl = p1; cycle = false }),
          p2 )
  | Point _ when t = 0. -> (p0, p0)
  | Point _ -> Error.absc_point "split"

let subpath p t1 t2 =
  assert (t1 <= t2);
  let t2 =
    if ceil t1 = ceil t2 then (t2 -. t1) /. (ceil t1 -. t1) else t2 -. floor t1
  in
  (* TODO implement it in a more efficient way *)
  fst (split (snd (split p t1)) t2)

let cut_before a b =
  (* TODO implement it in a more efficient way *)
  try
    let t = fst (one_intersection b a) in
    let res = snd (split b t) in
    (* Format.printf "t : %f@.point %a@.b : %a@.res : %a@."
       t P.print (abscissa_to_point b t) print b print res;*)
    res
  with Not_found -> b

let cut_after a b =
  (* TODO implement it in a more efficient way *)
  try
    let b = reverse b in
    reverse (snd (split b (fst (one_intersection b a))))
  with Not_found -> b

let dist_min_point p point =
  match p with
  | Path p -> (
      match p.pl with
      | [] -> assert false
      | x :: xs ->
          let m = Spline.dist_min_point point x in
          List.fold_left
            (fun ((d1, _) as m1) x ->
              let ((d2, _) as m2) = Spline.dist_min_point point x in
              if d1 < d2 then m1 else m2)
            m xs )
  | Point p -> (P.dist2 p point, 0.)

let dist_min_path p1 p2 =
  match (p1, p2) with
  | Path p1, Path p2 -> (
      match (p1.pl, p2.pl) with
      | [], _ | _, [] -> assert false
      | x :: xs, y :: ys ->
          let acc = Spline.dist_min_spline x y in
          one_to_one2
            (fun ((d1, _) as m1) a b ->
              let ((d2, _) as m2) = Spline.dist_min_spline a b in
              if d1 < d2 then m1 else m2)
            acc xs ys )
  | (Path _ as p1), Point p2 ->
      let d, a = dist_min_point p1 p2 in
      (d, (a, 0.))
  | Point p1, (Path _ as p2) ->
      let d, a = dist_min_point p2 p1 in
      (d, (0., a))
  | Point p1, Point p2 -> (P.dist2 p1 p2, (0., 0.))

let translate t p =
  match p with
  | Path p -> Path { p with pl = List.map (Spline.translate t) p.pl }
  | Point p -> Point (p +/ t)

let transform t = function
  | Path p -> Path { p with pl = List.map (Spline.transform t) p.pl }
  | Point p -> Point (P.transform t p)

let buildcycle _ _ = not_implemented "buildcycle"

let close = function
  | Path p1 (* TODO: tester si il est fermé*) -> Path { p1 with cycle = true }
  | Point _ -> invalid_arg "This path cannot be closed"

let of_bounding_box ({ x = x_min; y = y_min }, { x = x_max; y = y_max }) =
  let dl = { x = x_min; y = y_min } in
  let dr = { x = x_max; y = y_min } in
  let ul = { x = x_min; y = y_max } in
  let ur = { x = x_max; y = y_max } in
  close (create_lines [ ul; ur; dr; dl; ul ])
