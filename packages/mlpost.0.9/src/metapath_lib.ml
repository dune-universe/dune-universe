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

module P = Point_lib
open Point_lib
open Point_lib.Infix
module S = Spline_lib

let square x = x *. x

let debug = false

let info = debug || false

type point = P.t

type direction = DVec of point | DCurl of float | DNo

type joint =
  | JLine
  | JCurve of direction * direction
  | JCurveNoInflex of direction * direction
  | JTension of direction * float * float * direction
  | JControls of point * point

let _ = JLine

type knot = point

type t =
  | Start of knot
  | Cons of t * joint * knot
  | Start_Path of Spline.t list
  | Append_Path of t * joint * Spline.t list

open Format

let rec print_dir fmt = function
  | DNo -> fprintf fmt "DNo"
  | DVec p -> fprintf fmt "DVec %a" Point_lib.print p
  | DCurl f -> fprintf fmt "Dcurl %f" f

and print_knot = Point_lib.print

and print_joint fmt = function
  | JLine -> fprintf fmt "JLine"
  | JCurve (d1, d2) -> fprintf fmt "JCurve(%a,%a)" print_dir d1 print_dir d2
  | JCurveNoInflex _ -> fprintf fmt "JCurveNoInflex"
  | JTension (_, f1, f2, _) -> fprintf fmt "JTension (%f,%f)" f1 f2
  | JControls (p1, p2) ->
      fprintf fmt "JControls (%a,%a)" Point_lib.print p1 Point_lib.print p2

and print fmt = function
  | Start k1 -> fprintf fmt "[%a" print_knot k1
  | Cons (p, j, k) -> fprintf fmt "%a;%a-%a" print p print_joint j print_knot k
  | Start_Path p -> fprintf fmt "{%a}" S.print_splines p
  | Append_Path (p1, j, p2) ->
      fprintf fmt "%a;%a-%a" print p1 print_joint j S.print_splines p2

type tension = float

let tunity : tension = 1.

(* Metafont is wiser in the computation of
   calc_value, calc_ff, curl_ratio, ... *)
(* dk1, uk1 are d_k-1, u_k-1 *)
(* ((3-α)α²γ + β³) / ( α³γ + (3-β)β²) *)
let curl_ratio gamma alpha1 beta1 =
  let alpha = 1. /. alpha1 and beta = 1. /. beta1 in
  let gamma =
    if alpha <= beta then gamma *. square (alpha /. beta) else gamma
  in
  let beta = if alpha <= beta then beta else beta *. square (beta /. alpha) in
  ((gamma *. (3. -. alpha)) +. beta) /. ((alpha *. gamma) +. 3. -. beta)

let reduce_angle x =
  (* 292. define reduce angle (#) *)
  if abs_float x > 180. then if x > 0. then x -. 360. else x +. 360. else x

let velocity st ct sf cf t =
  let num =
    (* 2 + √2(st - sf/16)(sf - st/16) * (ct -cf) *)
    2. +. (sqrt 2. *. (st -. (sf /. 16.)) *. (sf -. (st /. 16.)) *. (ct -. cf))
  in
  let denom =
    (* 3(1+1/2(√5 - 1))ct + 1/2(3-√5)cf *)
    3.
    *. (1. +. (0.5 *. (sqrt 5. -. 1.) *. ct) +. (0.5 *. (3. -. sqrt 5.) *. cf))
  in
  min (num /. t /. denom) 4.

let calc_value dk1 dk art alt uk1 =
  (* Calculate the values aa = Ak /Bk , bb = Dk /Ck ,
     dd = (3 - k-1 )dk,k+1 , ee = (3 - k+1 )dk-1,k , and
     cc = (Bk - uk-1 Ak )/Bk 288 *)
  let aa = 1. /. ((3. *. art) -. 1.) in
  let bb = 1. /. ((3. *. alt) -. 1.) in
  let cc = 1. -. (uk1 *. aa) in
  let dd = dk *. (3. -. (1. /. art)) in
  let ee = dk1 *. (3. -. (1. /. alt)) in
  (aa, bb, cc, dd, ee)

let calc_ff cc dd ee art alt =
  (* Calculate the ratio ff = Ck /(Ck + Bk - uk-1 Ak ) 289 *)
  if alt < art then ee /. (ee +. (cc *. dd *. ((alt /. art) ** 2.)))
  else
    let ee = ee *. ((art /. alt) ** 2.) in
    ee /. (ee +. (cc *. dd))

type path_type =
  | Endpoint
  | Explicit of point
  | Open of tension
  | Endcycle of tension
  | Curl of tension * float
  | Given of tension * float

let tension = function
  | Endpoint -> 1. (* not sure ... *)
  | Explicit _ -> assert false
  | Open t | Endcycle t | Curl (t, _) | Given (t, _) -> t

type kpath = {
  mutable left : path_type;
  mutable right : path_type;
  mutable link : kpath;
  mutable coord : point;
}

let dumb_pos = { x = 666.; y = 42. }

let dumb_dir = Endcycle 42.

let rec dumb_link =
  { left = dumb_dir; right = dumb_dir; coord = dumb_pos; link = dumb_link }

let mk_kpath ?(left = dumb_dir) ?(right = dumb_dir) ?(link = dumb_link)
    ?(coord = dumb_pos) () =
  { left; right; link; coord }

let print_path_type fmt = function
  | Endpoint -> fprintf fmt "Endpoint"
  | Explicit p -> fprintf fmt "Explicit %a" P.print p
  | Open t -> fprintf fmt "Open %f" t
  | Endcycle t -> fprintf fmt "Endcycle %f" t
  | Curl (t, f) -> fprintf fmt "Curl (%f,%f)" t f
  | Given (t, f) -> fprintf fmt "Given (%f,%f)" t f

let print_one_kpath fmt q =
  fprintf fmt "@[{left = @[%a@];@,coord = @[%a@];@,right = @[%a@]}@]"
    print_path_type q.left P.print q.coord print_path_type q.right

let print_kpath fmt p =
  let rec aux fmt q =
    fprintf fmt
      "@[{left = @[%a@];@,coord = @[%a@];@,right = @[%a@];@,link= @[%a@]}@]"
      print_path_type q.left P.print q.coord print_path_type q.right
      (fun fmt q ->
        if q != p && q != dumb_link then aux fmt q else fprintf fmt "...")
      q.link
  in
  aux fmt p

let pi = acos (-1.)

let n_arg =
  let coef = 180. /. pi in
  fun p -> atan2 p.y p.x *. coef

let sincos =
  let coef = pi /. 180. in
  fun a ->
    let a = coef *. a in
    (sin a, cos a)

let set_controls p q at af rtension ltension deltaxy =
  (* procedure set controls (p, q : pointer ; k : integer ) 299 *)
  let st, ct = at in
  let sf, cf = af in
  let rr = velocity st ct sf cf (abs_float rtension) in
  let ss = velocity sf cf st ct (abs_float ltension) in
  let rr, ss =
    (* Decrease the velocities if necessary 300 *)
    if
      (rtension < 0. || ltension < 0.)
      && ((st >= 0. && sf >= 0.) || (st <= 0. && sf <= 0.))
    then
      let sine = (2. *. cf *. abs_float st) +. (cf *. abs_float sf) in
      if sine > 0. then
        let choice t s k =
          let sa = abs_float s in
          if t < 0. && sa < k *. sine then sa /. sine else k
        in
        (choice rtension sf rr, choice ltension st ss)
      else (rr, ss)
    else (rr, ss)
  in
  let sb = p.coord +/ (rr */ ((ct */ deltaxy) +/ (st */ P.swapmy deltaxy))) in
  p.right <- Explicit sb;
  let sc = q.coord -/ (ss */ ((cf */ deltaxy) +/ (sf */ P.swapmx deltaxy))) in
  q.left <- Explicit sc

let print_array print fmt = Array.iter (fun e -> fprintf fmt "%a;@," print e)

let print_float fmt = fprintf fmt "%f"

let solve_choices p q n deltaxyk deltak psik =
  (* If Only one simple arc*)
  match p with
  | { right = Given (rt, rp); link = { left = Given (lt, lq); _ }; _ } ->
      (* Reduce to simple case of two givens and return 301 *)
      let aa = n_arg deltaxyk.(0) in
      let at = sincos (rp -. aa) in
      let saf, caf = sincos (lq -. aa) in
      let af = (-.saf, caf) in
      set_controls p q at af rt lt deltaxyk.(0)
  | { right = Curl (tp, _); link = { left = Curl (tq, _); _ }; _ } ->
      (* Reduce to simple case of straight line and return 302 *)
      let lt = abs_float tq and rt = abs_float tp in
      let tmp = P.sign deltaxyk.(0) in
      let fx p t f =
        let d = if t = tunity then deltaxyk.(0) +/ tmp else deltaxyk.(0) in
        Explicit (f p.coord (d // (3. *. t)))
      in
      p.right <- fx p rt ( +/ );
      q.left <- fx q lt ( -/ )
  | { link = t; _ } as s ->
      let thetak = Array.make (n + 2) 0. in
      let uu = Array.make (n + 1) 0. in
      let vv = Array.make (n + 1) 0. in
      let ww = Array.make (n + 1) 0. in
      let curl_eq lt rt cc =
        (* Set up the equation for a curl 294 / 295 *)
        let lt = abs_float lt and rt = abs_float rt in
        if lt = tunity && rt = tunity then ((2. *. cc) +. 1.) /. (cc +. 2.)
        else curl_ratio cc rt lt
      in
      ( match p with
      | { right = Given (_, rp); _ } ->
          (* Set up the equation for a given value of 0 293 *)
          vv.(0) <- reduce_angle (rp -. n_arg deltaxyk.(0));
          uu.(0) <- 0.;
          ww.(0) <- 0.
      | { right = Curl (tp, cc); link = { left = lt; _ }; _ } ->
          uu.(0) <- curl_eq (tension lt) tp cc;
          vv.(0) <- -.(psik.(1) *. uu.(0));
          ww.(0) <- 0.
      | { right = Open _; _ } ->
          uu.(0) <- 0.;
          vv.(0) <- 0.;
          ww.(0) <- 1.
      | _ ->
          (* { there are no other cases } in 285 because of 273 *)
          assert false );
      (let rec aux k r = function
         (* last point*)
         | { left = Curl (t, cc); _ } ->
             let ff = curl_eq t (tension r.right) cc in
             thetak.(n) <- -.(vv.(n - 1) *. ff /. (1. -. (ff *. uu.(n - 1))))
         | { left = Given (_, f); _ } ->
             (* Calculate the given value of n and goto found 292 *)
             thetak.(n) <- reduce_angle (f -. n_arg deltaxyk.(n - 1))
         | { link = t; _ } as s -> (
             (*end cycle , open : Set up equation to match mock curvatures
               at zk ; then goto found with n adjusted to equal 0 , if a
               cycle has ended 287 *)
             let art = abs_float (tension r.right) in
             let alt = abs_float (tension t.left) in
             let aa, bb, cc, dd, ee =
               calc_value deltak.(k - 1) deltak.(k) art alt uu.(k - 1)
             in
             let art = abs_float (tension s.right) in
             let alt = abs_float (tension s.left) in
             let ff = calc_ff cc dd ee art alt in
             uu.(k) <- ff *. bb;
             (* Calculate the values of vk and wk 290 *)
             let acc = -.(psik.(k + 1) *. uu.(k)) in
             ( match r.right with
             | Curl _ ->
                 (*k=1...*)
                 ww.(k) <- 0.;
                 vv.(k) <- acc -. (psik.(1) *. (1. -. ff))
             | _ ->
                 let ff = (1. -. ff) /. cc in
                 let acc = acc -. (psik.(k) *. ff) in
                 let ff = ff *. aa in
                 vv.(k) <- acc -. (vv.(k - 1) *. ff);
                 ww.(k) <- -.(ww.(k - 1) *. ff) );
             match s.left with
             | Endcycle _ ->
                 (* Adjust n to equal 0 and goto found 291 *)
                 let aa, bb =
                   let rec aux aa bb = function
                     | 0 -> (vv.(n) -. (aa *. uu.(n)), ww.(n) -. (bb *. uu.(n)))
                     | k ->
                         aux
                           (vv.(k) -. (aa *. uu.(k)))
                           (ww.(k) -. (bb *. uu.(k)))
                           (k - 1)
                   in
                   aux 0. 1. (n - 1)
                 in
                 let aa = aa /. (1. -. bb) in
                 thetak.(n) <- aa;
                 vv.(0) <- aa;
                 for k = 1 to n - 1 do
                   vv.(k) <- vv.(k) +. (aa *. ww.(k))
                 done
             | _ -> aux (k + 1) s t )
       in
       aux 1 s t);
      (* Finish choosing angles and assigning control points 297 *)
      for k = n - 1 downto 0 do
        thetak.(k) <- vv.(k) -. (thetak.(k + 1) *. uu.(k))
      done;
      let rec aux k = function
        | _ when k = n -> ()
        | { right = rt; link = { left = lt; _ } as t; _ } as s ->
            let at = sincos thetak.(k) in
            let af = sincos (-.psik.(k + 1) -. thetak.(k + 1)) in
            set_controls s t at af (tension rt) (tension lt) deltaxyk.(k);
            aux (k + 1) t
      in
      aux 0 p

let make_choices knots =
  (* If consecutive knots are equal, join them explicitly 271*)
  (let p = ref knots in
   while !p != knots do
     ( match !p with
     | { coord; right = Given _ | Curl _ | Open _; link = q; _ } as k
       when coord == q.coord -> (
         if debug then
           Format.printf "@[find consecutive knots :k = @[%a@];@,q = @[%a@]@]@."
             print_one_kpath k print_one_kpath q;
         k.right <- Explicit coord;
         q.left <- Explicit coord;
         ( match k.left with
         | Open tension -> k.left <- Curl (tension, tunity)
         | _ -> () );
         match k.right with
         | Open tension -> k.right <- Curl (tension, tunity)
         | _ -> () )
     | _ -> () );
     p := !p.link
   done);
  (*Find the first breakpoint, h, on the path;
    insert an artificial breakpoint if the path is an unbroken cycle 272*)
  let h =
    let rec aux = function
      | ( { left = Endpoint | Endcycle _ | Explicit _ | Curl _ | Given _; _ }
        | { right = Endpoint | Endcycle _ | Explicit _ | Curl _ | Given _; _ }
          ) as h ->
          h
      | { left = Open t; _ } as h when h == knots ->
          knots.left <- Endcycle t;
          knots
      | { link = h; _ } -> aux h
    in
    aux knots
  in
  if debug then Format.printf "@[find h :h = @[%a@]@]@." print_one_kpath h;
  (*repeat Fill in the control points between p and the next breakpoint,
    then advance p to that
    breakpoint 273
    until p = h*)
  let rec aux = function
    | { right = Endpoint | Explicit _; link = q; _ } -> if q != h then aux q
    | p ->
        let n, q =
          let rec search_q n = function
            | { left = Open _; right = Open _; link = q; _ } ->
                search_q (n + 1) q
            | q -> (n, q)
          in
          search_q 1 p.link
        in
        if debug then
          Format.printf "@[search_q : n = %i;@,p = @[%a@];@,q = @[%a@]@]@." n
            print_one_kpath p print_one_kpath q;
        (*Fill in the control information between consecutive breakpoints
          p and q 278*)
        (* Calculate the turning angles k and the distances dk,k+1 ;
           set n to the length of the path 281*)
        let deltaxyk = Array.make (n + 1) P.zero in
        (* Un chemin sans cycle demande un tableau de taille n,
           de n+1 avec cycle *)
        let deltak = Array.make (n + 1) 0. in
        let psik = Array.make (n + 2) 0. in
        (let rec fill_array k = function
           (* K. utilise des inégalitées pour k=n et k = n+1 -> k>=n*)
           | s when k = n && match s.left with Endcycle _ -> false | _ -> true
             ->
               psik.(n) <- 0.
               (* On a fait un tour le s.left précédent était un Endcycle *)
           | _ when k = n + 1 -> psik.(n + 1) <- psik.(1)
           | { link = t; _ } as s ->
               deltaxyk.(k) <- t.coord -/ s.coord;
               deltak.(k) <- P.norm deltaxyk.(k);
               ( if k > 0 then
                 let { x = cosine; y = sine } =
                   deltaxyk.(k - 1) // deltak.(k - 1)
                 in
                 let m = Matrix.linear cosine sine (-.sine) cosine in
                 let psi = n_arg (Point_lib.transform m deltaxyk.(k)) in
                 psik.(k) <- psi );
               fill_array (k + 1) t
         in
         fill_array 0 p);
        if debug then (
          Format.printf "deltaxyk : %a@." (print_array P.print) deltaxyk;
          Format.printf "deltak : %a@." (print_array print_float) deltak;
          Format.printf "psik : %a@." (print_array print_float) psik );
        (*Remove open types at the breakpoints 282*)
        ( match q with
        | { left = Open t; _ } -> q.left <- Curl (t, 1.) (* TODO cas bizarre *)
        | _ -> () );
        ( match p with
        | { left = Explicit pe; right = Open t; _ } ->
            let del = p.coord -/ pe in
            if del = P.zero then p.right <- Curl (t, 1.)
            else p.right <- Given (t, n_arg del)
        | _ -> () );
        (*Format.printf "@[remove : p = @[%a@];@,q = @[%a@]@]@."
          print_one_kpath p print_one_kpath q;*)
        (* an auxiliary function *)
        solve_choices p q n deltaxyk deltak psik;
        if q != h then aux q
  in
  aux h

(* let tension_of = function
 *   | JTension (_,t1,t2,_) -> (t1,t2)
 *   | JCurveNoInflex (_,_) -> (-1.,-1.)
 *   | _ -> (1.,1.) *)

let direction t = function
  | DNo -> Open t
  | DVec p -> Given (t, n_arg p)
  | DCurl f -> Curl (t, f)

let right_of_join p = function
  | JLine -> Explicit p
  | JControls (c, _) -> Explicit c
  | JCurve (d, _) -> direction 1. d
  | JCurveNoInflex (d, _) -> direction 1. d (*pas totalement correcte*)
  | JTension (d, f, _, _) -> direction f d

let left_of_join p = function
  | JLine -> Explicit p
  | JControls (_, c) -> Explicit c
  | JCurve (_, d) -> direction 1. d
  | JCurveNoInflex (_, d) -> direction 1. d (*pas totalement correcte*)
  | JTension (_, _, f, d) -> direction f d

let path_to_meta nknot l =
  let rec aux aknot = function
    | [] -> assert false
    | [ a ] ->
        let sa, sb, sc, sd = Spline.explode a in
        nknot.left <- Explicit sc;
        nknot.coord <- sd;
        aknot.link <- nknot;
        aknot.right <- Explicit sb;
        aknot.coord <- sa;
        ()
    | a :: l ->
        let sa, sb, sc, _ = Spline.explode a in
        let nknot = mk_kpath ~left:(Explicit sc) () in
        aknot.link <- nknot;
        aknot.right <- Explicit sb;
        aknot.coord <- sa;
        aux nknot l
  in
  let aknot = mk_kpath ~left:Endpoint () in
  aux aknot l;
  aknot

let print_option f fmt = function
  | None -> Format.fprintf fmt "None"
  | Some e -> f fmt e

let kmeta_to_path ?cycle meta =
  if info then
    Format.printf "@[before (cycle:%a) : @[%a@]@]@." (print_option print_joint)
      cycle print meta;
  let rec to_knots aknot = function
    | Start p ->
        aknot.coord <- p;
        aknot.left <- Endpoint;
        aknot
    | Cons (pa, join, p) ->
        aknot.coord <- p;
        aknot.left <- left_of_join p join;
        let nknot = mk_kpath ~right:(right_of_join p join) ~link:aknot () in
        to_knots nknot pa
    | Start_Path pa -> path_to_meta aknot pa
    | Append_Path (p1, join, p2) ->
        let aknot2 = path_to_meta aknot p2 in
        aknot2.left <- left_of_join aknot2.coord join;
        let nknot =
          mk_kpath ~right:(right_of_join aknot2.coord join) ~link:aknot2 ()
        in
        to_knots nknot p1
  in
  let lknots = mk_kpath ~right:Endpoint () in
  let knots = to_knots lknots meta in
  lknots.link <- knots;
  (* Choose control points for the path and put the result into cur exp 891 *)
  (* when nocycle *)
  ( match cycle with
  | Some join ->
      lknots.right <- right_of_join knots.coord join;
      knots.left <- left_of_join knots.coord join
  | None -> (
      (match knots.right with Open t -> knots.right <- Curl (t, 1.) | _ -> ());
      match lknots.left with Open t -> lknots.left <- Curl (t, 1.) | _ -> () )
  );
  if debug then Format.printf "@[middle : @[%a@]@]@." print_kpath knots;
  make_choices knots;
  if debug then Format.printf "@[after : @[%a@]@]@." print_kpath knots;
  let rec aux = function
    | { right = Endpoint; _ } -> []
    | {
        right = Explicit sb;
        coord = sa;
        link = { left = Explicit sc; coord = sd; _ } as s;
        _;
      } ->
        Spline.create sa sb sc sd :: (if s == knots then [] else aux s)
    | _ -> assert false
  in
  aux knots

let kto_path ?cycle = function
  | Start p -> S.Point p
  | mp ->
      let res =
        S.Path { S.pl = kmeta_to_path ?cycle mp; cycle = cycle <> None }
      in
      if info then Format.printf "@[end : @[%a@]@]@." S.print res;
      res

let knot p = p

let vec_direction p = DVec p

let curl_direction f = DCurl f

let no_direction = DNo

let equalize_dir = function
  (* Faut-il égaliser l'un avec l'autre *et* l'autre avec l'un? *)
  (*Put the pre-join direction information into node q 879 *)
  | DNo, (DVec _ as y) -> (y, y)
  | c -> c

let start k = Start k

let line_joint =
  (*JLine but metafont defined -- as a macro for*)
  JCurve (curl_direction 1., curl_direction 1.)

let curve_joint dir1 dir2 = JCurve (dir1, dir2)

let curve_no_inflex_joint dir1 dir2 = JCurveNoInflex (dir1, dir2)

let tension_joint dir1 f1 f2 dir2 = JTension (dir1, f1, f2, dir2)

let controls_joint p1 p2 = JControls (p1, p2)

let concat p j k = Cons (p, j, k)

let rec append p j = function
  | Start knot -> Cons (p, j, knot)
  | Cons (p2, j2, k2) -> Cons (append p j p2, j2, k2)
  | Start_Path p2 -> Append_Path (p, j, p2)
  | Append_Path (p2, j2, p3) -> Append_Path (append p j p2, j2, p3)

let to_path p = kto_path p

let cycle j p = kto_path ~cycle:j p

let from_path = function S.Path p -> Start_Path p.S.pl | S.Point p -> Start p

module Approx = struct
  let lineto = S.create_lines

  let simple_join = curve_joint no_direction no_direction

  let curve l =
    let rec aux = function
      | [] -> assert false
      | [ a ] -> start (knot a)
      | a :: l -> concat (aux l) simple_join (knot a)
    in
    aux (List.rev l)

  let fullcircle_ l =
    let l2 = l /. 2. in
    cycle simple_join
      (curve
         [
           { x = l2; y = 0. };
           { x = 0.; y = l2 };
           { x = -.l2; y = 0. };
           { x = 0.; y = -.l2 };
         ])

  let fullcircle1 = lazy (fullcircle_ 1.)

  let fullcircle = function 1. -> Lazy.force fullcircle1 | l -> fullcircle_ l

  let halfcirle l =
    (* 2. because fullcircle is defined with 4 points *)
    S.subpath (fullcircle l) 0. 2.

  let quartercircle l = S.subpath (fullcircle l) 0. 1.

  let unitsquare l =
    let p = { x = 0.; y = 0. } in
    S.close
      (S.create_lines
         [ p; { x = l; y = 0. }; { x = l; y = l }; { x = 0.; y = l }; p ])
end
