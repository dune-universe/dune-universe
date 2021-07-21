(*-----------------------------------------------------------------------
** Copyright (C) - Verimag.
** This file may only be copied under the terms of the CeCill
** Public License
**-----------------------------------------------------------------------
**
** File: polyDraw.ml
** Main author: erwan.jahier@univ-grenoble-alpes.fr
*)

let debug = false

(***************************************************************************)

(*
  This algorithm is based on a <<changement de repere>>, so that we can
  draw in a paralellogram that contains P (which is easy), and reject
  bad points.

  The main challenge is to obtain such a paralellogramme that is as small
  as possible (to get fewer bad points).

     P={p0,p1, ..., pn} a convex polyhedron defined by a set of generators.

  0) We take one of the generator as the origin o (e.g., p0).


  1) Let T={t1, ..., tn} where ti is the vector from o to pi, for i=1,n.
     We compute a base of T (<<pivot de gauss>> (PG)):  B'={b1', ..., bd'}

  2) we othogonolise it with a schmit method: B={b1, ..., bd}

  nb : we need an othonormal base because otherwise, in dimension > 3,
  the volume of the parallelogram would much bigger than the one of the
  polyhedron.

  3) let T'={t1', ..., tn'} the vectors of T={t1, ..., tn} expressed
     in the new base. For all i in [1,d], we compute
    min_i = min{bi.tj'/j=1,n} and max_i = max{bi.tj'/j=1,n}

  4) for all i in [1,d], we draw uniformally alpha_i in [-min_i; max_i]

  And the drawn point is Sum(alpha_i.bi)i=1,d

  nb: of course, to draw another point, we need only to redo step 4.

  nb: complexity of the algorithm.
  step 1: O(n^2)
  step 2: O(d^2) (nb: n ougth to be big, not d)
  step 3: O(n.d)
  step 4: O(d.(n-d))

  Therefore the complexity is 0(d.n^2), which is ok for polyhedra
  of dimension smaller than 15.
 *)


open Util
type point = float list
type vector = float list

let fold_left = List.fold_left
let fold_left2 = List.fold_left2
let map = List.map
let map2 = List.map2
let hd = List.hd
let tl = List.tl
let flatten = List.flatten
let iter = List.iter
let partition = List.partition

let (scal_prod : vector -> vector -> float) =
  fun v1 v2 ->
    fold_left2 (fun acc x y -> acc +. x *. y) 0.0 v1 v2

let (vect_norm : vector -> float) =
  fun v ->
    sqrt (scal_prod v v)



(***************************************************************************)
(* step 0 *)

(** returns the coordinate of the smallest hypercube containing a
  convex Polyhedron defined by a set of generators *)
let (hypercube : point list -> (float * float) list) =
  fun pl ->
    List.combine
      (List.fold_left (fun acc p -> List.map2 min p acc) (List.hd pl) (List.tl pl))
      (List.fold_left (fun acc p -> List.map2 max p acc) (List.hd pl) (List.tl pl))


let (_center_of_hypercube : point list -> point) =
  fun pl ->
    let cube = hypercube pl in
      map (fun (min, max) -> (max +. min) /. 2.0) cube



(***************************************************************************)
(* step 1 *)
let (gen_to_vectors : point -> point list -> vector list) =
  fun o pl->
    (map (fun p -> map2 (fun oi pi -> pi -. oi) o p) pl)


let _is_a_null_vector v =
  List.for_all (fun vi -> (abs_float vi) < !(Util.eps)) v

let rec (_reorder_vector : vector list -> vector list) =
  fun vl ->
    (* order this list such that:
       for all i<j<k: ti.tj <= ti.tk (. is the scalar product)
    *)
    match vl with
	[] -> []
      | v::tail ->
	  v::(_reorder_vector (_sort_ortho v tail))
and
  (_sort_ortho : vector -> vector list -> vector list) =
  fun v_comp vl ->
    (List.sort
       (fun v1 v2 ->
	  if
	    (  (abs_float (scal_prod v_comp v1))
	     > (abs_float (scal_prod v_comp v2))
	    )
	  then
	    1
	  else
	    -1
       )
       vl
    )


(***************************************************************************)
(* step 2 *)


type gauss_acc =
    (* type containing the vectors manipulated during the PG elimination,
      as well as a copy of the original vector, and a represention of
      he operation that are made on the vectors.

      When a vector become null during the process, that list of operations
      provide a linear combination of the vector in terms of the vectors
      of the base.
    *)
    (
      vector *    (* the modified vector (column of the matrix) *)
      vector     (* the copy of the original vector *)
    ) list

let rec (compute_base : vector list -> vector list) =
  fun vl ->
    (*
      Returns a base a of vector list as well as linear combinations
      (in term of the base) for vectors that were not elected to be
      in the base.
    *)
    let vvl = map (fun v -> (v, v)) vl in
      compute_base_do vvl

and (compute_base_do : gauss_acc -> vector list) =
  fun vvl ->
    match vvl with
	     [] -> []
      | (v, v_copy)::tail ->
	       match find_non_null_index 0 v with
	           None ->
		          (* v=0: a LC is found *)
		          compute_base_do tail
	         | Some i ->
		          (* v<>0: a base element is found *)
		          let bl = compute_base_do (elim_vect i v tail) in
		            (v_copy::bl)

and (find_non_null_index : int -> vector -> int option) =
  fun i v ->
    if v = [] then None else
      if (abs_float (hd v)) > !Util.eps then Some i else
	     find_non_null_index (i+1) (tl v)

and (elim_vect : int -> vector -> gauss_acc -> gauss_acc) =
  fun i vp vvl ->
    (* set the i^th coord to 0.0 of all vect in vvl using vp (the <<pivot>>) *)
    (map
       (fun (v, v_copy) ->
	       let a = List.nth vp i
	       and b = List.nth v i in
	       let b_on_a = b /. a in
	         (* a is non nul because i comes from find_non_null_index *)
	       let new_v = map2 (fun x y -> x -. b_on_a *. y) v vp in
	         (new_v , v_copy)
       )
       vvl
    )


(***************************************************************************)
(* step 3 *)


let (change_base : vector list ->  vector -> vector) =
  fun bl v ->
    (map
       (fun b -> (fold_left2 (fun acc bi vi-> acc +. bi *. vi) 0.0 b v))
       bl
    )

let (mult_scal_vect : float -> vector -> vector) =
  fun a v ->
    map (fun vi -> a *. vi) v

let (diff_vect : vector -> vector -> vector) =
  map2 (fun v1i v2i -> v1i -. v2i)

let (add_vect : vector -> vector -> vector) =
  map2 (fun v1i v2i -> v1i +. v2i)


let rec (othogonalize : vector list -> vector list) =
  fun ul ->
    (* schmidt process *)
    fold_left (othogonalize_acc) [] ul
and
    (othogonalize_acc : vector list -> vector -> vector list) =
  fun acc u ->
    let a = compute_one_vect u acc in
      a::acc
and
    (compute_one_vect : vector -> vector list -> vector) =
  fun u al ->
    if
      al = []
    then
      mult_scal_vect (1.0 /. (vect_norm u)) u
    else
      let a_scal_u_l = map (fun a -> scal_prod a u) al in
      let a_scal_u_a_l = map2 (fun c a -> mult_scal_vect c a) a_scal_u_l al in
      let sum_vect =
	     fold_left (fun acc v -> add_vect acc v) (hd a_scal_u_a_l) (tl a_scal_u_a_l)
      in
      let a = diff_vect u sum_vect in
	     mult_scal_vect (1.0 /. (vect_norm a)) a



let rec (transpose : 'a list list -> 'a list list) =
  fun fll ->
    if flatten fll = [] then [] else
      let l =  map (hd) fll in
	     l::(transpose (map (tl) fll))


let (compute_bounds2 : vector list -> vector list ->
      (vector * float * float) list) =
  fun bl vl ->
    let vl1 = map (change_base bl) vl in
    let vl1t = transpose vl1 in
      (map2
	      (fun b alpha ->
	         assert (alpha <> []);
	         let minb = fold_left (min) 0.0 alpha
	         and maxb = fold_left (max) 0.0 alpha in
              (* 	    let minb = fold_left (min) (List.hd alpha) (List.tl alpha) *)
              (* 	    and maxb = fold_left (max) (List.hd alpha) (List.tl alpha) in *)
	           (b, minb, maxb)
	      )
	      bl
	      vl1t
      )


(***************************************************************************)
(* step 4 *)


type poly_wrap = point * (vector * float * float) list


let (poly_wrap_volume : poly_wrap -> float) =
  fun paral ->
    (List.fold_left
       (fun acc (v, min, max) -> acc *. (vect_norm v) *. (max -. min))
       1.0
       (snd paral)
    )

let (compute_poly_wrap_do : point list -> poly_wrap) =
  fun pl ->
    (* step 0 to 3 *)

    let o = hd pl in
    let vl = gen_to_vectors o (tl pl) in

    let bl0 = compute_base vl in
    let bl = othogonalize bl0 in
    let vmMl = compute_bounds2 bl vl in
      if debug then
        (
	       output_string stderr "\n ******************* les generateurs:\n";
	       print_fll stderr pl;
	       output_string stderr "\n ******************* l'origine:\n";
	       print_fl stderr o;
	       output_string stderr "\n ******************* les vecteurs:\n";
	       print_fll stderr vl;
	       output_string stderr "\n ******************* la base \n";
	       print_fll stderr bl;
	       output_string stderr "\n ******************* les vecteurs dans la nouvelle base \n";
	       print_fll stderr (map (change_base bl) vl);
	       output_string stderr "\n ******************* la base etirée \n";
	       iter 
	         (fun (b, min, max) -> 
	            output_string stderr 
	              ("\n min = " ^ (string_of_float min) ^ "   max = " ^ 
		              (string_of_float max) ^ " \t->");
	            print_fl stderr b; 
	         )
	         vmMl;
	       (*     output_string stderr "\n ******************* le volume du parallelogramme est  \n"; *)
	       (*     output_string stderr (string_of_float (parallelogram_volume (o, vmMl))); *)
	       (*     flush stderr; *)
        );
      (o, vmMl)

(* tabulate the result as this translation is expensive *)
let poly_wrap_table = ref (Hashtbl.create 1)
let poly_wrap_table_size = ref 0
let (compute_poly_wrap : point list -> poly_wrap) =
  fun pl ->
    (Util.tabulate_result
       poly_wrap_table poly_wrap_table_size 100 compute_poly_wrap_do pl)


let (print_points : point list -> unit) =
  fun pl ->
  List.iter
    (* Used for debugging... *)
    (fun fl ->
       print_string "(";
       List.iter
	      (fun f -> print_float f ; print_string " ")
	      fl;
       print_string ") "
    )	
    pl;
    print_string "\n";
    flush stdout


let (one_point_poly_wrap : poly_wrap -> point) =
  fun (o, bl) ->
    let point =
      (fold_left
	      (fun acc (b, minb, maxb) ->
	         let l = maxb -. minb in
	         let alpha = (Random.float l) +. minb in
	           (map2 (fun acci bi -> acci +. alpha *. bi) acc b)
	      )
	      o
	      bl
      )
    in
      point
        

let (n_points_poly_wrap : poly_wrap -> int -> point list) =
  fun para n ->
    Util.unfold (one_point_poly_wrap) para n





(*
  let pl1=
  [ [0.0; 0.0; 1.0];
  [0.0; 10.0; 0.0];
  [4.0; 0.0; 0.0];
  [8.0; 10.0; 0.0] ]

  let pl3=
  [ [0.0; 0.0];
  [0.0; 10.0];
  [4.0; 0.0];
  [8.0; 0.0] ]

  let pl2 =
  [ [0.0; 0.0];
  [10.0; 2.0];
  [1.0; 0.0];
  [2.0; 10.0] ]


  let rec loop n base o =
  if n > 0 then
  let point = draw_one_point base o in
  print_fl stdout point;
  print_string "\n";
  loop (n-1) base o
  

  let main pl =
  let (o, base) = get_base pl in
  Random.init 1;

  loop (int_of_string Sys.argv.(1)) base o;
  flush stdout;;

  main pl2;;
*)

(***************************************************************************)
(***************************************************************************)
(***************************************************************************)

(* other (unfair) cheap way of drawing points in a convex polyhedron

   Only one of them is exported: draw_point_cheap
*)



let rec (draw_n_points : int -> point list -> point list) =
  fun n pl ->
    if (n = 0  || pl = [] )then [] else
      let i =  Random.int (List.length pl) in
      let p = List.nth pl i in
	     (p::(draw_n_points (n-1) pl))
	
let rec (draw_n_distinct_points : int -> point list -> point list) =
  fun n pl ->
    (* draw n distinct points among pl *)
    if (n = 0 || pl = []) then [] else
      let i = Random.int (List.length pl) in
      let p = List.nth pl i in
      let new_pl = List.filter (fun p1 -> p1 <> p) pl in
	     (p::(draw_n_distinct_points (n-1) new_pl))
	

let (draw_point_pair : point -> point -> point) =
  fun p1 p2 ->
    (* Draw values in the convex hull of points in pl *)
    let alpha = Random.float 1. in
    let p12 =
      List.map2
	     (fun x y ->
	        let xy = alpha *. x +. (1. -. alpha) *. y in
	          xy
	     )
	     p1
	     p2
    in
    p12

let  (draw_point_triangle : point -> point -> point -> point) =
  fun p1 p2 p3 ->
    (* Draw values in the convex hull of points in pl *)
    let alpha0 = Random.float 1. in
    let alpha = alpha0 ** 0.6 in
      (* correct the error made by tossing in an triangle (more
	      points near the corners).

	      This leads to perfect result for symetric polyhedra.

	      For not symetric ones, there are other reasons for
	      non-uniformity: indeed, it ends up in chosing one of the
	      little triangles obtained when drawing all the possible
	      edges. The probability to be in each one of them is roughly
	      the same () but they have not the same surface, which means
	      that we draw more often in the little ones...

      *)
    let p12 = draw_point_pair p1 p2 in
    let p123 =
      List.map2
	     (fun x y ->
	        let xy = alpha *. x +. (1. -. alpha) *. y in
	          xy
	     )
	     p12
	     p3
	 in
    p123
	

let rec (draw_point1 : point list -> point) =
  fun pl ->
    (* Draw values in the convex hull of points in pl

       We can either take 3 points only, or 3 by 3 until there is
       no point left. Or 2 by 2. Or ...
       It never gives good repartition, in particular
       when the polyhedron is not symetric.

       It was basically the solution used in Lurette V1.
    *)
    let p =
      match pl with
	       [] -> assert false
        | [p] -> p
        | p1::p2::[] -> draw_point_pair p1 p2
        | p1::p2::p3::[] ->
	       draw_point_triangle p1 p2 p3
        | p1::p2::p3::tail ->
	       draw_point1 ((draw_point_triangle p1 p2 p3)::tail)
    in
    p	

let (_draw_point2 : point list -> point) =
  fun pl ->
    (*
      o Take 2 of the vertices
      o Perform a fair draw between them
      o Replace them by this new point
      o Go on until only one point is left

      Very bad when the polyhedron is not symetric...
    *)

    let alpha_l0 = map (fun _ -> Random.float 1.) pl in
    let sum = fold_left (fun acc alpha -> acc +. alpha) 0.0 alpha_l0 in
    let alpha_l =  map (fun alpha -> alpha /. sum) alpha_l0 in
    let point =
      fold_left2
	     (fun acc p alpha -> (map2 (fun acc_i x_i -> acc_i +. alpha *. x_i) acc p))
	     (map (fun _ -> 0.0) (hd pl))
	     pl
	     alpha_l
    in
    point

let (draw_point_cheap : point list -> point) = draw_point1



let (distance : point -> point -> float) =
  fun p1 p2 ->
    let sum_square = fold_left2 (fun acc x y -> (x -. y) ** 2.0 +. acc) 0.0 p1 p2 in
      sqrt sum_square

let (_draw_point3 : point list -> point) =
  (*
    1 - I compute the size of each edges
    2 - I draw 2 points on the edges (taking into account their size)
    3 - I draw uniformally between those 2 points

    Drawback: for n genetators, i have n^2 edges ...

    pas terrible...
  *)
  fun pl ->
    let edges0 =
      Util.cartesian_product pl pl (fun p1 p2 -> (p1,p2, distance p1 p2))
    in
      (* XXX Sortir ce calcul de la boucle !!!!! XXX *)
    let edges = fst (partition (fun (p1,p2, _d) -> p1 <> p2) edges0) in
    let sum_distance = fold_left (fun acc (_,_,d) -> acc +. d) 0.0 edges in
    let rec find_edge f el =
      match el with
	       [] -> assert false
	     | (p1,p2,d)::[] -> (p1, p2, d, f)
	     | (p1,p1',d1)::(p2,p2',d2)::tail ->
	         if
		        f < d1
	         then
		        (p1, p1', d1, f)
	         else
		        let new_f = f -. d1 in
		          if
		            new_f < d2
		          then
		            (p2, p2', d2, new_f)
		          else
		            find_edge new_f ((p2,p2',d2)::tail)
    in
    let f12 = Random.float sum_distance in
    let f34 = Random.float sum_distance in
    let (p1, p2, _d, _f) = find_edge f12 edges in
    let (p3, p4, d, f) = find_edge f34 edges in
    let p12 =
      map2 (fun x1 x2 -> ((f /. d) *. x1 +. (1.0 -. (f /. d)) *. x2)) p1 p2
    and p34 =
      map2 (fun x1 x2 -> ((f /. d) *. x1 +. (1.0 -. (f /. d)) *. x2)) p3 p4
    in
    let alpha = Random.float 1.0 in
    let p = map2 (fun x1 x2 -> alpha *. x1 +. (1.0 -. alpha) *. x2) p12 p34 in
    p


let (_draw_point4 : point list -> point) =
  (*
    o for each generator, draw an alpha_i in [0;1]
    o normalise the alpha_i (/ sum alpha_i)
    o compute the point

    XXX not terrible with losange3d3.luc ...
  *)
  fun pl ->
    (* coeff = 2.5 is better than 1.O (1.0 draw in the center)     *)
    let coeff = 1.0 in
    (*     let coeff = 2.5 in *)
    let alphal = map (fun _ -> (Random.float 1.0) ** coeff) pl in
    let alpha_s = fold_left (fun acc alpha -> acc +. alpha) 0.0 alphal in
    let alphal' = map (fun alpha -> alpha /. alpha_s) alphal in
    let point =
      fold_left2
	     (fun acc p alpha -> (map2 (fun acc_i x_i -> acc_i +. alpha *. x_i) acc p))
	     (map (fun _ -> 0.0) (hd pl))
	     pl
	     alphal'
    in
    point

(* exported *)
let (_draw_point_cheap2 : point list -> point) =
  fun zl ->
    (* This algorithm is come from "Random fonts for the simulation
       of handwriting" by Devroye.
       The result is not too bad, but not perfect...
    *)
    let ul0 = List.map (fun _ -> Random.float 1.0) (List.tl zl) in
    let ul = List.sort compare ul0 in
    let (u_last, sl0) =
      List.fold_left
	     (fun (u', l) u -> (u, (u -. u')::l))
	     (List.hd ul, [])
	     (List.tl ul)
    in
    let sl = [List.hd ul] @ sl0 @ [(1. -. u_last)] in
    let drawn_point =
      List.fold_left2
	     (fun acc s z -> List.map2 (fun zi acci -> acci +. zi *. s) z acc)
	     (List.map (fun _ -> 0.0) (List.hd zl))
	     sl
	     zl
    in
    drawn_point


let (point_to_vector : point -> Vector.t) =
  fun point ->
    let row = Vector.make ((List.length point) + 3) in
    let i = ref 3 in
    let denom = 1.0 /. !(Util.eps) in
    let point_int = List.map (fun p -> int_of_float (p *. denom)) point in
      Vector.set row 0 1; (* it is not a line *)
      Vector.set row 1 (int_of_float denom);
      Vector.set row 2 0; (* this is the epsilon dimension *)
      List.iter
	(fun p -> Vector.set row !i p; i:=!i+1)
	point_int;
      row



exception TooSmall

let rec (_draw_point_rw : Poly.t -> point list -> point) =
  (* rw stands for random walk The idea is to perform a random
     walk. The main difficulty is that we can not do a classic RW
     because if one dimension is <<flat>>, we are dead (ie, almost
     certainly outside the polyhedron). Therefore, instead of walking
     in any direction, we walk along vectors of the polyhedron.  Those
     vectors can be obtained, let's say, by considering 3 non-aligned
     vertices, or the current point plus 2 vertices.

     Such a RW have 2 parameters. The step of the walk, and its length.
     We need to carefully choose them according to the polyhedron
     volume, otherwise, the rw would not be fair (it needs to have a
     chance to explore the whole polyhedron!).

     The idea there is to chose a step that is somehow proportional the
     polyhedron diameter.

     The main drawback is the efficiency, because we need to check at
     each step if we are still inside the polyhedron. Moreover it can
     happen relatively often that it is the case...

     One solution to accelerate the finding of a valid point is to
     decrease the volume of the ellipse we draw in at each failure. But
     then the probability to draw in corners become smaller.

  *)
  fun poly pl ->
    (*     let pl2 = draw_n_distinct_points 3 pl in *)
    let point =
      draw_point_cheap pl
    in
    let dim = Poly.dim poly in
    let walk_step = (float_of_int dim) *. 10.0 in
    let walk_length = 5 in
      (*
	     This heurictic is not too bad (and cheap), but not good enough
	     for asymmetric polyhedron.
	     Therefore we compose it with a RW; hence having a not so long RW
	     will give correct result.

	     nb : if step is too big, more retries are needed
	     (favorises draws out of the poly).
	     if it is too small, it requires too much steps to get a uniform RW
	     
      *)

    _rw poly walk_length walk_step pl point

(*
  There is a efficiency tradeoff betwenn the length and the step
  size.
  I am not sure what the best compromise is.
  In particular, are there values that fit for any dimension,
  and any polyhedron shape ???

  Clearly, the step ougth to depend on the dimension.

*)

and (_rw : Poly.t -> int ->float -> point list -> point -> point) =
  fun poly length step pl point0 ->
    if length = 0 then point0 (* end of the walk *) else
      (* choose 2 vertices that will serve to make up our two vectors t1 and t2 *)
      let s1s2 = draw_n_distinct_points 2 pl in
      let s1 = hd s1s2
      and s2 = hd (tl s1s2)
      in
      let sum1 =
	     fold_left2 (fun acc x x' -> acc +. abs_float(x -. x')) 0.0 s1 point0
      and sum2 =
	     fold_left2 (fun acc x x' -> acc +. abs_float(x -. x')) 0.0 s2 point0
      in
      let t1 = map2 (fun x x' -> x -. x') s1 point0
      and t2 = map2 (fun x x' -> x -. x') s2 point0
      in
      let point =
	     try _check_point_is_inside poly point0 step t1 t2 sum1 sum2
	     with TooSmall ->
          (* 	  output_string stderr "Too small !!\n"; flush stderr; *)
	       (* nb: if we are on the enveloppe, there are directions for which there is
	          no hope in finding a point inside...*)
	       _rw poly length step pl point0
      in
	   _rw poly (length-1) step pl point
		  
and (_check_point_is_inside : Poly.t -> point -> float ->
     point -> point -> float -> float  -> point) =
  fun poly point0 step s1 s2 sum1 sum2 ->
    (*
      this function draw a new point of the walk with the vectors t1 and t2,
      and check that this point is inside [poly].

      If it is not, we keep the draw direction,
    *)

    let delta1 = ((Random.float step) -. (step /. 2.0)) /. sum1 in
    let delta2 = ((Random.float step) -. (step /. 2.0)) /. sum2 in
    if (abs_float step) < !(Util.eps) then raise TooSmall (* cf nb above *) else
	   let point =
        Util.list_map3
	       (fun x t1 t2 -> x +. delta1 *. t1 +. delta2 *. t2)
	       point0 s1 s2
      in
      let v = point_to_vector point in
      if
	     Poly.is_generator_included_in v poly
      then	
	     point
      else
	     (
	           (* We divide the step by two until we are inside the polyhedron *)
	       _check_point_is_inside
	         poly point0 (step /. 2.0) s1 s2 sum1 sum2
	     )



(**********************************************************************)
(**********************************************************************)
(**********************************************************************)

