(* Generic code to draw level cuves.  To be included in a file that
   defines the drawing primitives. *)

module M = Map.Make(struct
                      type t = int
                      let compare x y = compare (x:int) y
                    end)

(* Module to build a structure helping to determine when the segment
   joining 2 points are on the boundary. *)
module Edge =
struct
  let make() = ref M.empty

  let add_edge t i1 i2 =
    assert(i1 < i2);
    try
      let v = M.find i1 !t in
      v := i2 :: !v
    with Not_found ->
      t := M.add i1 (ref [i2]) !t

  (* Declare the segment joining the points of indexes [i1] and [i2]
     as being part of the boundary.   It is auusmed that [i1 <> i2]. *)
  let add t i1 i2 =
    if i1 < i2 then add_edge t i1 i2 else add_edge t i2 i1

  let on_boundary t i1 i2 =
    assert(i1 < i2);
    try
      let v = M.find i1 !t in List.mem i2 !v
    with Not_found -> false

  (* Tells whether the segment (if any) joining the points of indices
     [i1] and [i2] is on the boundary (according to the information in
     [t]).  It is assumed that [i1 <> i2]. *)
  let on t i1 i2 =
    if i1 < i2 then on_boundary t i1 i2 else on_boundary t i2 i1
end;;

let default_level_eq l1 l2 =
  abs_float(l1 -. l2) <= 1E-8 *. (abs_float l1 +. abs_float l2)

let mid p q = {x = 0.5 *. (p.x +. q.x);  y = 0.5 *. (p.y +. q.y) }

(* Intersection of the curve et level [l] and the line passing through
   (x1,y1) and (x2,y2).  [z1 <> z2] assumed. *)
let intercept {x=x1; y=y1} z1 {x=x2; y=y2} z2 l =
  let d = z1 -. z2 and a = l -. z2 and b = z1 -. l in
  {x = (a *. x1 +. b *. x2) /. d;  y = (a *. y1 +. b *. y2) /. d }

let draw_levels ~boundary (mesh: mesh) (z: vec)
    ?(level_eq=default_level_eq) levels surf =
  let edge = mesh#edge in
  let marker = mesh#edge_marker in
  let pt = mesh#point in
  if NCOLS(edge) = 0 then
    invalid_arg("MOD.level_curves: mesh#edge must be nonempty");
  if NROWS(edge) <> 2 then
    invalid_arg("MOD.level_curves: mesh#edge must have 2 rows (fortran)");
  if Array1.dim marker < NCOLS(edge) then
    invalid_arg("MOD.level_curves: dim mesh#edge_marker < number edges");
  if NCOLS(pt) = 0 then
    invalid_arg("MOD.level_curves: mesh#point must be nonempty");
  if NROWS(pt) <> 2 then
    invalid_arg("MOD.level_curves: mesh#point must have 2 rows (fortran)");
  let bd = Edge.make() in
  (* Draw the boundary edges *)
  for e = FST to LASTCOL(edge) do
    let m = marker.{e} in
    if m <> 0 (* not an interior point *) then begin
      let i1 = GET(edge, FST,e)
      and i2 = GET(edge, SND,e) in
      Edge.add bd i1 i2; (* collect boundary points *)
      match boundary m with
      | None -> ()
      | Some color ->
          let p1 = { x = GET(pt, FST,i1);  y = GET(pt, SND,i1) }
          and p2 = { x = GET(pt, FST,i2);  y = GET(pt, SND,i2) } in
          line surf color p1 p2
    end
  done;
  let tr = mesh#triangle in
  if NCOLS(tr) = 0 then
    invalid_arg("MOD.level_curves: mesh#triangle must be nonempty");
  if NROWS(tr) < 3 then
    invalid_arg("MOD.level_curves: mesh#triangle must have at least 3 \
      rows (fortran) or 3 columns (C)");
  let marker = mesh#point_marker in
  for t = FST to LASTCOL(tr) do
    let i1 = GET(tr, FST,t)
    and i2 = GET(tr, SND,t)
    and i3 = GET(tr, THIRD,t) in
    let p1 = { x = GET(pt, FST,i1);  y = GET(pt, SND,i1) }
    and z1 = z.{i1} in
    let p2 = { x = GET(pt, FST,i2);  y = GET(pt, SND,i2) }
    and z2 = z.{i2} in
    let p3 = { x = GET(pt, FST,i3);  y = GET(pt, SND,i3) }
    and z3 = z.{i3} in
    List.iter
      (fun (l, color) ->
         (* Draw the level curve [l] on the triangle [t] except if
            that curve is on the boundary. *)
         if level_eq l z1 then (
           if level_eq l z2 then (
             if level_eq l z3 then
               (* The entire triangle is at the same level.  Try to
                  remove boundary edges. *)
               if Edge.on bd i1 i2 then
                 if Edge.on bd i1 i3 || Edge.on bd i2 i3 then
                   triangle surf color p1 p2 p3 (* Full triangle ! *)
                 else line surf color p3 (mid p1 p2)
               else (* i1-i2 not on boundary *)
                 if Edge.on bd i1 i3 then
                   if Edge.on bd i2 i3 then triangle surf color p1 p2 p3
                   else line surf color p2 (mid p1 p3)
                 else (* i1-i3 not on boundary *)
                   if Edge.on bd i2 i3 then line surf color p1 (mid p2 p3)
                   else triangle surf color p1 p2 p3 (* Full triangle ! *)
             else (* l = z1 = z2 <> z3 *)
               if not(Edge.on bd i1 i2) then line surf color p1 p2
           )
           else (* l = z1 <> z2 *)
             if level_eq l z3 then (* l = z1 = z3 <> z2 *)
               (if not(Edge.on bd i1 i3) then line surf color p1 p3)
             else
               if (z2 < l && l < z3) || (z3 < l && l < z2) then
                 line surf color p1 (intercept p2 z2 p3 z3 l)
         )
         else if l < z1 then (
           if level_eq l z2 then
             if level_eq l z3 then
               (if not(Edge.on bd i2 i3) then line surf color p2 p3)
             else if l > z3 then (* z3 < l = z2 < z1 *)
               line surf color p2 (intercept p1 z1 p3 z3 l)
             else (* corner point, inside the domain.  Ususally this
                     happens because the level line passes through a
                     triangle corner. *)
               (if marker.{i2} = 0 then point surf i2 p2)
           else if l < z2 then (
             if level_eq l z3 then
               (if marker.{i3} = 0 then point surf i3 p3)
             else if l > z3 then
               line surf color (intercept p1 z1 p3 z3 l)
                 (intercept p2 z2 p3 z3 l)
           )
           else (* z2 < l < z1 *)
             line surf color (intercept p1 z1 p2 z2 l)
               (if level_eq l z3 then p3
                else if l < z3 then intercept p2 z2 p3 z3 l
                else (* l > z3 *)   intercept p1 z1 p3 z3 l)
         )
         else (* l > z1 *) (
           (* Symmetric of [l < z1] with all inequalities reversed *)
           if level_eq l z2 then
             if level_eq l z3 then
               (if not(Edge.on bd i2 i3) then line surf color p2 p3)
             else if l < z3 then (* z1 < l = z2 < z3 *)
               line surf color p2 (intercept p1 z1 p3 z3 l)
             else (* corner point, inside the domain *)
               (if marker.{i2} = 0 then point surf i2 p2)
           else if l > z2 then (
             if level_eq l z3 then
               (if marker.{i3} = 0 then point surf i3 p3)
             else if l < z3 then
               line surf color (intercept p1 z1 p3 z3 l)
                 (intercept p2 z2 p3 z3 l)
           )
           else (* z1 < l < z2 *)
             line surf color (intercept p1 z1 p2 z2 l)
               (if level_eq l z3 then p3
                else if l > z3 then intercept p2 z2 p3 z3 l
                else (* l < z3 *)   intercept p1 z1 p3 z3 l)
         )
      ) levels
  done
;;

type polygon_fill =
| Tri123 (* triangle with edge 1 and cut in edges 2, 3 *)
| Tri231 | Tri312
| Quad123 (* Quadrilateral with edges 1-2 and 1-3 of the triangle cut *)
| Quad231 | Quad312
| Whole | Empty;;

(* base 3: c1 + 1 + 3(c2 + 1) + 9(c3 + 1).  The [c1], [c2] and [c3]
   are the comparisons of the 3 corners with the desired level. *)
let index c1 c2 c3 = c1 + 3 * c2 + 9 * c3 + 13

let super =
  let d = Array.make 27 Empty in
  d.(index( 1) ( 1) ( 1)) <- Whole;
  d.(index( 1) ( 1) ( 0)) <- Whole;
  d.(index( 1) ( 1) (-1)) <- Quad312;
  d.(index( 1) ( 0) ( 1)) <- Whole;
  d.(index( 1) ( 0) ( 0)) <- Whole;
  d.(index( 1) ( 0) (-1)) <- Tri123;
  d.(index( 1) (-1) ( 1)) <- Quad231;
  d.(index( 1) (-1) ( 0)) <- Tri123;
  d.(index( 1) (-1) (-1)) <- Tri123;
  d.(index( 0) ( 1) ( 1)) <- Whole;
  d.(index( 0) ( 1) ( 0)) <- Whole;
  d.(index( 0) ( 1) (-1)) <- Tri231;
  d.(index( 0) ( 0) ( 1)) <- Whole;
  d.(index( 0) ( 0) ( 0)) <- Empty; (* > 0 required *)
  d.(index( 0) ( 0) (-1)) <- Empty;
  d.(index( 0) (-1) ( 1)) <- Tri312;
  d.(index( 0) (-1) ( 0)) <- Empty;
  d.(index( 0) (-1) (-1)) <- Empty;
  d.(index(-1) ( 1) ( 1)) <- Quad123;
  d.(index(-1) ( 1) ( 0)) <- Tri231;
  d.(index(-1) ( 1) (-1)) <- Tri231;
  d.(index(-1) ( 0) ( 1)) <- Tri312;
  d.(index(-1) ( 0) ( 0)) <- Empty;
  d.(index(-1) ( 0) (-1)) <- Empty;
  d.(index(-1) (-1) ( 1)) <- Tri312;
  d.(index(-1) (-1) ( 0)) <- Empty;
  d.(index(-1) (-1) (-1)) <- Empty;
  d

let sub =
  let d = Array.make 27 Empty in
  d.(index( 1) ( 1) ( 1)) <- Empty;
  d.(index( 1) ( 1) ( 0)) <- Empty;
  d.(index( 1) ( 1) (-1)) <- Tri312;
  d.(index( 1) ( 0) ( 1)) <- Empty;
  d.(index( 1) ( 0) ( 0)) <- Empty;
  d.(index( 1) ( 0) (-1)) <- Tri312;
  d.(index( 1) (-1) ( 1)) <- Tri231;
  d.(index( 1) (-1) ( 0)) <- Tri231;
  d.(index( 1) (-1) (-1)) <- Quad123;
  d.(index( 0) ( 1) ( 1)) <- Empty;
  d.(index( 0) ( 1) ( 0)) <- Empty;
  d.(index( 0) ( 1) (-1)) <- Tri312;
  d.(index( 0) ( 0) ( 1)) <- Empty;
  d.(index( 0) ( 0) ( 0)) <- Empty; (* < 0 required *)
  d.(index( 0) ( 0) (-1)) <- Whole;
  d.(index( 0) (-1) ( 1)) <- Tri231;
  d.(index( 0) (-1) ( 0)) <- Whole;
  d.(index( 0) (-1) (-1)) <- Whole;
  d.(index(-1) ( 1) ( 1)) <- Tri123;
  d.(index(-1) ( 1) ( 0)) <- Tri123;
  d.(index(-1) ( 1) (-1)) <- Quad231;
  d.(index(-1) ( 0) ( 1)) <- Tri123;
  d.(index(-1) ( 0) ( 0)) <- Whole;
  d.(index(-1) ( 0) (-1)) <- Whole;
  d.(index(-1) (-1) ( 1)) <- Quad312;
  d.(index(-1) (-1) ( 0)) <- Whole;
  d.(index(-1) (-1) (-1)) <- Whole;
  d

let draw_xxx_level decision name ?(boundary=(fun _ -> Some black))
    (mesh: mesh) (z: vec) l color surf =
  let edge = mesh#edge in
  let edge_marker = mesh#edge_marker in
  let pt = mesh#point in
  if NCOLS(edge) = 0 then
    invalid_arg("MOD" ^ name ^ ": mesh#edge must be nonempty");
  if NROWS(edge) <> 2 then
    invalid_arg("MOD" ^ name ^ ": mesh#edge must have 2 rows (fortran)");
  if Array1.dim edge_marker < NCOLS(edge) then
    invalid_arg("MOD" ^ name ^ ": dim mesh#edge_marker < number edges");
  if NCOLS(pt) = 0 then
    invalid_arg("MOD" ^ name ^ ": mesh#point must be nonempty");
  if NROWS(pt) <> 2 then
    invalid_arg("MOD" ^ name ^ ": mesh#point must have 2 rows (fortran)");
  let tr = mesh#triangle in
  if NCOLS(tr) = 0 then
    invalid_arg("MOD" ^ name ^ ": mesh#triangle must be nonempty");
  if NROWS(tr) < 3 then
    invalid_arg("MOD" ^ name ^ ": mesh#triangle must have at least 3 \
      rows (fortran) or 3 columns (C)");
  for t = FST to LASTCOL(tr) do
    let i1 = GET(tr, FST,t)
    and i2 = GET(tr, SND,t)
    and i3 = GET(tr, THIRD,t) in
    let p1 = { x = GET(pt, FST,i1);  y = GET(pt, SND,i1) }
    and z1 = z.{i1} in
    let p2 = { x = GET(pt, FST,i2);  y = GET(pt, SND,i2) }
    and z2 = z.{i2} in
    let p3 = { x = GET(pt, FST,i3);  y = GET(pt, SND,i3) }
    and z3 = z.{i3} in
    match decision.(index (compare z1 l) (compare z2 l) (compare z3 l)) with
    | Tri123 -> fill_triangle surf color p1 (intercept p1 z1 p2 z2 l)
                                           (intercept p1 z1 p3 z3 l)
    | Tri231 -> fill_triangle surf color p2 (intercept p2 z2 p3 z3 l)
                                           (intercept p2 z2 p1 z1 l)
    | Tri312 -> fill_triangle surf color p3 (intercept p3 z3 p1 z1 l)
                                           (intercept p3 z3 p2 z2 l)
    | Quad123 -> fill_quadrilateral surf color (intercept p1 z1 p2 z2 l)
                                              (intercept p1 z1 p3 z3 l) p3 p2
    | Quad231 -> fill_quadrilateral surf color (intercept p2 z2 p3 z3 l)
                                              (intercept p2 z2 p1 z1 l) p1  p3
    | Quad312 -> fill_quadrilateral surf color (intercept p3 z3 p1 z1 l)
                                              (intercept p3 z3 p2 z2 l) p2 p1
    | Whole -> fill_triangle surf color p1 p2 p3
    | Empty -> ()
  done;
  (* Draw the boundary edges (over the filled area) *)
  for e = FST to LASTCOL(edge) do
    let m = edge_marker.{e} in
    if m <> 0 (* not an interior point *) then begin
      match boundary m with
      | None -> ()
      | Some color ->
        let i1 = GET(edge, FST,e)
        and i2 = GET(edge, SND,e) in
        let p1 = { x = GET(pt, FST,i1);  y = GET(pt, SND,i1) }
        and p2 = { x = GET(pt, FST,i2);  y = GET(pt, SND,i2) } in
        line surf color p1 p2
    end
  done

let draw_super_level ?boundary mesh z level color surf =
  draw_xxx_level super ".super_level" ?boundary mesh z level color surf

let draw_sub_level ?boundary mesh z level color surf =
  draw_xxx_level sub ".sub_level" ?boundary mesh z level color surf
