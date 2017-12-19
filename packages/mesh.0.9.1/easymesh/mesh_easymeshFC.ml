(* FORTRAN/C functions *)

open Printf
open Scanf
open Bigarray

type pslg = LAYOUT Mesh_common.pslg
type mesh = LAYOUT Mesh_common.t

(* Write the [pslg] to the channel [fh]. *)
let output_pslg fh (pslg: pslg) area =
  let pt = pslg#point
  and seg = pslg#segment
  and pt_marker = pslg#point_marker
  and seg_marker = pslg#segment_marker in
  let pt_marker =
    if Array1.dim pt_marker > 0 then (fun i -> pt_marker.{i})
    else (fun _i -> 1) in
  let seg_marker =
    if Array1.dim seg_marker > 0 then (fun i -> seg_marker.{i})
    else (fun _i -> 1) in
  (* Save points coordinates *)
  fprintf fh "%i\n" (NCOLS(pt)); (* number of nodes *)
  for i = FST to LASTCOL(pt) do
    (* EasyMesh expects indexes in 0 .. nnodes-1 *)
    fprintf fh "%i: %.13g %.13g %.13g %i\n"
      (OF_IDX(i)) (GET(pt,FST,i)) (GET(pt,SND,i))  area (pt_marker i)
  done;
  (* Save segments *)
  fprintf fh "%i\n" (NCOLS(seg)); (* number of segments *)
  for i = FST to LASTCOL(seg) do
    fprintf fh "%i: %i %i %i\n"
      (OF_IDX(i))  (OF_IDX(GET(seg,FST,i))) (OF_IDX(GET(seg,SND,i)))
      (seg_marker i)
  done

(* FIXME: comments are possible in the files *)
(* FIXME: if a file does not exists, return empty array? *)
(* [read_fortran fname] reads the collection of filenames [fname].n,
   [fname].e and [fname].s and creates a mesh struture with fortran
   layout.  This function can throw a variety of exceptions depending
   of what goes wrong.  *)
let read (pslg: pslg) fname : mesh =
  (* Read nodes *)
  let fh = open_in (fname ^ ".n") in
  let nnodes = int_of_string(input_line fh) in
  let pt = CREATE_MAT(float64, 2, nnodes) in
  let pt_marker = CREATE_VEC(int, nnodes) in
  let sb = Scanning.from_channel fh in
  for _i = FST to LASTCOL(pt) do
    bscanf sb " %i: %g %g %i" (fun i x y m ->
                                 let i = TO_IDX(i) in
                                 GET(pt, FST,i) <- x;
                                 GET(pt, SND,i) <- y;
                                 pt_marker.{i} <- m)
  done;
  close_in fh;
  (* Read triangles *)
  let fh = open_in (fname ^ ".e") in
  let n = int_of_string(input_line fh) in
  let tr = CREATE_MAT(int, 3, n)
  and tr_nbh = CREATE_MAT(int, 3, n) in
  let sb = Scanning.from_channel fh in
  for _i = FST to LASTCOL(tr) do
    bscanf sb " %i: %i %i %i %i %i %i %_i %_i %_i %_f %_f %_i"
      (fun e i j k ei ej ek ->
         let e = TO_IDX(e) in
         GET(tr, 1,e) <- TO_IDX(i);
         GET(tr, 2,e) <- TO_IDX(j);
         GET(tr, 3,e) <- TO_IDX(k);
         GET(tr_nbh, 1,e) <- TO_IDX(ei);
         GET(tr_nbh, 2,e) <- TO_IDX(ej);
         GET(tr_nbh, 3,e) <- TO_IDX(ek);
      )
  done;
  close_in fh;
  try
    (* Read edges, if file exists *)
    let fh = open_in (fname ^ ".s") in
    let n = int_of_string(input_line fh) in
    let edge = CREATE_MAT(int, 2, n)
    and edge_marker = CREATE_VEC(int, n) in
    let sb = Scanning.from_channel fh in
    for _i = FST to LASTCOL(edge) do
      bscanf sb " %i: %i %i %_i %_i %i" (fun s c d m ->
                                           let s = TO_IDX(s) in
                                           GET(edge, 1,s) <- TO_IDX(c);
                                           GET(edge, 2,s) <- TO_IDX(d);
                                           edge_marker.{s} <- m;
                                        )
    done;
    close_in fh;
    (Mesh_common.make_mesh
       ~point: pt
       ~point_marker: pt_marker
       ~triangle: tr
       ~neighbor: tr_nbh
       ~edge: edge
       ~edge_marker: edge_marker
       ~segment: pslg#segment
       ~segment_marker: pslg#segment_marker
       ~hole: pslg#hole
       ~region: pslg#region)
  with Sys_error _ ->
    (Mesh_common.make_mesh
       ~point: pt
       ~point_marker: pt_marker
       ~triangle: tr
       ~neighbor: tr_nbh
       ~edge: (CREATE_MAT(int, 2, 0))
       ~edge_marker: (CREATE_VEC(int, 0))
       ~segment: pslg#segment
       ~segment_marker: pslg#segment_marker
       ~hole: pslg#hole
       ~region: pslg#region)

let empty_pslg : pslg =
  let empty_mat = CREATE_MAT(float64, 2, 0)
  and empty_int_mat = CREATE_MAT(int, 2, 0)
  and empty_vec = CREATE_VEC(int, 0) in
  (object
     method point = empty_mat
     method point_marker = empty_vec
     method segment = empty_int_mat
     method segment_marker = empty_vec
     method hole = empty_mat
     method region = empty_mat
   end)

(* [sort3 a b c] sort [a], [b], [c] from the larger to the smaller *)
let sort3 (a:int) b c =
  if a > b then
    if a <= c then (c, a, b)
    else if b > c then (a, b, c) else (a, c, b)
  else (* a <= b *)
    if b <= c then (c, b, a)
    else if a > c then (b, a, c) else (b, c, a)

let write (mesh: mesh) file =
  (* Compute the information needed by easymesh *)
  let pt = mesh#point in
  let n = Array2.dim2 pt in
  let tr = mesh#triangle in
  let ed = mesh#edge in
  (* For two nodes (i,j), i > j, return the (at most) two triangles
     sharing that edge. *)
  let pt_tr = Hashtbl.create (NCOLS(ed)) in
  for t = FST to LASTCOL(tr) do
    let i1, i2, i3 = (* i1 > i2 > i3 *)
      sort3 (GET(tr, FST, t)) (GET(tr, SND, t)) (GET(tr, THIRD, t)) in
    if i1 = i2 || i1 = i3 || i2 = i3 then
      invalid_arg "Easymesh.write: illegal mesh (triangle corner indices)";
    Hashtbl.add pt_tr (i1,i2) t;
    Hashtbl.add pt_tr (i1,i3) t;
    Hashtbl.add pt_tr (i2,i3) t;
  done;
  (* For two nodes (i,j), i > j, give the index of the edge, if exists. *)
  let pt_ed = Hashtbl.create (NCOLS(ed)) in
  for e = FST to LASTCOL(ed) do
    let i = GET(ed, FST,e) and j = GET(ed, SND,e) in
    if i = j then invalid_arg "Easymesh.write: illegal mesh (egde enpoints)"
    else if i > j then Hashtbl.add pt_ed (i,j) e
    else Hashtbl.add pt_ed (j,i) e
  done;
  (* Write [file].n *)
  let pt_marker = mesh#point_marker in
  let marker =
    if Array1.dim pt_marker = 0 then (fun _ -> 1) else (fun i -> pt_marker.{i}) in
  let fh = open_out(file ^ ".n") in
  fprintf fh "%i\n" n;
  for i = FST to LASTCOL(pt) do
    fprintf fh "%i: %.17g %.17g %i\n" (OF_IDX(i))
      (GET(pt,FST,i)) (GET(pt,SND,i)) (marker i)
  done;
  close_out fh;
  (* Write [file].e *)
  let fh = open_out(file ^ ".e") in
  fprintf fh "%i\n" (NCOLS(tr));
  let other_triangle i12 t = (* assume i12 = (i1, i2) with i1 > i2 *)
    match Hashtbl.find_all pt_tr i12 with
    | [ _ ] -> -1 (* no other triangle *)
    | [t1; t2] -> if t1 = t then OF_IDX(t2) else OF_IDX(t1)
    | _ -> assert false in
  for t = FST to LASTCOL(tr) do
    let i1, i2, i3 = (* i1 > i2 > i3 *)
      sort3 (GET(tr, FST, t)) (GET(tr, SND, t)) (GET(tr, THIRD, t)) in
    let x1 = GET(pt, FST, i1) and y1 = GET(pt, SND, i1)
    and x2 = GET(pt, FST, i2) and y2 = GET(pt, SND, i2)
    and x3 = GET(pt, FST, i3) and y3 = GET(pt, SND, i3) in
    (* Compute the circumcenter.
       See http://www.ics.uci.edu/~eppstein/junkyard/circumcenter.html *)
    let x21 = x2 -. x1 and y21 = y2 -. y1
    and x31 = x3 -. x1 and y31 = y3 -. y1 in
    (* FIXME: compute the determinant more accurately. *)
    let det = 2. *. (x21 *. y31 -. x31 *. y21) in
    let d21 = x21 *. x21 +. y21 *. y21
    and d31 = x31 *. x31 +. y31 *. y31 in
    let mx = x1 -. (y21 *. d31 -. y31 *. d21) /. det
    and my = y1 +. (x21 *. d31 -. x31 *. d21) /. det in
    let i1' = (i2,i3) and i2' = (i1,i3) and i3' = (i1,i2) in
    fprintf fh "%i: %i %i %i %i %i %i %i %i %i %g %g 0\n" (OF_IDX(t))
      (OF_IDX(i1)) (OF_IDX(i2)) (OF_IDX(i3))
      (other_triangle i1' t) (* opposite i1 *)
      (other_triangle i2' t)
      (other_triangle i3' t)
      (OF_IDX(Hashtbl.find pt_ed i1'))
      (OF_IDX(Hashtbl.find pt_ed i2'))
      (OF_IDX(Hashtbl.find pt_ed i3'))
      mx my
  done;
  close_out fh;
  (* Write [file].s *)
  let fh = open_out(file ^ ".s") in
  fprintf fh "%i\n" (NCOLS(ed));
  let ed_marker = mesh#edge_marker in
  let marker =
    if Array1.dim ed_marker = 0 then (fun _ -> 1) else (fun e -> ed_marker.{e}) in
  for e = FST to LASTCOL(ed) do
    let i1 = GET(ed, FST, e) and i2 = GET(ed, SND, e) in
    let i12 = if i1 > i2 then (i1, i2) else (i2, i1) in
    let t1, t2 = match Hashtbl.find_all pt_tr i12 with
      | [t1] -> OF_IDX(t1), -1
      | [t1; t2] -> OF_IDX(t1), OF_IDX(t2)
      | _ -> assert false in
    fprintf fh "%i: %i %i %i %i %i\n" (OF_IDX(e)) (OF_IDX(i1)) (OF_IDX(i2))
      t1 t2 (marker e)
  done;
  close_out fh
