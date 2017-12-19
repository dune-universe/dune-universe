(* Binding to Triangle for layout LAYOUT. *)

open Printf
open Bigarray
open Mesh_triangle_common

type layout = Bigarray.LAYOUT
type mesh = layout t
type mat = layout Mesh.mat
type vec = layout Mesh.vec
type int_mat = layout Mesh.int_mat
type int_vec = layout Mesh.int_vec

let layout = Bigarray.LAYOUT
let default_switches = "DEFAULT_SWITCHES"

let empty_vec = Array1.create int layout 0
let empty_mat0 = CREATE_MAT(float64, 0, 0)
let empty_mat2 = CREATE_MAT(float64, 2, 0)
let empty_mat4 = CREATE_MAT(float64, 4, 0)
let empty_int_mat2 = CREATE_MAT(int, 2, 0)
let empty_int_mat3 = CREATE_MAT(int, 3, 0)

let check_point name point =
  if NCOLS(point) = 0 then
    invalid_arg(name ^ ": points cannot be empty");
  if NROWS(point) <> 2 then
    invalid_arg(name ^ ": ROWS points must be 2")

let check_point_marker name ~point = function
  | None -> empty_vec
  | Some m ->
     let n = Array1.dim m in
     if 0 < n && n < NCOLS(point) then
       invalid_arg(name ^ ": point_marker too small");
     m

let check_point_attribute name ~point = function
  | None -> empty_mat0
  | Some a ->
     if NROWS(a) > 0 && NCOLS(a) <> NCOLS(point) then
       invalid_arg(name ^ ": COLS point_attribute <> COLS point");
     a

let check_segment_marker name ~segment = function
  | None -> empty_vec
  | Some m ->
     let n = Array1.dim m in
     if 0 < n && n < NCOLS(segment) then
       invalid_arg(name ^ ": segment_marker too small");
     m

let check_hole name = function
  | None -> empty_mat2
  | Some h ->
     if NCOLS(h) > 0 && NROWS(h) <> 2 then
       invalid_arg(name ^ ": ROWS hole must be 2");
     h

let check_region name = function
  | None -> empty_mat4
  | Some r ->
     if NCOLS(r) > 0 && NROWS(r) <> 4 then
       invalid_arg(name ^ ": ROWS region must be 4");
     r

let pslg ~hole ~region ~point_attribute ~point_marker ~point
         ~segment_marker ~segment =
  check_point "Mesh_triangle.pslg" point;
  let point_marker =
    check_point_marker "Mesh_triangle.pslg" ~point point_marker in
  let point_attribute =
    check_point_attribute "Mesh_triangle.pslg" ~point point_attribute in
  let segment_marker =
    check_segment_marker "Mesh_triangle.pslg" ~segment segment_marker in
  let hole = check_hole "Mesh_triangle.pslg" hole in
  let region = check_region "Mesh_triangle.pslg" region in
  (object
      method point = point
      method point_marker = point_marker
      method point_attribute = point_attribute
      method segment = segment
      method segment_marker = segment_marker
      method hole = hole
      method region = region
    end : LAYOUT pslg)

let create ~hole ~region ~point_attribute ~point_marker ~point
      ~segment_marker ~segment ~neighbor ~edge ~edge_marker
      ~triangle_attribute ~triangle =
  check_point "Mesh_triangle.create" point;
  let point_marker =
    check_point_marker "Mesh_triangle.create" ~point point_marker in
  let point_attribute =
    check_point_attribute "Mesh_triangle.pslg" ~point point_attribute in
  let segment = match segment with
    | None -> empty_int_mat2
    | Some s ->
       if NCOLS(s) > 0 && NROWS(s) <> 2 then
         invalid_arg "Mesh_triangle.create: ROWS segment must be 2";
       s in
  let segment_marker =
    check_segment_marker "Mesh_triangle.create" ~segment segment_marker in
  let hole = check_hole "Mesh_triangle.create" hole in
  let region = check_region "Mesh_triangle.create" region in
  if NCOLS(triangle) = 0 then
    invalid_arg "Mesh_triangle.create: triangle cannot be empty";
  if NROWS(triangle) < 3 then
    invalid_arg "Mesh_triangle.create: ROWS triangle must be at least 3";
  let triangle_attribute = match triangle_attribute with
    | None -> empty_mat0
    | Some a ->
       if NROWS(a) > 0 && NCOLS(a) > 0 && NCOLS(a) <> NCOLS(triangle) then
         invalid_arg "Mesh_triangle.create: COLS triangle_attribute <> \
                      COLS triangle";
       a in
  let neighbor = match neighbor with
    | None -> empty_int_mat3
    | Some nbh ->
       if NCOLS(nbh) > 0 then (
         if NCOLS(nbh) <> NCOLS(triangle) then
           invalid_arg "Mesh_triangle.create: COLS neighbor <> COLS triangle";
         if NROWS(nbh) <> 3 then
           invalid_arg "Mesh_triangle.create: ROWS neighbor <> 3";
       );
       nbh in
  let edge = match edge with
    | None -> empty_int_mat2
    | Some e ->
       if NCOLS(e) > 0 && NROWS(e) <> 2 then
         invalid_arg "Mesh_triangle.create: ROWS edge <> 2";
       e in
  let edge_marker = match edge_marker with
    | None -> empty_vec
    | Some e ->
       if Array1.dim e > 0 && Array1.dim e <> NCOLS(edge) then
         invalid_arg "Mesh_triangle.create: COLS edge_marker <> COLS edge";
       e in
  (object
     method point = point
     method point_marker = point_marker
     method point_attribute = point_attribute
     method segment = segment
     method segment_marker = segment_marker
     method hole = hole
     method region = region
     method triangle_attribute = triangle_attribute
     method triangle = triangle
     method neighbor = neighbor
     method edge = edge
     method edge_marker = edge_marker
   end : LAYOUT t)

external triangle :
  string ->                        (* options *)
  layout t ->
  vec                             (* trianglearea *)
  -> mat * mat * int_vec * int_mat * mat * int_mat * int_mat * int_vec
    * (* edge *) int_mat * int_vec
    * (* voronoi *) mat * mat * int_mat * mat
  = "triangulate_LAYOUT"


let empty_vec = Array1.create float64 layout 0 (* not used => global *)

(* check that all C "triexit" have been avoided. *)

let triangulate ?(delaunay=true) ?min_angle ?max_area ?(region_area=false)
    ?max_steiner ?(voronoi=false) ?(edge=true) ?(neighbor=false)
    ?(subparam=false) ?triangle_area ?triunsuitable
    ?(check_finite=true) ?(debug=true) ?verbose
    ~pslg ~refine (mesh: layout t) =
  (* Check points *)
  let point = mesh#point in
  if NROWS(point) <> 2 then invalid_arg("ROWS mesh#point <> 2");
  if NCOLS(mesh#point_attribute) > 0
    && NCOLS(mesh#point_attribute) < NCOLS(point) then
    invalid_arg("COLS mesh#point_attribute < COLS mesh#point");
  if Array1.dim mesh#point_marker > 0
    && Array1.dim mesh#point_marker < NCOLS(point) then
    invalid_arg("dim mesh#point_marker < COLS mesh#point");
  if check_finite then (
    (* Check that no point contains NaN (or infinities).  Triangle
       seems to go into an infinite loop with these which can easily
       be confused with other difficulties. *)
    for i = FST to NCOLS(point) do
      if not(is_finite(GET(point, FST, i))) then
        invalid_arg(sprintf "mesh#point.{%i, %i} is not finite"
                            LINE_COL(FST, i));
      if not(is_finite(GET(point, SND, i))) then
        invalid_arg(sprintf "mesh#point.{%i, %i} is not finite"
                            LINE_COL(SND, i));
    done;
  );
  let switches = Buffer.create 20 in
  Buffer.add_string switches default_switches;
  (* Check for PSLG *)
  if pslg then (
    if NCOLS(mesh#segment) > 0 then begin
      if NROWS(mesh#segment) <> 2 then invalid_arg("ROWS segment <> 2");
      if Array1.dim mesh#segment_marker > 0
        && Array1.dim mesh#segment_marker < NCOLS(mesh#segment) then
        invalid_arg("dim mesh#segment_marker < COLS mesh#segment");
    end;
    if not refine then (
      let hole = mesh#hole in
      if NCOLS(hole) > 0 && NROWS(hole) <> 2 then
        invalid_arg("ROWS hole <> 2");
      let region = mesh#region in
      if NCOLS(region) > 0 then (
        if NROWS(region) <> 4 then invalid_arg("ROWS region <> 4");
        Buffer.add_char switches 'A'; (* regional attributes *)
        if region_area then Buffer.add_char switches 'a'; (* area constraint *)
      );
      if check_finite then (
        for i = FST to NCOLS(hole) do
          if not(is_finite(GET(hole, FST, i))) then
            invalid_arg(sprintf "mesh#hole.{%i, %i} is not finite"
                                LINE_COL(FST, i));
          if not(is_finite(GET(hole, SND, i))) then
            invalid_arg(sprintf "mesh#hole.{%i, %i} is not finite"
                                LINE_COL(SND, i));
        done;
        for i = FST to NCOLS(region) do
          for j = FST to NROWS(region) do
            if not(is_finite(GET(region, j, i))) then
              invalid_arg(sprintf "mesh#region.{%i, %i} is not finite"
                                  LINE_COL(j, i));
          done
        done
      )
    );
    Buffer.add_char switches 'p';
    if NROWS(mesh#segment) = 0 || NCOLS(mesh#segment) = 0 then
      Buffer.add_char switches 'c';
  );
  (* Check for refinement -- triangles *)
  if refine then (
    if NCOLS(mesh#triangle) > 0 then begin
      if NROWS(mesh#triangle) < 3 then
        invalid_arg("ROWS mesh#triangle < 3");
      if NROWS(mesh#triangle_attribute) > 0
        && NCOLS(mesh#triangle_attribute) < NCOLS(mesh#triangle) then
        invalid_arg("COLS mesh#triangle_attribute < COLS mesh#triangle");
    end;
    Buffer.add_char switches 'r';
    (* Check triangle_area *)
    (match triangle_area with
     | Some a ->
        if Array1.dim a < NCOLS(mesh#triangle) then
          invalid_arg("dim triangle_area < COLS mesh#triangle");
        Buffer.add_char switches 'a';
     | None -> ());
  );
  (* Area constraints *)
  (match max_area with
   | None -> ()
   | Some a -> bprintf switches "a%f" a);
  let triangle_area = match triangle_area with
    | None -> empty_vec
    | Some a -> a (* for refinement only *) in
  (* Check for a triunsuitable function *)
  (match triunsuitable with
  | None -> ()
  | Some f -> register_triunsuitable f;  Buffer.add_char switches 'u');
  (* Other switches *)
  if delaunay then Buffer.add_char switches 'D';
  (match min_angle with
  | None -> ()
  | Some a ->
    if a < 0. || a > 60. then (* required: 3 min_algle <= 180 *)
      Buffer.add_char switches 'q'
    else
      (* Angle may include a decimal point, but not exponential notation. *)
      bprintf switches "d%f" a);
  (match max_steiner with
   | None -> ()
   | Some a -> bprintf switches "S%i" a);
  if voronoi then Buffer.add_char switches 'v';
  if edge then Buffer.add_char switches 'e';
  if neighbor then Buffer.add_char switches 'n';
  if subparam then Buffer.add_string switches "o2";
  if not debug then Buffer.add_char switches 'Q';
  (match verbose with
   | Some `V -> Buffer.add_string switches "V";
   | Some `VV -> Buffer.add_string switches "V";
   | Some `VVV -> Buffer.add_string switches "VVV";
   | None -> ());
  (* Call triangle and build the resulting objects *)
  let point, point_attribute, point_marker, triangle, triangle_attribute,
      neighbor, segment, segment_marker, edge, edge_marker,
      vor_point, vor_point_attribute, vor_edge, vor_normal =
    triangle (Buffer.contents switches) mesh triangle_area in
  let mesh_out : layout t =
    (make_mesh
      ~point:              point
      ~point_attribute:    point_attribute
      ~point_marker:       point_marker
      ~triangle:           triangle
      ~triangle_attribute: triangle_attribute
      ~neighbor:           neighbor
      ~segment:            segment
      ~segment_marker:     segment_marker
      ~edge:               edge
      ~edge_marker:        edge_marker
      ~hole: mesh#hole
      ~region: mesh#region)
  and vor : layout voronoi =
    (object
      method point               = vor_point
      method point_attribute     = vor_point_attribute
      method edge                = vor_edge
      method normal              = vor_normal
     end) in
  (mesh_out, vor)


(* Sub
 ***********************************************************************)

let sub (mesh: mesh) ?(pos=FST) len =
  let m, n_tr, cols_tr = Mesh__MeshFC.internal_sub (mesh :> Mesh__MeshFC.mesh)
                                             ~pos len in
  let point_attribute =
    if NROWS(mesh#point_attribute) = 0 || NCOLS(mesh#point_attribute) = 0 then
      mesh#point_attribute
    else
      Array2.CHOOSE_FC(sub_right, sub_left) mesh#point_attribute pos len in
  let triangle_attribute =
    let old_att = mesh#triangle_attribute in
    if NROWS(old_att) = 0 || NCOLS(old_att) = 0 then old_att
    else (
      let att = CREATE_MAT(float64, NROWS(old_att), n_tr) in
      Mesh__MeshFC.iteri (fun i pi ->
                    for j = FST to LASTROW(att) do
                      GET(att, j, i) <- GET(old_att, j, pi);
                    done
                   ) cols_tr;
      att
    ) in
  extend_mesh m
              ~point_attribute: point_attribute
              ~triangle_attribute: triangle_attribute


(* Permutations
 ***********************************************************************)

let permute_points_name = "Mesh_triangle.permute_points"

let do_permute_points (old_mesh: mesh) (perm: int_vec) inv_perm : mesh =
  let mesh = Mesh__MeshFC.do_permute_points permute_points_name
                                      (old_mesh :> Mesh__MeshFC.mesh)
                                      perm inv_perm in
  (* Permute the attributes *)
  let old_attr : mat = old_mesh#point_attribute in
  let attr = CREATE_MAT(float64, NROWS(old_attr), NCOLS(old_attr)) in
  for i = FST to LASTCOL(old_attr) do
    let old_i = perm.{i} in
    for a = FST to LASTROW(old_attr) do
      GET(attr, a, i) <- GET(old_attr, a, old_i)
    done
  done;
  extend_mesh mesh
              ~point_attribute: attr
              ~triangle_attribute: old_mesh#triangle_attribute


let permute_points (mesh: mesh) ~inv (perm: int_vec) =
  let inv_perm = Mesh__MeshFC.inverse_perm permute_points_name perm in
  if inv then do_permute_points mesh inv_perm perm
  else do_permute_points mesh perm inv_perm


let permute_triangles_name = "Mesh_triangle.permute_triangles"

let do_permute_triangles (old_mesh: mesh) (perm: int_vec) : mesh =
  let mesh = Mesh__MeshFC.do_permute_triangles permute_triangles_name
                                         (old_mesh :> Mesh__MeshFC.mesh) perm in
  (* Permute attributes *)
  let old_attr : mat = old_mesh#triangle_attribute in
  let attr = CREATE_MAT(float64, NROWS(old_attr), NCOLS(old_attr)) in
  for i = FST to LASTCOL(old_attr) do
    let old_i = perm.{i} in
    for a = FST to LASTROW(old_attr) do
      GET(attr, a, i) <- GET(old_attr, a, old_i)
    done
  done;
  extend_mesh mesh
              ~point_attribute: (old_mesh#point_attribute)
              ~triangle_attribute: attr

let permute_triangles (mesh: mesh) ~inv (perm: int_vec) =
  let inv_perm = Mesh__MeshFC.inverse_perm permute_triangles_name perm in
  if inv then do_permute_triangles mesh inv_perm
  else do_permute_triangles mesh perm
