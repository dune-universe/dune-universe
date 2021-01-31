open Geometry

(* This module handles the obj file format generation *)
type id = int

type face = id * id * id

(* Vertex printing *)
let print_vertex oc ((x, y, z) : point3d) =
  output_string oc
    ( "v " ^ string_of_float x ^ " " ^ string_of_float y ^ " "
    ^ string_of_float z ^ "\n" )

(* Face printing *)
let print_face oc ((id1, id2, id3) : face) =
  output_string oc
    ( "f " ^ string_of_int id1 ^ " " ^ string_of_int id2 ^ " "
    ^ string_of_int id3 ^ "\n" )

(* vertex id generator *)
let gen_vert_id : unit -> id =
  let v_id = ref 0 in
  fun () -> incr v_id ; !v_id

(* prints a vertex and return the associated ID *)
let print_vertex : out_channel -> point3d -> id =
  (* cache to avoid the multiple printing of the same vertice *)
  let h_v = Hashtbl.create 10000 in
  fun oc v ->
    try Hashtbl.find h_v v
    with Not_found ->
      let id = gen_vert_id () in
      Hashtbl.add h_v v id ; print_vertex oc v ; id

(* declare three vtx and the face between them *)
let triangle oc (p0, p1, p2) =
  let id0 = print_vertex oc p0 in
  let id1 = print_vertex oc p1 in
  let id2 = print_vertex oc p2 in
  print_face oc (id0, id1, id2)

(* Main function of the module. Prints the triangle list *)
let triangles oc = List.iter (triangle oc)

let combine f acc l1 l2 =
  List.fold_left
    (fun acc x -> List.fold_left (fun acc y -> f acc x y) acc l2)
    acc l1

(* builds the list of triangle faces of a polyhedra *)
let polyhedra_to_triangles gens : triangle3D list =
  let pairs l1 l2 = combine (fun acc x y -> (x, y) :: acc) [] l1 l2 in
  let triplets l1 pl =
    combine (fun acc x (y, z) -> (x, y, z) :: acc) [] l1 pl
  in
  match gens with
  | [] -> []
  | [x] -> [(x, x, x)]
  | [x; y] -> [(x, x, y)]
  | _ :: (_ :: _ :: _ as p2 as p1) -> pairs p1 p2 |> triplets gens

let output r file =
  let open Rendering3d in
  let oc = open_out file in
  List.iter (fun e -> triangles oc (polyhedra_to_triangles e)) r.bounded3
