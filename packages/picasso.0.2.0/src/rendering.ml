open Tools
open Apronext
module E = Environmentext

(* Speed *)
let sx = 1000.

and sy = 1000.

let zo = 2.

type color = int * int * int

(* type elem =
 *   { elem: Apol.t
 *   ; col: color
 *   ; is_bounded: bool
 *         (\* to avoid recomputing the projections of a given element on a pair
 *            of variable *\)
 *   ; projs: (string * string, proj) Hashtbl.t }
 *
 * and proj = Apol.t
 *
 * let make_elem elem col =
 *   {elem; col; is_bounded= Apol.is_bounded elem; projs= Hashtbl.create 10}
 *
 * let project elem v1 v2 =
 *   try Hashtbl.find elem.projs (v1, v2)
 *   with Not_found ->
 *     let p2d = Apol.proj2D_s elem.elem v1 v2 in
 *     Hashtbl.add elem.projs (v1, v2) p2d ;
 *     p2d
 *
 * type t =
 *   { window: window_settings
 *   ; scene: scene_settings
 *   ; (\* graphical options *\)
 *     grid: bool
 *   ; axis: bool
 *   ; (\* content *\)
 *     elems: elem list
 *   ; (\* projection variables *\)
 *     abciss: string
 *   ; ordinate: string } *)

type t =
  { window: window_settings
  ; scene: scene_settings
  ; (* graphical options *)
    grid: bool
  ; axis: bool
  ; (* content *)
    elems: (color * Apol.t) list
  ; (* projection variables *)
    abciss: string
  ; ordinate: string
  ; (* elems projected on the projection variables. We differentiate the
       bounded ones from the unbounded ones for efficiency *)
    bounded: (color * Geometry.hull) list
  ; unbounded: (color * Apol.t) list }

and window_settings =
  {padding: float; sx: float; sy: float; title: string option}

(* drawing scenes are bounded *)
and scene_settings = {x_min: float; x_max: float; y_min: float; y_max: float}

let empty_scene =
  {x_min= infinity; x_max= neg_infinity; y_min= infinity; y_max= neg_infinity}

let create ?title ?padding:(pad = 60.) ?grid ?axis ~abciss ~ordinate sx sy =
  { window= {padding= pad; sx; sy; title}
  ; scene= empty_scene
  ; axis= Option.value axis ~default:true
  ; grid= Option.value grid ~default:true
  ; elems= []
  ; abciss
  ; ordinate
  ; bounded= []
  ; unbounded= [] }

let toggle_grid r = {r with grid= not r.grid}

let toggle_axes r = {r with axis= not r.axis}

(* set new bounds for a scene *)
let set_scene s x_min x_max y_min y_max =
  { x_min= min x_min s.x_min
  ; x_max= max x_max s.x_max
  ; y_min= min y_min s.y_min
  ; y_max= max y_max s.y_max }

let translate (x, y) a =
  let x = x /. a.window.sx in
  let y = y /. a.window.sy in
  let lx = (a.scene.x_max -. a.scene.x_min) *. x in
  let ly = (a.scene.y_max -. a.scene.y_min) *. y in
  { a with
    scene=
      { x_min= a.scene.x_min -. lx
      ; x_max= a.scene.x_max -. lx
      ; y_min= a.scene.y_min -. ly
      ; y_max= a.scene.y_max -. ly } }

let scale a alpha =
  let center_x = 0.5 *. (a.scene.x_max +. a.scene.x_min) in
  let center_y = 0.5 *. (a.scene.y_max +. a.scene.y_min) in
  let x_min = center_x +. ((a.scene.x_min -. center_x) *. alpha) in
  let y_min = center_y +. ((a.scene.y_min -. center_y) *. alpha) in
  let x_max = center_x +. ((a.scene.x_max -. center_x) *. alpha) in
  let y_max = center_y +. ((a.scene.y_max -. center_y) *. alpha) in
  {a with scene= {x_min; x_max; y_min; y_max}}

let zoom a = scale a zo

let unzoom a = scale a (1. /. zo)

let change_size_x x a = {a with window= {a.window with sx= x}}

let change_size_y y a = {a with window= {a.window with sy= y}}

let change_size x y a = {a with window= {a.window with sx= x; sy= y}}

let add ?autofit:(auto = true) r ((c, x) : color * Drawable.t) =
  let r =
    {r with elems= List.fold_left (fun acc e -> (c, e) :: acc) r.elems x}
  in
  if auto then
    let i1, i2 = Drawable.bounds r.abciss r.ordinate x in
    let (l1, u1), (l2, u2) = Intervalext.(to_float i1, to_float i2) in
    {r with scene= set_scene r.scene l1 u1 l2 u2}
  else r

let add_l ?autofit:(auto = true) r drawables =
  List.fold_left (add ~autofit:auto) r drawables

(* given a window and a scene, returns a function that maps an abstract
   coordinate to a point of the scene to the window *)
let normalize u =
  let s, w = (u.scene, u.window) in
  let to_coord (min_x, max_x) (min_y, max_y) (a, b) =
    let a = projection (min_x, max_x) (w.padding, w.sx -. w.padding) a
    and b = projection (min_y, max_y) (w.padding, w.sy -. w.padding) b in
    (a, b)
  in
  to_coord (s.x_min, s.x_max) (s.y_min, s.y_max)

(* given a window and a scene, returns a function that maps an
 *     a point of the window to the abstract coordinate of the scene *)
let denormalize u =
  let s, w = (u.scene, u.window) in
  let to_coord (min_x, max_x) (min_y, max_y) (a, b) =
    let a = projection (w.padding, w.sx -. w.padding) (min_x, max_x) a
    and b = projection (w.padding, w.sy -. w.padding) (min_y, max_y) b in
    (a, b)
  in
  to_coord (s.x_min, s.x_max) (s.y_min, s.y_max)

(* convex hull computation *)
let to_vertice r e =
  Apol.to_generator_list e
  |> List.rev_map (fun g ->
         Generatorext.to_vertices2D_s g r.abciss r.ordinate)
  |> Geometry.hull

(* TODO: memoization according to projection variables. changes the
   projection variables. if those are different from the previous ones we: -
   compute the hull for bounded elements - project the unbounded ones on the
   specified variables *)
let set_proj_vars r v1 v2 =
  let bounded, unbounded =
    List.fold_left
      (fun (b, u) (c, pol) ->
        let p2d = Apol.proj2D_s pol v1 v2 in
        if Apol.is_bounded p2d then ((c, to_vertice r p2d) :: b, u)
        else (b, (c, p2d) :: u))
      ([], []) r.elems
  in
  {r with abciss= v1; ordinate= v2; bounded; unbounded}

(* TODO: recompute screen only when the window changes size and when
   projection variables are changed *)
let abstract_screen r =
  let x = r.abciss and y = r.ordinate in
  let scenv = E.make_s [||] [|x; y|] in
  let to_gens (x, y) = Generatorext.of_float_point scenv [x; y] in
  [(0., 0.); (r.window.sx, 0.); (r.window.sx, r.window.sy); (0., r.window.sy)]
  |> List.rev_map (denormalize r)
  |> List.rev_map to_gens |> Apol.of_generator_list

let to_vertices r =
  let norm = normalize r in
  let screen = abstract_screen r in
  let r = set_proj_vars r r.abciss r.ordinate in
  List.fold_left
    (fun acc (c, e) ->
      let interscreen = Apol.meet e screen in
      if Apol.is_bottom interscreen then acc
      else (c, to_vertice r interscreen) :: acc)
    r.bounded r.unbounded
  |> List.rev_map (fun (c, h) -> (c, List.rev_map norm h))
