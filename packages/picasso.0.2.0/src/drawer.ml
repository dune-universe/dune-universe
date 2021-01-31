module Make (D : Backend.T) = struct
  (* Initialization and Backend setting *)

  let x_min = ref 0.

  let x_max = ref 0.

  let y_min = ref 0.

  let y_max = ref 0.

  let square ((a, b) as bl) ((c, d) as tr) = [bl; (c, b); tr; (a, d)]

  (* screen as a polygon *)
  let screen () = square (0., 0.) (D.width (), D.height ())

  let to_backend_coord (x, y) =
    D.normalize (!x_min, !x_max) (!y_min, !y_max) (x, y)

  (* few constants colors *)
  let black = D.rgb 0 0 0

  let white = D.rgb 255 255 255

  let darkgray = D.rgb 64 64 64

  let lightgray = D.rgb 230 230 230

  let gray = D.rgb 128 128 128

  (* redefining drawing utilities *)

  let clear () = D.fill_poly white (screen ())

  let draw_line ~dashed col p1 p2 =
    let p1 = to_backend_coord p1 and p2 = to_backend_coord p2 in
    D.draw_line ~dashed col p1 p2

  let draw_text col pos (text_x, text_y) text =
    let text_x, text_y = to_backend_coord (text_x, text_y) in
    D.draw_text col pos (text_x, text_y) text

  let fill_circle col ((cx, cy) as center) rad =
    let ((px, _) as p) = to_backend_coord center in
    let px', _ = to_backend_coord (cx +. rad, cy) in
    let rad = px' -. px in
    D.fill_circle col p rad

  let poly f col vertices =
    match vertices with
    | [] -> ()
    | [x] -> fill_circle col x 2.
    | [(xa, ya); (xb, yb)] -> draw_line ~dashed:false col (xa, ya) (xb, yb)
    | _ ->
        let vertices = List.rev_map to_backend_coord vertices in
        f col vertices

  let draw_poly = poly D.draw_poly

  let fill_poly = poly D.fill_poly

  (* Filled, black-outlined polygon *)
  let polygon col vertices =
    fill_poly col vertices ; draw_poly black vertices

  let xline r cur =
    let open Rendering in
    let up = r.scene.y_max and down = r.scene.y_min in
    let p1 = normalize r (cur, down) and p2 = normalize r (cur, up) in
    draw_line ~dashed:true lightgray p1 p2

  let yline r cur =
    let open Rendering in
    let left = r.scene.x_min and right = r.scene.x_max in
    let p1 = normalize r (left, cur) and p2 = normalize r (right, cur) in
    draw_line ~dashed:true lightgray p1 p2

  let draw_grid render =
    let open Rendering in
    let sx = render.scene.x_max -. render.scene.x_min in
    let sy = render.scene.y_max -. render.scene.y_min in
    let xl10 = 10. ** (log10 sx -. 1.) in
    let yl10 = 10. ** (log10 sy -. 1.) in
    let min_x = floor (render.scene.x_min /. xl10) *. xl10 in
    let min_y = floor (render.scene.y_min /. yl10) *. yl10 in
    Tools.iterate (xline render) min_x (( +. ) xl10)
      (( < ) render.scene.x_max) ;
    Tools.iterate (yline render) min_y (( +. ) yl10)
      (( < ) render.scene.y_max)

  let graduation rating fx fy render =
    let stepify dim =
      (* TODO:less hardcode *)
      let step = dim /. 10. in
      let exp = log10 step |> ceil in
      let c = step /. (10. ** exp) in
      c *. (10. ** exp) *. rating
    in
    let open Rendering in
    let left = render.scene.x_min and right = render.scene.x_max in
    let down = render.scene.y_min and up = render.scene.y_max in
    let step_w = stepify (right -. left) in
    let step_h = stepify (up -. down) in
    Tools.iterate fx left (( +. ) step_w) (( < ) right) ;
    Tools.iterate fy down (( +. ) step_h) (( < ) up)

  let draw_axes r =
    let open Rendering in
    let pad = 30. in
    let x0, y0 = (0., 0.) in
    let left = r.scene.x_min and right = r.scene.x_max in
    let up = r.scene.y_max and down = r.scene.y_min in
    let hx, hy = normalize r (left, y0)
    and hx', hy' = normalize r (right, y0) in
    let th = 2. in
    let thick_line =
      [(hx, hy +. th); (hx, hy -. th); (hx', hy' -. th); (hx', hy' +. th)]
    in
    fill_poly gray (List.rev_map (fun (x, y) -> (x, y)) thick_line) ;
    let vx, vy = normalize r (x0, down)
    and vx', vy' = normalize r (x0, up) in
    let thick_line =
      [(vx +. th, vy); (vx -. th, vy); (vx' -. th, vy'); (vx' +. th, vy')]
    in
    fill_poly gray (List.rev_map (fun (x, y) -> (x, y)) thick_line) ;
    let minibar_size = 6. in
    let fx cur =
      let x, _ = normalize r (cur, down) in
      draw_line ~dashed:false gray
        (x, hy -. minibar_size)
        (x, hy +. minibar_size)
    in
    let fy cur =
      let _, y = normalize r (left, cur) in
      draw_line ~dashed:false gray
        (vx -. minibar_size, y)
        (vx +. minibar_size, y)
    in
    graduation 0.5 fx fy r ;
    (* vetrtical coordinates *)
    let fx =
      let flag = ref 0 in
      fun cur ->
        let text = Format.asprintf "%a" Tools.pp_float cur in
        let x, _ = normalize r (cur, down) in
        if !flag mod 4 = 0 then draw_text darkgray `Center (x, pad) text ;
        incr flag
    in
    (* hozizontal coordinates *)
    let fy =
      let flag = ref 0 in
      fun cur ->
        let text = Format.asprintf "%a" Tools.pp_float cur in
        let _, y = normalize r (left, cur) in
        if !flag mod 2 = 0 then draw_text darkgray `Center (pad, y) text ;
        incr flag
    in
    graduation 0.5 fx fy r ;
    draw_text black `Center (r.window.sx /. 2., pad /. 2.) r.abciss ;
    draw_text black `Center (pad /. 2., r.window.sy /. 2.) r.ordinate

  (* main drawing function *)
  let draw r =
    let open Rendering in
    x_min := r.scene.x_min ;
    x_max := r.scene.x_max ;
    y_min := r.scene.y_min ;
    y_max := r.scene.y_max ;
    r |> to_vertices
    |> List.iter (fun ((r, g, b), e) -> polygon (D.rgb r g b) e) ;
    if r.grid then draw_grid r ;
    if r.axis then draw_axes r
end
