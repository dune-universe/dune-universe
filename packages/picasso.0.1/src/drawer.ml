module Make(D:Backend.T) = struct

  (**************************************)
  (* Initialization and Backend setting *)
  (**************************************)

  let width () = D.width () |> float
  let height () = D.height () |> float

  let x_min = ref 0.
  let x_max = ref 0.
  let y_min = ref 0.
  let y_max = ref 0.

  let square ((a,b) as bl) ((c,d) as tr) =
    [bl; c,b; tr; a,d]

  (* screen as a polygon *)
  let screen () = square (0.,0.) (width(),height())

  let to_backend_coord (x,y) =
     D.normalize (!x_min,!x_max) (!y_min,!y_max) (x,y)

  (* few constants colors *)
  let black     = D.rgb 0 0 0
  let white     = D.rgb 255 255 255
  let lightgray = D.rgb 230 230 230
  let gray      = D.rgb 128 128 128

  (********************************)
  (* redefining drawing utilities *)
  (********************************)

  let clear () =
    D.fill_poly white (screen ())

  let draw_line col p1 p2 =
    let p1 = to_backend_coord p1 and p2 = to_backend_coord p2 in
    D.draw_line col p1 p2

  let draw_text col pos (text_x, text_y) text =
    let text_x,text_y = to_backend_coord (text_x, text_y) in
    D.draw_text col pos (text_x, text_y) text

  let fill_circle col (cx,cy as center) rad =
    let (px,_ as p) = to_backend_coord center in
    let (px',_) = to_backend_coord (cx+.rad,cy) in
    let rad = px' -. px in
    D.fill_circle col p rad

  let fill_poly col vertices =
    match vertices with
    | [] -> ()
    | [x] -> fill_circle col x 2.
    | [(xa,ya);(xb,yb)] -> draw_line col (xa,ya) (xb,yb)
    | _ ->
       let vertices = List.rev_map to_backend_coord vertices in
       D.fill_poly col vertices

  let draw_poly col vertices =
    match vertices with
    | [] -> ()
    | [x] ->  fill_circle col x 8.
    | [a;b] -> draw_line col a b
    | _ ->
       let vertices = List.rev_map to_backend_coord vertices in
       D.draw_poly col vertices

   (* Filled, black-outlined polygon *)
  let polygon col vertices =
    fill_poly col vertices;
    draw_poly black vertices

  let graduation rating fx fy render =
    let stepify dim =
      (* TODO:less hardcode *)
      let helper_1_2_5 c =
        if c < 1.5 then 1.
        else if c < 3.5 then 2.
        else 5.
      in
      let step = dim /. 10. in
      let exp = log10 step |> ceil in
      let c = step /. (10. ** exp) in
      let c = helper_1_2_5 c in
      c *. (10.**exp) *. rating
    in
    let open Rendering in
    let left = render.scene.x_min and right = render.scene.x_max in
    let down = render.scene.y_min and up    = render.scene.y_max in
    let step_w = stepify (right -. left) in
    let step_h = stepify (up -. down) in
    let iterate f step min max =
      let rec loop cur =
        if cur < max then begin
            f cur;
            loop (cur +. step)
          end
      in
      loop  ((min /. step |> ceil)*.step)
    in
    iterate fx step_w left right;
    iterate fy step_h down up

  let draw_grid render =
    let open Rendering in
    let left = render.scene.x_min and right = render.scene.x_max in
    let up = render.scene.y_max and down = render.scene.y_min in
    let fx cur =
      let p1 = normalize render (cur, down)
      and p2 = normalize render (cur, up) in
      draw_line lightgray p1 p2;
    in
    let fy cur =
      let p1 = normalize render (left,cur)
      and p2 = normalize render (right,cur) in
      draw_line lightgray p1 p2;
    in
    graduation 1. fx fy render

  let draw_axes render =
    let open Rendering in
    let pad = 30. in
    let x0,y0 = (0.,0.) in
    let left = render.scene.x_min and right = render.scene.x_max in
    let up = render.scene.y_max and down = render.scene.y_min in
    let hx,hy   = Rendering.normalize render (left, y0)
    and hx',hy' = Rendering.normalize render (right, y0) in
    let th = 2. in
    let thick_line = [(hx, hy+.th); (hx, hy-.th); (hx', hy'-.th);(hx', hy'+.th)] in
    fill_poly gray (List.rev_map (fun (x,y) -> (x, y)) thick_line);
    let vx,vy   = Rendering.normalize render (x0, down)
    and vx',vy' = Rendering.normalize render (x0, up) in
    let thick_line = [(vx+.th, vy); (vx-.th, vy); (vx'-.th, vy');(vx'+.th, vy')] in
    fill_poly gray (List.rev_map (fun (x,y) -> (x, y)) thick_line);
    let minibar_size = 6. in
    let fx cur =
      let (x,_) = normalize render (cur, down) in
      draw_line gray (x, hy-.minibar_size) (x, (hy+.minibar_size));
    in
    let fy cur =
      let (_,y) = normalize render (left, cur) in
      draw_line gray (vx-.minibar_size, y) ((vx+.minibar_size), y);
    in
    graduation 0.5 fx fy render;
    (* erasing what is in the margin *)
    let rwp = render.window.padding in
    fill_poly white (square (0.,0.) (width(),rwp));
    fill_poly white (square (0.,0.) (rwp,height()));
    fill_poly white (square (0.,height()) (width(),height()-.rwp));
    fill_poly white (square (width()-.rwp,height()) (width(),0.));
    (* vetrtical coordinates *)
    let fx =
      let flag = ref 0 in
      fun cur ->
      let text = Format.asprintf "%.1f" cur in
      let (x,_) = normalize render (cur, down) in
      if !flag mod 4 = 0 then draw_text gray `Center (x,pad) text;
      incr flag
    in
    (* hozizontal coordinates *)
    let fy =
      let flag = ref 0 in
      fun cur ->
      let text = Format.asprintf "%.1f" cur in
      let (_,y) = normalize render (left, cur) in
      if !flag mod 2 = 0 then draw_text gray `Center (pad, y) text;
      incr flag
    in
    graduation 0.5 fx fy render

  (* main drawing function *)
  let draw render =
    let open Rendering in
    x_min:=render.scene.x_min;
    x_max:=render.scene.x_max;
    y_min:=render.scene.y_min;
    y_max:=render.scene.y_max;
    render |> to_vertices |>
    List.iter (fun ((r,g,b),e) -> polygon (D.rgb r g b) e);
    if render.grid then draw_grid render;
    if render.axis then draw_axes render
end
