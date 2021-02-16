open Mlpost
module P = Path
module N = Num
open Point

let ( |> ) x f = f x

let red dir distmax distmin =
  let l = dir |> length |> N.minn (N.bp distmax) |> N.maxn (N.bp distmin) in
  scale l (normalize dir)

let little_man ~phead ~plhand ~prhand ~plfoot ~prfoot =
  let downbody = origin in
  let upbody = bpp (0., 1.) in
  let armfac = 0.75 in
  let body = P.pathp [ downbody; upbody ] in
  let dneck = sub phead upbody in
  let neck, head =
    let red = red dneck 0.75 0.25 in
    (add (scale (N.bp (0.5 /. 0.75)) red) upbody, add red upbody)
  in
  let head = P.fullcircle |> P.scale (N.bp 0.5) |> P.shift head in
  let neck = P.pathp [ neck; upbody ] in
  let handbody = scale (N.bp armfac) upbody in
  let pos anchor dir = add anchor (red (sub dir anchor) 1. 0.10) in
  let hand dir =
    let c = P.fullcircle |> P.scale (N.bp 0.1) in
    let p = pos handbody dir in
    (P.shift p c, p)
  in
  let lhand, plhand = hand plhand in
  let rhand, prhand = hand prhand in
  let foot dir =
    let c = P.fullcircle |> P.scale (N.bp 0.1) in
    let p = pos origin dir in
    (P.shift p c, p)
  in
  let lfoot, plfoot = foot plfoot in
  let rfoot, prfoot = foot prfoot in
  let larm = P.pathp [ handbody; plhand ] in
  let rarm = P.pathp [ handbody; prhand ] in
  let lleg = P.pathp [ origin; plfoot ] in
  let rleg = P.pathp [ origin; prfoot ] in
  [ body; head; neck; larm; rarm; lhand; rhand; lfoot; rleg; lleg; rfoot ]

let rot p i = rotate (360. *. i) p

let rot_lit i =
  little_man
    ~phead:(rot (bpp (0., 2.)) i)
    ~plhand:(rot (bpp (-2., 1.)) i)
    ~prhand:(rot (bpp (2., 1.)) i)
    ~plfoot:(rot (bpp (-2., -1.)) i)
    ~prfoot:(rot (bpp (2., -1.)) i)
  |> List.map (P.scale (N.cm 1.))
  |> List.map P.draw |> Command.seq

(*let () = List.iter (fun (s,f) -> Metapost.emit s f)
  ["little_man0", rot_lit 0.;
   "little_man1", rot_lit 0.1;
   "little_man2", rot_lit 0.2;
   "little_man3", rot_lit 0.3;
   "little_man4", rot_lit 0.4;]
*)

let _ = GMain.Main.init ()

let width = ref 400

let height = ref 400

let new_pixmap width height =
  let drawable = GDraw.pixmap ~width ~height () in
  drawable#set_foreground `WHITE;
  drawable#rectangle ~x:0 ~y:0 ~width ~height ~filled:true ();
  drawable

let pm = ref (new_pixmap !width !height)

let need_update = ref true

let init_time = Unix.gettimeofday ()

let fps =
  let nb = ref 0 in
  let time = ref (Unix.time ()) in
  fun () ->
    let time2 = Unix.time () in
    if time2 -. !time > 1. then (
      Format.printf "fps : %i@." !nb;
      nb := 0;
      time := time2 )
    else incr nb

let paint () =
  let cr = Cairo_lablgtk.create !pm#pixmap in
  !pm#rectangle ~x:0 ~y:0 ~width:!width ~height:!height ~filled:true ();
  let w, h = (float_of_int !width, float_of_int !height) in
  let i = Unix.gettimeofday () -. init_time in
  let fig = rot_lit i in
  let fig = Picture.shift (ptp (w /. 2., h /. 2.)) fig in
  fps ();
  Cairost.emit_cairo cr (w, h) fig

let refresh da =
  need_update := true;
  GtkBase.Widget.queue_draw da#as_widget

let grow_pixmap () =
  pm := new_pixmap !width !height;
  need_update := true

(* no need to queue a redraw here, an expose
   event should follow the configure, right ? *)

let config_cb ev =
  let w = GdkEvent.Configure.width ev in
  let h = GdkEvent.Configure.height ev in
  let has_grown = w > !width || h > !height in
  width := w;
  height := h;
  if has_grown then grow_pixmap ();
  true

let expose da x y width height =
  let gwin = da#misc#window in
  let d = new GDraw.drawable gwin in
  d#put_pixmap ~x ~y ~xsrc:x ~ysrc:y ~width ~height !pm#pixmap

let expose_cb da ev =
  let area = GdkEvent.Expose.area ev in
  let module GR = Gdk.Rectangle in
  if !need_update then paint ();
  expose da (GR.x area) (GR.y area) (GR.width area) (GR.height area);
  refresh da;
  true

let button_ev da ev =
  match GdkEvent.get_type ev with
  | `BUTTON_RELEASE ->
      refresh da;
      true
  | _ -> false

let init packing =
  let da = GMisc.drawing_area ~width:!width ~height:!height ~packing () in
  da#misc#set_can_focus true;
  da#event#add [ `KEY_PRESS; `BUTTON_MOTION; `BUTTON_PRESS; `BUTTON_RELEASE ];
  ignore (da#event#connect#expose (expose_cb da));
  ignore (da#event#connect#configure config_cb);
  ignore (da#event#connect#button_release (button_ev da))

let main =
  let w =
    GWindow.window ~title:"Cairo spline demo" ~allow_grow:true
      ~allow_shrink:true ()
  in
  ignore (w#connect#destroy GMain.quit);
  init w#add;
  w#show ();
  GMain.main ()
