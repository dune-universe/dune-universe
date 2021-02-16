open GMain
open GdkKeysyms
open Glexer
open Format

let () = ignore (GtkMain.Main.init ())

let usage () =
  eprintf "usage: gmlpost file.ml fig-name@.";
  exit 1

let ml_file, fig_name =
  if Array.length Sys.argv <> 3 then usage ();
  let f = Sys.argv.(1) in
  if not (Sys.file_exists f && Filename.check_suffix f ".ml") then usage ();
  (f, Sys.argv.(2))

(* run the mlpost file and create PNG image *)

let sys_command cmd =
  let c = Sys.command cmd in
  eprintf "%s@." cmd;
  if c <> 0 then (
    eprintf "command '%s' failed with exit code %d@." cmd c;
    exit 1 )

(* size parameters *)

let xmin = ref (-1.)

let xmax = ref 1.

let ymin = ref (-1.)

let ymax = ref 1.

let dx = ref 2.

let dy = ref 2.

let pic_w = ref 1.

let pic_h = ref 1.

let set_bbox xmi ymi xma yma =
  xmin := xmi;
  xmax := xma;
  ymin := ymi;
  ymax := yma;
  dx := !xmax -. !xmin;
  dy := !ymax -. !ymin

let update_bbox () =
  let file = fig_name ^ ".1" in
  let c = open_in file in
  try
    while true do
      let s = input_line c in
      try
        Scanf.sscanf s "%%%%HiResBoundingBox: %f %f %f %f" (fun a b c d ->
            set_bbox a b c d;
            raise Exit)
      with Scanf.Scan_failure _ -> ()
    done
  with
  | End_of_file ->
      eprintf "warning: could not find the bounding box in %s @." file;
      close_in c
  | Exit -> close_in c

let png_file = fig_name ^ ".png"

let make_png () =
  let dvi_file = fig_name ^ ".dvi" in
  let ps_file = fig_name ^ ".ps" in
  sys_command ("mlpost -ps -ccopt glexer.cmo " ^ ml_file);
  sys_command ("latex " ^ fig_name ^ " > /dev/null");
  sys_command ("dvips -E " ^ dvi_file ^ " > /dev/null");
  sys_command ("convert " ^ ps_file ^ " " ^ png_file ^ " > /dev/null");
  update_bbox ()

let () = make_png ()

let edit_file = Filename.chop_suffix ml_file ".ml" ^ ".edit"

let elements = Glexer.read_file edit_file

let pointstable = Hashtbl.create 17

let rec init_table = function
  | [], _, _ | _, [], _ | _, _, [] | Point _ :: _, [ _ ], _ -> ()
  | Num _ :: r, _ :: sps, ps -> init_table (r, sps, ps)
  | Point (s, (_, d1), (_, d2)) :: r, sp1 :: sp2 :: sps, p :: ps ->
      Hashtbl.add pointstable s ((sp1, d1, sp2, d2), p);
      init_table (r, sps, ps)

let spins = ref []

let points = ref []

let z = ref 1.

let belong x y = x > 0. && x < !pic_w && y > 0. && y < !pic_h

(*------------------------------------------------------------------------*)

let rec go_string fmt = function
  | [], _ | _, [] | Point _ :: _, [ _ ] -> ()
  | Num (s, (_, d)) :: r, sp :: sps ->
      let n = sp#value in
      fprintf fmt "num \"%s\" %f %s \n" s n (string_of_dim d);
      go_string fmt (r, sps)
  | Point (s, (_, d1), (_, d2)) :: r, sp1 :: sp2 :: sps ->
      let n1 = sp1#value in
      let n2 = sp2#value in
      fprintf fmt "point \"%s\" %f %s , %f %s \n" s n1 (string_of_dim d1) n2
        (string_of_dim d2);
      go_string fmt (r, sps)

let write_edit () =
  let f = open_out edit_file in
  let fmt = formatter_of_out_channel f in
  go_string fmt (elements, !spins);
  fprintf fmt "@?";
  close_out f

let bp_of_dim n = function
  | Bp -> n
  | Pt -> n *. 0.99626
  | Cm -> n *. 28.34645
  | Mm -> n *. 2.83464
  | Inch -> n *. 72.

let dim_of_bp n = function
  | Bp -> n
  | Pt -> n /. 0.99626
  | Cm -> n /. 28.34645
  | Mm -> n /. 2.83464
  | Inch -> n /. 72.

let update_points _ ((sp1, d1, sp2, d2), p) =
  let v1 = bp_of_dim sp1#value d1 in
  let v2 = bp_of_dim sp2#value d2 in
  let x = (v1 -. !xmin) *. !pic_w /. !dx in
  let y = (!ymax -. v2) *. !pic_h /. !dy in
  p#set [ `X1 (x -. 3.); `Y1 (y -. 3.); `X2 (x +. 3.); `Y2 (y +. 3.) ];
  ()

let refresh _canvas pic () =
  eprintf
    "@.------------------------------refresh------------------------------@.";
  write_edit ();
  make_png ();
  let pixbuf = GdkPixbuf.from_file png_file in
  pic#set [ `PIXBUF pixbuf ];

  pic_w := float_of_int (GdkPixbuf.get_width pixbuf);
  pic_h := float_of_int (GdkPixbuf.get_height pixbuf);

  (*mise a l'echelle de tous les points
    au cas ou il y aurait un changement de bbox*)
  Hashtbl.iter update_points pointstable;

  eprintf "refresh:@.";
  eprintf "  xmin = %f@." !xmin;
  eprintf "  xmax = %f@." !xmax;
  eprintf "  ymin = %f@." !ymin;
  eprintf "  ymax = %f@." !ymax;
  eprintf "  png  = %f x %f pixels@." !pic_w !pic_h;
  ()

(*------------------------------------------------------------------------*)

let highlight_point item ev =
  ( match ev with
  | `ENTER_NOTIFY _ -> item#set [ `FILL_COLOR "red" ]
  | `LEAVE_NOTIFY ev ->
      let state = GdkEvent.Crossing.state ev in
      if not (Gdk.Convert.test_modifier `BUTTON1 state) then
        item#set [ `FILL_COLOR "black" ]
  | `BUTTON_PRESS ev ->
      let curs = Gdk.Cursor.create `FLEUR in
      item#grab
        [ `POINTER_MOTION; `BUTTON_RELEASE ]
        curs (GdkEvent.Button.time ev)
  | `BUTTON_RELEASE ev -> item#ungrab (GdkEvent.Button.time ev)
  | _ -> () );
  false

let move_point _x _y s item ev =
  ( match ev with
  | `MOTION_NOTIFY ev ->
      let state = GdkEvent.Motion.state ev in
      let x = GdkEvent.Motion.x ev in
      let y = GdkEvent.Motion.y ev in
      if Gdk.Convert.test_modifier `BUTTON1 state && belong x y then (
        let (sp1, d1, sp2, d2), _ = Hashtbl.find pointstable s in
        let x = dim_of_bp x d1 in
        let y = dim_of_bp y d2 in
        sp1#set_value (!xmin +. (x *. !dx /. !pic_w));
        sp2#set_value (!ymax -. (y *. !dy /. !pic_h));
        item#set [ `X1 (x -. 3.); `Y1 (y -. 3.); `X2 (x +. 3.); `Y2 (y +. 3.) ]
        )
  | `BUTTON_RELEASE _ -> ()
  | _ -> () );
  false

let draw_point root s n1 n2 d1 d2 =
  let v1 = bp_of_dim n1 d1 in
  let v2 = bp_of_dim n2 d2 in
  let x = (v1 -. !xmin) *. !pic_w /. !dx in
  let y = (!ymax -. v2) *. !pic_h /. !dy in
  let point =
    GnoCanvas.ellipse ~x1:(x -. 3.) ~x2:(x +. 3.) ~y1:(y -. 3.) ~y2:(y +. 3.)
      ~props:[ `FILL_COLOR "black"; `OUTLINE_COLOR "black"; `WIDTH_PIXELS 0 ]
      root
  in
  let sigs = point#connect in
  ignore (sigs#event ~callback:(highlight_point point));
  ignore (sigs#event ~callback:(move_point x y s point));
  points := point :: !points;
  ()

(*------------------------------------------------------------------------*)

let point_of_spin s sb () =
  let n = sb#value in
  try
    let (sb1, d1, _, d2), p = Hashtbl.find pointstable s in
    if sb == sb1 then
      let v = bp_of_dim n d1 in
      let n = (v -. !xmin) *. !pic_w /. !dx in
      p#set [ `X1 (n -. 3.); `X2 (n +. 3.) ]
    else
      let v = bp_of_dim n d2 in
      let n = (!ymax -. v) *. !pic_h /. !dy in
      p#set [ `Y1 (n -. 3.); `Y2 (n +. 3.) ]
  with Not_found -> ()

let init_spin_bounds = function
  | Bp -> (-150., 150., 0.5)
  | Pt -> (-150., 150., 0.5)
  | Cm -> (-50., 50., 0.1)
  | Mm -> (-500., 500., 1.)
  | Inch -> (-3., 3., 0.05)

let left_part_lign _pic vbox vbox2 vbox3 s n d s' =
  ignore (GMisc.label ~text:(s ^ s') ~packing:vbox#add ());
  ignore (GMisc.label ~text:(string_of_dim d) ~packing:vbox3#add ());
  let sb =
    GEdit.spin_button ~packing:vbox2#add ~digits:2 ~numeric:true ~wrap:true ()
  in
  let lower, upper, step_incr = init_spin_bounds d in
  sb#adjustment#set_bounds ~lower ~upper ~step_incr ();
  sb#set_value n;
  ignore (sb#adjustment#connect#value_changed ~callback:(point_of_spin s sb));
  spins := sb :: !spins;
  ()

let left_part pic vbox vbox2 vbox3 root = function
  | Num (s, (n, d)) -> left_part_lign pic vbox vbox2 vbox3 s n d " :"
  | Point (s, (n1, d1), (n2, d2)) ->
      left_part_lign pic vbox vbox2 vbox3 s n1 d1 " xpart :";
      left_part_lign pic vbox vbox2 vbox3 s n2 d2 " ypart :";
      draw_point root s n1 n2 d1 d2

let zoom canvas zoo =
  z := zoo;
  canvas#set_pixels_per_unit !z

(*------------------------------------------------------------------------*)

(* *)
let main () =
  let window = GWindow.window ~title:"GMLPost" () in
  let vb = GPack.vbox ~spacing:10 ~packing:window#add () in

  (* Menu bar *)
  let menubar = GMenu.menu_bar ~packing:vb#pack () in
  ignore (window#connect#destroy ~callback:write_edit);
  ignore (window#connect#destroy ~callback:Main.quit);

  let hbox = GPack.hbox ~spacing:10 ~packing:vb#add () in
  let scrolled_window =
    GBin.scrolled_window ~width:350 ~border_width:10 ~hpolicy:`AUTOMATIC
      ~packing:hbox#add ()
  in
  let hbox2 =
    GPack.hbox ~spacing:10 ~packing:scrolled_window#add_with_viewport ()
  in

  (* Partie Canvas *)
  let pixbuf = GdkPixbuf.from_file png_file in
  pic_w := float_of_int (GdkPixbuf.get_width pixbuf);
  pic_h := float_of_int (GdkPixbuf.get_height pixbuf);

  let scrolled_canvas =
    GBin.scrolled_window ~width:500 ~height:500 ~border_width:10
      ~hpolicy:`AUTOMATIC ~packing:hbox#add ()
  in
  let canvas =
    GnoCanvas.canvas ~width:(int_of_float !pic_w) ~height:(int_of_float !pic_h)
      ~packing:scrolled_canvas#add_with_viewport ()
  in
  canvas#set_scroll_region ~x1:0. ~y1:0. ~x2:!pic_w ~y2:!pic_h;

  let root = canvas#root in

  let pic = GnoCanvas.pixbuf root ~pixbuf in

  let factory = new GMenu.factory menubar in
  let accel_group = factory#accel_group in
  let file_menu = factory#add_submenu "File" in
  let zoom_menu = factory#add_submenu "Zoom" in

  (* File menu *)
  let factory = new GMenu.factory file_menu ~accel_group in
  ignore (factory#add_item "Refresh" ~key:_r ~callback:(refresh canvas pic));
  let factory = new GMenu.factory file_menu ~accel_group in
  ignore (factory#add_item "Quit" ~key:_Q ~callback:Main.quit);

  (* Zoom *)
  let factory = new GMenu.factory zoom_menu ~accel_group in
  ignore (factory#add_item "50%" ~callback:(fun () -> zoom canvas 0.5));
  ignore (factory#add_item "75%" ~callback:(fun () -> zoom canvas 0.75));
  ignore (factory#add_item "100%" ~callback:(fun () -> zoom canvas 1.));
  ignore (factory#add_item "125%" ~callback:(fun () -> zoom canvas 1.25));
  ignore (factory#add_item "150%" ~callback:(fun () -> zoom canvas 1.5));

  window#add_accel_group accel_group;

  let vbox = GPack.vbox ~spacing:10 ~packing:hbox2#add () in
  let vbox2 = GPack.vbox ~spacing:10 ~packing:hbox2#add () in
  let vbox3 = GPack.vbox ~spacing:10 ~packing:hbox2#add () in

  List.iter (left_part pic vbox vbox2 vbox3 root) elements;
  spins := List.rev !spins;
  points := List.rev !points;

  init_table (elements, !spins, !points);

  window#show ();
  Main.main ()

;;
main ()
