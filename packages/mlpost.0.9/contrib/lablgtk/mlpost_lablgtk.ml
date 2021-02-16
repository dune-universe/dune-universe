(*    Lablgtk - Examples                                                  *)

open StdLabels
open Mlpost
module P = Picture

type auto_aspect = width:Num.t -> height:Num.t -> P.t -> Mlpost.Transform.t

let aa_nothing ~width:_ ~height:_ _ = []

let aa_center ~width ~height pic =
  let p = Point.pt (Num.divf width 2., Num.divf height 2.) in
  [ Transform.shifted (Point.sub p (P.ctr pic)) ]

let aa_fit_page ~width ~height pic =
  let swidth = Num.divn width (P.width pic) in
  let sheight = Num.divn height (P.height pic) in
  let scale = Num.minn swidth sheight in
  let t = Transform.scaled scale in
  t :: aa_center ~width ~height (P.transform [ t ] pic)

let aa_fit_width ~width ~height pic =
  let swidth = Num.divn width (P.width pic) in
  let t = Transform.scaled swidth in
  t :: aa_center ~width ~height (P.transform [ t ] pic)

let aa_fit_height ~width ~height pic =
  let sheight = Num.divn height (P.height pic) in
  let t = Transform.scaled sheight in
  t :: aa_center ~width ~height (P.transform [ t ] pic)

class mlpost_pic ?width ?height ?packing ?show () =
  (* Create the drawing area. *)
  let da = GMisc.drawing_area ?width ?height ?packing ?show () in
  let drawable = lazy (new GDraw.drawable da#misc#window) in

  let new_pixmap color width height =
    let drawable = GDraw.pixmap ~width ~height () in
    drawable#set_foreground color;
    drawable
  in

  object (self)
    inherit GObj.widget da#as_widget

    val mutable need_update = true

    (* The mlpost pic. *)
    val mutable pic = Command.nop

    method set_pic t =
      pic <- t;
      need_update <- true

    method pic = pic

    (* For the background color *)
    val mutable background = `WHITE

    method background = background

    method set_background c = background <- c

    (* For the aspect *)
    val mutable auto_aspect = aa_nothing

    method set_auto_aspect x = auto_aspect <- x

    val mutable show_corner = false

    method set_show_corner b = show_corner <- b

    val mutable size = (1, 1)

    method size = size

    val mutable pm = new_pixmap `WHITE 1 1

    val origin = Point.origin

    method private repaint () =
      let drawable = Lazy.force drawable in
      let ((width, height) as ssize) = drawable#size in
      size <- ssize;
      pm <- new_pixmap background width height;
      (* reset the pixmap *)
      pm#rectangle ~x:0 ~y:0 ~width ~height ~filled:true ();
      let w, h = (float_of_int width, float_of_int height) in
      (* *)
      let pic =
        if show_corner then
          let f x = Point.draw ~color:Color.red (Picture.corner x pic) in
          Command.seq
            ( pic
            :: List.map ~f
                 [ `Center; `Northeast; `Southeast; `Northwest; `Southwest ] )
        else pic
      in
      let t = auto_aspect ~width:(Num.pt w) ~height:(Num.pt h) pic in
      let pic = Picture.transform t pic in
      let cr = Cairo_gtk.create pm#pixmap in
      Cairost.emit_cairo cr (w, h) pic;
      need_update <- false

    (* Repaint the widget. *)
    method private expose ev =
      if need_update then self#repaint ();
      let area = GdkEvent.Expose.area ev in
      let gwin = da#misc#window in
      let d = new GDraw.drawable gwin in
      let x = Gdk.Rectangle.x area and y = Gdk.Rectangle.y area in
      let width = Gdk.Rectangle.width area
      and height = Gdk.Rectangle.height area in
      d#put_pixmap ~x ~y ~xsrc:x ~ysrc:y ~width ~height pm#pixmap

    initializer
    ignore
      (da#event#connect#expose ~callback:(fun ev ->
           self#expose ev;
           false));
    ignore
      (da#event#connect#configure ~callback:(fun _ ->
           need_update <- true;
           false))
  end

module Interface = struct
  type interface = {
    window : GWindow.window;
    main_vbox : GPack.box;
    mutable show : bool;
    (* The main window is shown *)
    mutable picda : ((unit -> Command.t) * (mlpost_pic * GWindow.window)) list;
  }

  let new_interface ?width ?height ?title () =
    let window = GWindow.window ?width ?height ?title () in
    let vbox = GPack.vbox ~packing:window#add () in
    let _ = GMenu.menu_bar ~packing:vbox#pack () in
    ignore (window#connect#destroy ~callback:GMain.quit);
    { window; main_vbox = vbox; show = false; picda = [] }

  let remove_pic window pic = window.picda <- List.remove_assq pic window.picda

  let add_pic w ?width ?height ?title ?(show_corner = false)
      ?(auto_aspect = aa_nothing) pic =
    let window = GWindow.window ?width ?height ?title () in
    let mlpost_pic = new mlpost_pic ?width ?height ~packing:window#add () in
    mlpost_pic#set_pic (pic ());
    mlpost_pic#set_auto_aspect auto_aspect;
    mlpost_pic#set_show_corner show_corner;
    w.picda <- (pic, (mlpost_pic, window)) :: w.picda;
    ignore (window#connect#destroy ~callback:(fun () -> remove_pic w pic));
    if w.show then ignore (window#show ())

  let refresh w =
    List.iter
      ~f:(fun (pic, (mlpic, _)) ->
        ( try mlpic#set_pic (pic ())
          with e ->
            Format.eprintf "Error raised inside picure generation@ :@ %s@."
              (Printexc.to_string e) );
        GtkBase.Widget.queue_draw mlpic#as_widget)
      w.picda

  (** Editor window *)
  let create_option w ~packing ?label l =
    ( match label with
    | None -> ()
    | Some text -> ignore (GMisc.label ~text ~packing ()) );
    let menu = GMenu.menu () in
    let optionmenu = GMenu.option_menu ~packing () in
    optionmenu#set_menu menu;
    optionmenu#set_history 3;
    ignore
      (List.fold_left
         ~f:(fun group (s, (c : unit -> unit)) ->
           let c () =
             c ();
             refresh w
           in
           let menuitem =
             GMenu.radio_menu_item ?group ~label:s ~packing:menu#append ()
           in
           ignore (menuitem#connect#toggled ~callback:c);
           Some menuitem#group)
         ~init:None l)

  let create_option w = create_option w ~packing:w.main_vbox#pack

  let create_text w ?label first set =
    ( match label with
    | None -> ()
    | Some text -> ignore (GMisc.label ~text ~packing:w.main_vbox#pack ()) );
    let text = GText.view ~packing:w.main_vbox#pack ~show:true () in
    text#buffer#set_text first;
    ignore
      (text#buffer#connect#changed ~callback:(fun () ->
           set (text#buffer#get_text ());
           refresh w))

  let main w =
    ignore (w.window#show ());
    List.iter ~f:(fun (_, (_, window)) -> ignore (window#show ())) w.picda;
    GMain.main ()
end
