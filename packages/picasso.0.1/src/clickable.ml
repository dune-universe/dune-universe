(* This module is a drawing area wrapper that handles mouse events and expose event *)
open Geometry

type mouse_fun = point -> unit
type mouse_move_fun = point -> point -> unit

class clickable ~packing ~width ~height () =
  (* Create the containing vbox. *)
  let da = GMisc.drawing_area ~width ~height ~packing () in
  let drawable = lazy (new GDraw.drawable da#misc#window) in

  let mc = Gdk.Cursor.create `ARROW in
  let oc = Gdk.Cursor.create `FLEUR in

  object(self)
        initializer
          (ignore (da#event#add ([`BUTTON_PRESS;`BUTTON_RELEASE;`POINTER_MOTION;`SCROLL]);
                   ()))

    val mutable tolerance = 900.

    val mutable old_right = None
    val mutable old_left = None

    method private set_moving_left (x,y) =
      old_left <- Some (x,y)

    method private unset_moving_left () =
      old_left <- None

    method get_drawable () =
      try Lazy.force drawable
      with Gpointer.Null -> failwith "drawable null"

    method private get_coord (x,y) =
      (* in gtk y axis is inversed *)
      let _,b = (self#get_drawable())#size in
      x,((float b) -. y)

    method private expose (f : int*int -> unit)=
      ignore(da#event#connect#expose
        ~callback:(fun _ -> f (self#get_drawable())#size; false))

    method private press f_left =
      ignore (da#event#connect#button_press ~callback:(fun c ->
          let x,y = (GdkEvent.Button.x c),(GdkEvent.Button.y c) in
          let w = da#misc#window in
          (match GdkEvent.Button.button c with
           | 1 ->
              self#set_moving_left(x,y);
              Gdk.Window.set_cursor w oc;
              f_left (self#get_coord (x,y))
           | _ -> ());
          false
         ))

    method private click f_left =
      ignore (da#event#connect#button_press ~callback:(fun c ->
          let x,y = (GdkEvent.Button.x c),(GdkEvent.Button.y c) in
          (match GdkEvent.Button.button c with
           | 1 -> f_left (self#get_coord (x,y))
           | _ -> ());
          false
         ))

    method private release f_left =
      ignore (da#event#connect#button_release ~callback:(fun c ->
           let x,y = (GdkEvent.Button.x c),(GdkEvent.Button.y c) in
           let w = da#misc#window in
           (match GdkEvent.Button.button c with
            | 1 ->
               self#unset_moving_left ();
               Gdk.Window.set_cursor w mc;
               f_left (self#get_coord (x,y))
            | _ -> ()
           );
           false
         ))

    method private drag (f_left : point -> point -> unit) =
      ignore (da#event#connect#motion_notify ~callback:(fun c ->
          let x,y as p1 = GdkEvent.Motion.x c,GdkEvent.Motion.y c in
          (match old_left with
           | None -> ()
           | Some((a,b) as p2) ->
              if Geometry.sq_dist p1 p2 >= tolerance then begin
                  f_left (self#get_coord (a,b)) (self#get_coord (x,y));
                  self#set_moving_left (x,y);
                end
          );
          false
         ))

    method private scrollwheel (fscroll:Gdk.Tags.scroll_direction ->unit) =
      ignore (da#event#connect#scroll ~callback:(fun c ->
           fscroll (GdkEvent.Scroll.direction c);
           false
         ))

    method mouse_set
             ?expose:(exp=((fun _ ->()):int*int->unit))
             ?press:(pl=((fun _ ->()):mouse_fun))
             ?release:(rl=((fun _ ->()):mouse_fun))
             ?click:(cl=((fun _ ->()):mouse_fun))
             ?drag:(dl=((fun _ _ ->()):mouse_move_fun))
             ?scrollwheel:(sw=((fun _ ->()):Gdk.Tags.scroll_direction -> unit))
             ()
      =
      self#expose exp;
      self#press pl;
      self#release rl;
      self#click cl;
      self#drag dl;
      self#scrollwheel sw
end
