(* Drawing area wrapper that handles events and build the gui *)

open Tools
open Geometry
open GButton

let array_var render =
  let open Apronext.Environmentext in
  let e = Rendering.get_vars render in
  let a = Array.make (size e) "" in
  let i = ref 0 in
  iter
    (fun v ->
      a.(!i) <- Apron.Var.to_string v ;
      incr i )
    e ;
  a

let find_index arr v =
  let exception Found of int in
  try
    Array.iteri (fun i v' -> if v' = v then raise (Found i)) arr ;
    None
  with Found i -> Some i

let next_v vars i j =
  let len = Array.length vars in
  let i = (i + 1) mod len in
  if i = j then vars.((i + 1) mod len) else vars.(i)

let prev_v vars i j =
  let len = Array.length vars in
  let i = (len + i - 1) mod len in
  if i = j then vars.((len + i - 1) mod len) else vars.(i)

let layout ~packing ~size ~orientation nb =
  let cell_size = size / nb in
  match orientation with
  | `V ->
      let box = GPack.vbox ~height:size ~packing () in
      Array.init nb (fun _ ->
          GPack.vbox ~height:cell_size ~packing:box#add () )
  | `H ->
      let box = GPack.hbox ~width:size ~packing () in
      Array.init nb (fun _ ->
          GPack.hbox ~width:cell_size ~packing:box#add () )

class toolbar ~width ~height ~hpack ~vpack () =
  (* horizontal toolbar *)
  let tb_h = layout ~packing:hpack ~size:(width - 40) ~orientation:`H 6 in
  let prev_var_h = button ~packing:tb_h.(0)#add () in
  let _ = GMisc.label ~packing:tb_h.(2)#add ~text:"X axis: " () in
  let cur_h = GMisc.label ~packing:tb_h.(3)#add () in
  let next_var_h = button ~packing:tb_h.(5)#add () in
  (* vertical toolbar *)
  let v0 = GPack.vbox ~height ~packing:vpack () in
  let tb_v = layout ~packing:v0#add ~size:(height - 40) 6 ~orientation:`V in
  let _lab = GMisc.label ~packing:v0#add () in
  let prev_var_v = button ~packing:tb_v.(0)#add () in
  let _ = GMisc.label ~packing:tb_v.(2)#add ~text:"\tY\n axis: " () in
  let cur_v = GMisc.label ~packing:tb_v.(3)#add () in
  let next_var_v = button ~packing:tb_v.(5)#add () in
  object (self)
    val mutable abc = 0

    val mutable ord = 0

    val mutable vars = [||]

    val mutable refresh = fun () -> ()

    method set_vars () =
      next_var_h#set_label (next_v vars abc ord) ;
      prev_var_h#set_label (prev_v vars abc ord) ;
      cur_h#set_label vars.(abc) ;
      next_var_v#set_label (next_v vars ord abc) ;
      prev_var_v#set_label (prev_v vars ord abc) ;
      cur_v#set_label vars.(ord)

    method init (render : Rendering.t ref) refresh =
      vars <- array_var !render ;
      abc <- find_index vars !render.abciss |> Option.get ;
      ord <- find_index vars !render.ordinate |> Option.get ;
      self#set_vars () ;
      let len = Array.length vars in
      let update () =
        render := Rendering.set_proj_vars !render vars.(abc) vars.(ord) ;
        self#set_vars () ;
        refresh ()
      in
      ignore
        (prev_var_h#connect#clicked ~callback:(fun () ->
             abc <- (len + abc - 1) mod len ;
             if abc = ord then abc <- (len + abc - 1) mod len ;
             update () ) ) ;
      ignore
        (next_var_h#connect#clicked ~callback:(fun () ->
             abc <- (abc + 1) mod len ;
             if abc = ord then abc <- (abc + 1) mod len ;
             update () ) ) ;
      ignore
        (prev_var_v#connect#clicked ~callback:(fun () ->
             ord <- (len + ord - 1) mod len ;
             if abc = ord then ord <- (len + ord - 1) mod len ;
             update () ) ) ;
      ignore
        (next_var_v#connect#clicked ~callback:(fun () ->
             ord <- (ord + 1) mod len ;
             if abc = ord then ord <- (ord + 1) mod len ;
             update () ) ) ;
      ()
  end

class clickable ~packing ~width ~height () =
  (* Create the drawing_area. *)
  let da = GMisc.drawing_area ~width ~height ~packing () in
  let drawable = lazy (new GDraw.drawable da#misc#window) in
  let mc = Gdk.Cursor.create `ARROW in
  let oc = Gdk.Cursor.create `FLEUR in
  object (self)
    initializer
    ignore
      ( da#event#add
          [`BUTTON_PRESS; `BUTTON_RELEASE; `POINTER_MOTION; `SCROLL] ;
        () )

    val mutable tolerance = 900.

    val mutable old : point option = None

    method private set_moving (x, y) = old <- Some (x, y)

    method private unset_moving () = old <- None

    method private cb f c =
      let p = GdkEvent.Button.(x c, y c) in
      ( match GdkEvent.Button.button c with
      | 1 -> f (self#get_coord p)
      | _ -> () ) ;
      false

    method get_drawable () =
      try Lazy.force drawable
      with Gpointer.Null -> failwith "drawable null"

    method private get_coord (x, y) =
      (* in gtk y axis is inversed *)
      let _, b = (self#get_drawable ())#size in
      (x, float b -. y)

    method private expose (f : int * int -> unit) =
      ignore
        (da#event#connect#expose ~callback:(fun _ ->
             f (self#get_drawable ())#size ;
             false ) )

    method private press f =
      ignore
        (da#event#connect#button_press
           ~callback:
             (self#cb (fun p ->
                  self#set_moving p ;
                  Gdk.Window.set_cursor da#misc#window oc ;
                  f p ) ) )

    method private click f =
      ignore (da#event#connect#button_press ~callback:(self#cb f))

    method private release f =
      ignore
        (da#event#connect#button_release
           ~callback:
             (self#cb (fun p ->
                  self#unset_moving () ;
                  Gdk.Window.set_cursor da#misc#window mc ;
                  f p ) ) )

    method private drag (f : point -> point -> unit) =
      ignore
        (da#event#connect#motion_notify ~callback:(fun c ->
             let ((x, y) as p1) =
               (GdkEvent.Motion.x c, GdkEvent.Motion.y c)
             in
             Option.iter
               (fun ((a, b) as p2) ->
                 if sq_dist p1 p2 >= tolerance then (
                   f (self#get_coord (a, b)) (self#get_coord (x, y)) ;
                   self#set_moving (x, y) ) )
               old ;
             false ) )

    method private scrollwheel (fscroll : Gdk.Tags.scroll_direction -> unit)
        =
      ignore
        (da#event#connect#scroll ~callback:(fun c ->
             fscroll (GdkEvent.Scroll.direction c) ;
             false ) )

    method mouse_set ?expose:(exp = (fun _ -> () : int * int -> unit))
        ?press:(pl = fun _ -> ()) ?release:(rl = fun _ -> ())
        ?click:(cl = fun _ -> ()) ?drag:(dl = fun _ _ -> ())
        ?scrollwheel:(sw = fun _ -> ()) () =
      self#expose exp ;
      self#press pl ;
      self#release rl ;
      self#click cl ;
      self#drag dl ;
      self#scrollwheel sw
  end

module Gtkcanvas = struct
  type internal = GDraw.drawable

  let drawable = ref (None : GDraw.drawable option)

  let set_drawable d = drawable := Some d

  let get_drawable () =
    try Option.get !drawable
    with Invalid_argument _ ->
      failwith "drawable should be set before canvas operations"

  let ending () = ()

  let width () =
    let d = get_drawable () in
    fst d#size |> float

  let height () =
    let d = get_drawable () in
    snd d#size |> float

  let normalize _ _ (x, y) = (x, height () -. y)

  type color = GDraw.color

  let rgb r g b : color = `RGB (256 * r, 256 * g, 256 * b)

  let descr = Pango.Font.from_string "fixed"

  (* Draw text left, centre or right justified at point. (x,y) point is *
     either top left, top middle or top right of text. *)
  let draw_text col position p text : unit =
    let x, y = to_int_point p in
    let drawable = get_drawable () in
    drawable#set_foreground col ;
    let context = Gdk.Screen.get_pango_context () in
    let layout = Pango.Layout.create context in
    Pango.Layout.set_font_description layout descr ;
    Pango.Layout.set_text layout text ;
    let width = Pango.Layout.get_width layout in
    let fore, back = (None, None) in
    match position with
    | `Left -> drawable#put_layout ~x ~y ?fore ?back layout
    | `Center ->
        drawable#put_layout ~x:(x - (width / 2)) ~y ?fore ?back layout
    | `Right -> drawable#put_layout ~x:(x - width) ~y ?fore ?back layout

  let draw_line ~dashed col a b =
    let ax, ay = to_int_point a in
    let bx, by = to_int_point b in
    let drawable = get_drawable () in
    drawable#set_foreground col ;
    if dashed then drawable#set_line_attributes ~style:`ON_OFF_DASH ()
    else drawable#set_line_attributes ~style:`SOLID () ;
    drawable#line ~x:ax ~y:ay ~x:bx ~y:by

  let circle fill col (x, y) rad =
    let x, y = to_int_point (x -. (rad /. 2.), y -. (rad /. 2.)) in
    let rad = int_of_float rad in
    let drawable = get_drawable () in
    drawable#set_foreground col ;
    drawable#arc ~x ~y ~height:rad ~width:rad ~filled:fill ~angle:360. ()

  let fill_circle = circle true

  let draw_circle = circle false

  (* polygons *)
  let poly fill col vertices =
    let vertices = List.rev_map to_int_point vertices in
    let drawable = get_drawable () in
    drawable#set_foreground col ;
    drawable#polygon vertices ~filled:fill

  let fill_poly = poly true

  let draw_poly = poly false
end

module Draw = Drawer.Make (Gtkcanvas)

class canvas ~packing ~width ~height () =
  let hbox = GPack.hbox ~width ~height ~packing () in
  object (self)
    inherit clickable ~width ~height ~packing:hbox#add ()

    val mutable rend = None

    method get_render () =
      match rend with None -> failwith "render not set" | Some r -> !r

    method set_render (render : Rendering.t ref) =
      rend <- Some render ;
      self#mouse_set
        ~expose:(fun (a, b) ->
          render := !render |> Rendering.change_size (float a) (float b) ;
          self#repaint () )
        ~drag:(fun (a, b) (x, y) ->
          render := Rendering.translate (x -. a, y -. b) !render ;
          self#repaint () )
        ~scrollwheel:(fun direction ->
          (render :=
             match direction with
             | `DOWN -> Rendering.zoom !render
             | `UP -> Rendering.unzoom !render
             | _ -> !render ) ;
          self#repaint () )
        ()

    (* Repaint the widget. *)
    method repaint () =
      let drawable = self#get_drawable () in
      Gtkcanvas.set_drawable drawable ;
      Draw.clear () ;
      Draw.draw (self#get_render ()) ;
      ()
  end

(* building the main view *)
let build render =
  let open Rendering in
  GtkMain.Main.init () |> ignore ;
  let width = render.window.sx |> iof in
  let height = render.window.sy |> iof in
  let title = Option.value render.window.title ~default:"Picasso" in
  let window = GWindow.window ~width ~height ~title () in
  window#connect#destroy ~callback:GMain.Main.quit |> ignore ;
  window#event#add [`ALL_EVENTS] ;
  let hbox = GPack.hbox ~width ~packing:window#add () in
  let vbox = GPack.vbox ~width:(width - 40) ~packing:hbox#add () in
  let c = new canvas ~packing:vbox#add ~height:(height - 30) ~width () in
  let tb = new toolbar ~height ~width ~hpack:vbox#add ~vpack:hbox#add () in
  let render = ref render in
  c#set_render render ;
  tb#init render (fun () -> c#repaint ()) ;
  window#show () ;
  GMain.Main.main ()
