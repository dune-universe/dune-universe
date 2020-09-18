open Tools

module Draw = Drawer.Make(Gtkcanvas)

class canvas ~packing ~width ~height () =
  let hbox = GPack.hbox ~width ~height ~packing () in
  object (self)
    inherit Clickable.clickable ~width ~height ~packing:hbox#add ()

    val mutable rend = None

    method get_render () =
      match rend with
      | None -> failwith "render not set"
      | Some r -> !r

    method set_render (render:Rendering.t ref) =
      rend <- Some render;
      self#mouse_set
        ~expose:
        (fun (a,b) ->
          render := !render |> Rendering.change_size (float a) (float b);
          self#repaint())
        ~drag:
         (fun (a,b) (x,y) ->
           render := Rendering.translate (x -. a, y -. b) (!render);
           self#repaint ();
         )
        ~scrollwheel:(fun direction ->
          render :=
            (match direction with
             | `DOWN ->  Rendering.zoom (!render)
             | `UP ->  Rendering.unzoom (!render)
             | _ -> !render);
          self#repaint ())
      ()

    (* Repaint the widget. *)
    method repaint () =
      let drawable = self#get_drawable() in
      Gtkcanvas.set_drawable drawable;
      Draw.clear();
      Draw.draw (self#get_render());
      ()
  end

(* constructor *)
let create_canvas ~packing ~height ~width =
  new canvas ~packing ~height ~width ()

(* building the main view *)
let build render =
  let open Rendering in
  let width = render.window.sx |> iof in
  let height = render.window.sy |> iof in
  let title = Option.value render.window.title ~default:"Picasso" in
  let window = GWindow.window ~width ~height ~title () in
  window#connect#destroy ~callback:GMain.Main.quit |> ignore;
  window#event#add ([`ALL_EVENTS]);
  let canvas = create_canvas ~packing:window#add ~height ~width in
  let render = ref render in
  canvas#set_render render;
  window
