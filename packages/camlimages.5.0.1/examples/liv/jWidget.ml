open GMain

class status_bar ?packing ?show () =
  let hbox = GPack.hbox ?packing ?show () in

  let label = 
    let f = GBin.frame ~shadow_type: `IN 
              ~packing: (hbox#pack ~expand: true ~fill: true) () in
    let hbox = GPack.hbox ~packing: f#add () in
    (* add some space on the left side of the frame... *)
    let _ = GMisc.label ~text: " " ~packing: (hbox#pack ~expand: false) () in
    let l = GMisc.label ~packing: (hbox#pack ~expand: false ~fill: true) () in
    l
  in
  let progress =
(*
    let f = GBin.frame ~width: 120 ~border_width: 0 ~shadow_type: `IN
	~packing: (hbox#pack ~expand: false ~fill: false) () in
*)
    let f = GBin.event_box ~width: 120 
	~packing: (hbox#pack ~expand: false ~fill: false) () in
    let p = GRange.progress_bar ~packing: f#add () in
(*
    p#set_show_text true;
    p#set_format_string "";
*)
    p
  in

  object 
  inherit GObj.widget hbox#as_widget

  method set_text = label#set_text
  method text = label#text
  method set_justify = label#set_justify
  method set_pattern = label#set_pattern
  method set_line_wrap = label#set_line_wrap

(*
  method adjustment = progress#adjustment
  method configure = progress#configure
  method percentage = progress#percentage
  method set_adjustment = progress#set_adjustment
*)
  method get_fraction = progress#fraction

  method event = progress#event
(*
  method set_activity_blocks = progress#set_activity_blocks
  method set_activity_step = progress#set_activity_step
  method set_bar_style = progress#set_bar_style
  method set_discrete_blocks = progress#set_discrete_blocks
*)
  method set_orientation = progress#set_orientation

(*
  val mutable activity_mode = false
*)
      
  method set_fraction p = 
(*
    if activity_mode then begin
      progress#set_activity_mode false;
      activity_mode <- false
    end;
*)
    progress#set_fraction p

  method activate () =
(*
    if not activity_mode then begin
      progress#set_activity_mode true;
      activity_mode <- true
    end;
*)
    let p = 
      let p = progress#fraction +. 0.01 in
      if p > 1.0 then 0.0 else p
    in
    progress#set_fraction p
end

let status_bar ?packing ?show () = new status_bar ?packing ?show ()

class img_button ?label ?frames ?packing ?show () =
  let button = GButton.button (*?border_width ?width ?height*) ?packing ?show () in
  let vbox = GPack.vbox ~packing: button#add () in
  let pix = 
    match frames with
    | Some [] | None -> None
    | Some frames ->
	Some (GMisc.pixmap (fst (List.hd frames))
		~packing: (vbox#pack ~expand: true ~fill: true) ())
  in
  let _ = 
    match label with
    | Some label -> 
	Some (GMisc.label ~text: label
		~packing: (vbox#pack ~expand: false ~fill: true) ())
    | None -> None
  in
  object (self)
  inherit GButton.button (Obj.magic button#as_widget : Gtk.button Gtk.obj)

  val mutable current_frames = match frames with Some fs -> fs | None -> []
  val mutable timeout = None

  method start_rotate =
    match pix, timeout, frames with
    | Some pix, None, Some frames when List.length frames > 1 ->
	let img, wait =
	  match current_frames with
	  | [img, wait] -> 
	      current_frames <- frames;
	      img, wait
	  | (img, wait)::fs -> 
	      current_frames <- fs;
	      img, wait
	  | _ -> assert false
	in
	let wait = if wait <= 0 then 100 else wait in
	pix#set_pixmap img;
	timeout <- 
	  Some (Timeout.add ~ms:wait ~callback: (fun () ->
	    timeout <- None;
	    self#start_rotate;
	    false));
	Gui.sync ()
    | _ -> ()

  method stop_rotate =
    match timeout with
    | Some id -> 
	Timeout.remove id; timeout <- None;
	begin match pix, frames with
	| Some pix, Some ((img,_wait)::_) -> pix#set_pixmap img; Gui.sync ()
	| _ -> ()
	end
    | None -> ()

end

let img_button = new img_button
