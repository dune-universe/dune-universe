(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: editor2.ml 1347 2007-06-20 07:40:34Z guesdon $ *)

open StdLabels

let _ = GMain.Main.init ()

let file_dialog ~title ~callback ?filename () =
  let sel = GWindow.file_selection ~title ~modal:true ?filename () in
  ignore (sel#cancel_button#connect#clicked ~callback:sel#destroy);
  ignore
    (sel#ok_button#connect#clicked ~callback:(fun () ->
         let name = sel#filename in
         sel#destroy ();
         callback name));
  sel#show ()

let input_channel b ic =
  let buf = String.create 1024 and len = ref 0 in
  while
    len := input ic buf 0 1024;
    !len > 0
  do
    Buffer.add_substring b buf 0 !len
  done

let with_file name ~f =
  let ic = open_in name in
  try
    f ic;
    close_in ic
  with exn ->
    close_in ic;
    raise exn

class editor ?packing ?show () =
  object (self)
    val text = GText.view ?packing ?show ()

    val mutable filename = None

    method text = text

    method load_file name =
      try
        let b = Buffer.create 1024 in
        with_file name ~f:(input_channel b);
        let s = Glib.Convert.locale_to_utf8 (Buffer.contents b) in
        let n_buff = GText.buffer ~text:s () in
        text#set_buffer n_buff;
        filename <- Some name;
        n_buff#place_cursor n_buff#start_iter
      with _ -> prerr_endline "Load failed"

    method open_file () = file_dialog ~title:"Open" ~callback:self#load_file ()

    method save_dialog () =
      file_dialog ~title:"Save" ?filename
        ~callback:(fun file -> self#output ~file)
        ()

    method save_file () =
      match filename with
      | Some file -> self#output ~file
      | None -> self#save_dialog ()

    method output ~file =
      try
        if Sys.file_exists file then Sys.rename file (file ^ "~");
        let s = text#buffer#get_text () in
        let oc = open_out file in
        output_string oc (Glib.Convert.locale_from_utf8 s);
        close_out oc;
        filename <- Some file
      with _ -> prerr_endline "Save failed"

    method get_text () = text#buffer#get_text ()
  end

let window = GWindow.window ~width:500 ~height:300 ~title:"editor" ()

let vbox = GPack.vbox ~packing:window#add ()

let menubar = GMenu.menu_bar ~packing:vbox#pack ()

let factory = new GMenu.factory ~accel_path:"<EDITOR2>/" menubar

let accel_group = factory#accel_group

let file_menu = factory#add_submenu "File"

let edit_menu = factory#add_submenu "Edit"

let scrollwin = GBin.scrolled_window ~packing:vbox#add ()

let editor = new editor ~packing:scrollwin#add ()

open Mlpost

let fig () =
  let fig = Box.tex (editor#get_text ()) in
  let fig = Box.center Point.origin fig in
  let fig = Box.scale (Num.bp 2.) fig in
  Box.draw fig

let width = 400

let height = 500

let window2 = GWindow.window ~width ~height ~title:"view" ()

let () =
  ignore (window2#connect#destroy ~callback:GMain.quit);
  ignore (window2#show ())

let new_pixmap width height =
  let drawable = GDraw.pixmap ~width ~height () in
  drawable#set_foreground `WHITE;
  drawable#rectangle ~x:0 ~y:0 ~width ~height ~filled:true ();
  drawable

let pm = ref (new_pixmap width height)

let need_update = ref true

let paint () =
  try
    let w, h = (float_of_int width, float_of_int height) in
    let fig = Picture.shift (Point.ptp (w /. 2., h /. 2.)) (fig ()) in
    let _ = Mlpost.Concrete.float_of_num (Picture.width fig) in
    let cr = Cairo_lablgtk.create !pm#pixmap in
    !pm#rectangle ~x:0 ~y:0 ~width ~height ~filled:true ();
    Cairost.emit_cairo cr (w, h) fig
  with _ -> ()

let refresh da =
  need_update := true;
  GtkBase.Widget.queue_draw da#as_widget

let expose da x y width height =
  let gwin = da#misc#window in
  let d = new GDraw.drawable gwin in
  d#put_pixmap ~x ~y ~xsrc:x ~ysrc:y ~width ~height !pm#pixmap

let expose_cb da ev =
  let area = GdkEvent.Expose.area ev in
  let module GR = Gdk.Rectangle in
  if !need_update (*&& editor#text#buffer#modified*) then paint ();
  expose da (GR.x area) (GR.y area) (GR.width area) (GR.height area);
  need_update := false;
  true

let button_ev da ev =
  match GdkEvent.get_type ev with
  | `BUTTON_RELEASE ->
      refresh da;
      true
  | _ -> false

let init packing =
  let da = GMisc.drawing_area ~width ~height ~packing () in
  da#misc#set_can_focus true;
  ignore (da#event#connect#expose (expose_cb da));
  da#event#add [ `BUTTON_RELEASE ];
  ignore (da#event#connect#button_release (button_ev da));
  ignore (editor#text#buffer#connect#changed ~callback:(fun _ -> refresh da));
  da

let dda =
  let dda = init window2#add in
  window2#show ();
  dda

(** Editor window *)

open GdkKeysyms

let _ =
  ignore (window#connect#destroy ~callback:GMain.quit);
  let factory =
    new GMenu.factory ~accel_path:"<EDITOR2 File>/////" file_menu ~accel_group
  in
  ignore (factory#add_item "Open" ~key:_O ~callback:editor#open_file);
  ignore (factory#add_item "Save" ~key:_S ~callback:editor#save_file);
  ignore (factory#add_item "Save as..." ~callback:editor#save_dialog);
  ignore (factory#add_separator ());
  ignore (factory#add_item "Quit" ~key:_Q ~callback:window#destroy);
  let factory =
    new GMenu.factory ~accel_path:"<EDITOR2 File>///" edit_menu ~accel_group
  in
  ignore
    (factory#add_item "Copy" ~key:_C ~callback:(fun () ->
         editor#text#buffer#copy_clipboard GMain.clipboard));
  ignore
    (factory#add_item "Cut" ~key:_X ~callback:(fun () ->
         GtkSignal.emit_unit editor#text#as_view GtkText.View.S.cut_clipboard));
  ignore
    (factory#add_item "Paste" ~key:_V ~callback:(fun () ->
         GtkSignal.emit_unit editor#text#as_view GtkText.View.S.paste_clipboard));
  ignore (factory#add_separator ());
  ignore
    (factory#add_check_item "Word wrap" ~active:false ~callback:(fun b ->
         editor#text#set_wrap_mode (if b then `WORD else `NONE)));
  ignore
    (factory#add_check_item "Read only" ~active:false ~callback:(fun b ->
         editor#text#set_editable (not b)));
  ignore
    (factory#add_item "Save accels" ~callback:(fun () ->
         GtkData.AccelMap.save "test.accel"));
  ignore (factory#add_item "Refresh" ~callback:(fun () -> refresh dda));
  ignore (window#add_accel_group accel_group);
  ignore
    (editor#text#event#connect#button_press ~callback:(fun ev ->
         let button = GdkEvent.Button.button ev in
         if button = 3 then (
           file_menu#popup ~button ~time:(GdkEvent.Button.time ev);
           true )
         else false));
  ignore (window#show ());
  let () = GtkData.AccelMap.load "test.accel" in
  GMain.main ()
