(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Jun Furuse, projet Cristal, INRIA Rocquencourt           *)
(*                                                                     *)
(*  Copyright 1999-2004,                                               *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: livsh.ml,v 1.10 2008/02/19 12:44:04 furuse Exp $ *)

open GPack
open Gui
open Livshtype
open Livmisc

let font =
  Gdk.Font.load "-*-helvetica-medium-r-normal-*-12-*-*-*-*-*-iso8859-1"

let dummy_pixmap =
  try
    GDraw.pixmap_from_xpm_d (* ~window: window (may hang...) *)
      ~colormap: colormap ~data: Deficon.data ()
  with
  | _ -> failwith "default icon does not exist..."

let joe_anim =
  lazy
    (try
       Some (Seq.load_sequence_as_pixmaps ~window: window#misc#window
               (Pathfind.find
                  ["~/.liv"; "/usr/lib/liv"; "/usr/local/lib/liv"; "."]
                  "faceanm.gif"))
     with
     | _ -> prerr_endline "There is no Joe's face!"; None)

let icon_width = 80
let icon_height = 60
let button_width = 100
let button_height = 80
let label_height = 16
let max_text = button_width * 9 / 10

class virtual icon_creator = object (self)
  val mutable icons = []

  method virtual activate : unit -> unit
  method virtual deactivate : unit -> unit
  method virtual set_text : string -> unit

  method add f =
    let was_empty = icons = [] in
    icons <- f :: icons;
    if was_empty
    then ignore (GMain.Timeout.add ~ms: 1 ~callback: self#make_icon)

  method make_icon () =
    begin match icons with
    | [] -> self#deactivate (); sync ()
    | f :: fs ->
      icons <- fs;
      f ();
      self#activate ();
      sync ();
      ignore (GMain.Timeout.add ~ms: 1 ~callback: self#make_icon)
    end;
    false

  method clear () = icons <- []
end

and icon ~dir ~name (req : icon_creator) =
  let ebox =
    GBin.event_box ~border_width: 0
      ~width: button_width ~height: (button_height + label_height) () in
  let vbox = lazy (GPack.vbox ~packing: ebox#add ()) in
  let pressed = ref (fun () -> ())
  and enter = ref (fun () -> ())
  and leave = ref (fun () -> ()) in
  let button = lazy (
    let b =
      GButton.button (* ~width:button_width
                        ~height:button_height ~border_width:0  *)
        ~packing: !!vbox#pack () in
    ignore (b#connect#pressed ~callback: !pressed);
    ignore (b#connect#enter ~callback: !enter);
    ignore (b#connect#leave ~callback: !leave);
    b) in
  let pix = lazy (
    GMisc.pixmap dummy_pixmap ~width:icon_width ~height:icon_height
      ~packing: !!button#add ()) in
  let label = lazy (
    let shorten_name name =
      let rec aux name =
        let name_dots = name ^ "..." in
        if Gdk.Font.string_width font name_dots > max_text then
          if name = "" then name_dots
          else aux (String.sub name 0 (String.length name - 1))
        else name_dots in
      if Gdk.Font.string_width font name > max_text then aux name else name in
    GMisc.label
      ~text: (shorten_name name)
      ~width:button_width ~packing: !!vbox#pack ~justify: `LEFT ()) in
  let typ = lazy (
    try
      let typ = Livshtype.guess (Filename.concat dir name) in
      match typ with
      | ContentType x ->
          begin
            match Mstring.split_str (fun c -> c = '/') x with
            | [mj; mn] -> mj, mn
            | _ -> assert false
          end
      | ContentEncoding x -> "encoding", x
      | Special m -> "special", m
    with
    | _ -> "?","?") in

  object (self)
    inherit GObj.widget_full ebox#as_widget

    method connect_pressed f = pressed := f
    method connect_enter f = enter := f
    method connect_leave f = leave := f

    method typ = !!typ

    val info_icon =
      Mylazy.make (fun () ->
        (* prerr_endline (Printf.sprintf "Icon(%s)" name); *)
        let info, pixmap = Icon.load_icon (Filename.concat dir name) !!typ in
        prog#unmap ();
        !!pix#set_pixmap pixmap;
        sync ();
        (* prerr_endline "done"; *)
        info, pixmap)

    method info = fst (Mylazy.force info_icon)
    method icon = snd (Mylazy.force info_icon)

    val mutable x = -1
    val mutable y = -1

    method position = x, y
    method set_position nx ny = x <- nx; y <- ny

    method name = name

    initializer
      let callback v = (fun _ ->
        (* we create vbox button pix and label if they are not available *)
        ignore !!vbox; ignore !!button; ignore !!pix; ignore !!label;
        begin match !info_icon with
        | Mylazy.Delayed _ ->
            req#add (fun () ->
              if !!button#misc#visible then
                (try ignore (self#icon) with _ -> ()))
        | _ -> ()
        end;
        v) in
      (*
        (* for the widget visible from the first *)
        ignore (ebox#misc#connect#draw ~callback: (fun _ ->
          (* prerr_endline (Printf.sprintf "draw(%s)" name); *)
          callback () ()));
      *)
      (* for newly appearing widgets *)
      ignore
        (ebox#event#connect#expose
           ~callback: (fun _ ->
             if ebox#misc#visible then begin
             (* prerr_endline (Printf.sprintf "expose(%s)" name); *)
               callback true () end else true))
end

class livsh init_dir func =
  (* widgets *)
  let win =
    GWindow.window ~allow_shrink: true ~allow_grow: true
      ~width: 100 ~height: 100 ~title: "liv shell" () in
  let style = win#misc#style in
  let _ =
    style#set_font font;
    win#misc#set_style style; in

  let vbox = vbox ~packing: win#add () in
  let vhbox = hbox ~packing: (vbox#pack ~expand: false ~fill: true) () in
  let toolbar =
    GButton.toolbar ~packing: (vhbox#pack ~expand: false ~fill: true) () in
  let _ =
    toolbar#insert_button ~text: "Back" ~tooltip: "Go back" () in
  let _ =
    toolbar#insert_button ~text: "Forward" ~tooltip: "Go forward" () in
  let _ =
    toolbar#insert_button ~text: "Reload" ~tooltip: "Reload" () in
  let _ =
    toolbar#insert_button ~text: "Home" ~tooltip: "Go to Home" () in
  let _ =
    GEdit.entry ~editable: true ~max_length: 256
      ~packing: (vhbox#pack ~expand: true ~fill: true) () in
  let joe =
    JWidget.img_button ?frames: !!joe_anim
      ~packing: (vhbox#pack ~expand: false ~fill: false) () in
  let viewport =
    GBin.scrolled_window
      ~hpolicy: `AUTOMATIC ~vpolicy: `ALWAYS
      ~packing: (vbox#pack ~expand: true ~fill: true) () in
  let fixed = GPack.fixed ~border_width: 2 ~width: 1 ~height: 1 () in
  (*
    let fixed =
      GPack.layout ~border_width: 2
        ~layout_width: 1000 ~layout_height: 1000 () in
  *)
  let _ = viewport#add_with_viewport fixed#coerce in

  let reconf_tout = ref None in

  object (self)
  inherit icon_creator
  inherit
    JWidget.status_bar
      ~packing: (vbox#pack ~expand: false ~fill: false)
      ~show: true () as status_bar

  method! activate () = joe#start_rotate; status_bar#activate ()
  method deactivate () = joe#stop_rotate; status_bar#set_fraction 0.0

  val mutable dir = init_dir
  val mutable items = []
  val mutable prevw = -1
  val mutable prevh = -1

  method reconfigure () =
    let content_window =
      Gdk.Window.get_parent (Gdk.Window.get_parent fixed#misc#window) in
    let vw,vh = Gdk.Drawable.get_size content_window in
    if vw <> prevw || vh <> prevh then begin
      joe#start_rotate;
      prevw <- vw;
      prevh <- vh;
(*
      prerr_endline "RECONFIG";
      prerr_endline (Printf.sprintf "get size done (%d,%d)" vw vh);
*)
      fixed#misc#unmap ();

      let mx = ref 0 and my = ref 0 in
      let x = ref 0 and y = ref 0 in
      let positions =
        List.map
          (fun _item ->
             let px = !x and py = !y in
             if !mx < !x + button_width then mx := !x + button_width;
             if !my < !y + button_height + label_height
             then my := !y + button_height + label_height;
             x := !x + button_width;
             if !x + button_width > vw then begin
               x := 0;
               y := !y + button_height + label_height
             end;
             px, py)
          items in

      let adj = viewport#vadjustment in
      adj#set_value 0.0;
      viewport#set_vadjustment adj;

      List.iter2
        (fun item (x,y) ->
           let ix, _iy = item#position in
           if ix < 0
           then fixed#put item#coerce ~x ~y
           else fixed#move item#coerce ~x ~y;
           item#set_position x y)
        items positions;

(*
      prerr_endline (Printf.sprintf "change <fixed> %dx%d" !mx !my);
*)
      fixed#misc#set_size_request ~width: !mx ~height: !my ();
      fixed#misc#map ();
    end

  method force_reconfigure () =
    prevw <- -1;
    prevh <- -1;
    self#reconfigure ()

  method open_dir d =
    joe#start_rotate;
    self#clear ();
    let num_files = ref 0 in
    List.iter (fun item -> item#destroy ()) items;
    items <- [];

    self#set_text ("Opening "^d^" ...");
    let dh = Unix.opendir d in
    let files =
      let files = ref [] in
      begin
        try
          while true do
            files := Unix.readdir dh :: !files;
            self#activate ();
            sync ();
            incr num_files;
          done with
        | End_of_file -> ()
      end;
      Unix.closedir dh;
      !files in
    self#set_text "";
    self#set_fraction 0.0;

    (* successfully loaded *)
    dir <- d;

    self#set_text ("Scanning " ^ d);
    let items_unsort =
      let cntr = ref 0 in
      List.fold_right
        (fun f acc ->
           incr cntr;
           self#set_fraction (float !cntr /. float !num_files); sync ();
           if f = "." || f = ".xvpics" then acc else begin
             let box = new icon ~dir: dir ~name: f (self :> icon_creator) in
             ignore (
                box#connect_pressed (fun () ->
                Livmisc.after
                  (fun () ->
                     if !active then begin
                       active := false;
                       let file =
                         Livmisc.normalize_filename (Filename.concat dir f) in
                       let typ = box#typ in
                       prerr_endline
                         ("Pressed " ^ file ^
                          " (" ^ fst typ ^ "/" ^ snd typ ^ ")");
                       match typ with
                       | "special", "dir"
                       | "special", "lnkdir" ->
                           self#open_dir file;
                           self#force_reconfigure ()
                       | t -> func file t
                     end)
                  (fun () -> active := true)));
             box#connect_enter
               (fun () -> self#set_text (Filename.concat dir f));
             box#connect_leave
               (fun () -> self#set_text "");
             box :: acc
           end)
        files [] in

    let sortf a b =
      let typval i =
        match i#typ with
        | "special", "dir" -> 0
        | "special", "lnkdir" -> 0
        | "special", _ -> 10
        | _ -> 100 in
      if typval a = typval b then a#name < b#name
      else typval a < typval b in

    items <-  Sort.list sortf items_unsort;
    self#set_text "";
    self#deactivate ()

  initializer
    ignore (win#connect#destroy ~callback:
      (fun () ->
         self#clear ();
         match !reconf_tout with
         | Some id -> GMain.Timeout.remove id
         | None -> ()));

    self#open_dir dir;
    (* This does not work well...
       ignore (win#connect#after#event#configure ~callback: (fun _ ->
              self#reconfigure (); true)); *)
    reconf_tout :=
      Some (GMain.Timeout.add ~ms: 500
              ~callback: (fun _ -> self#reconfigure (); true));
    win#set_default_size ~width: (button_width * 13 / 2)
      ~height: ((button_height + label_height) * 9 / 2);
    win#show ();
end
