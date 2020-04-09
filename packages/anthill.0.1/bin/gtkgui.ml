open StdLabels
open Core
open Types
open Top
open Utility

type model = {
  env: Env.env;
  history : string list
}

let new_model dict = {
  history = [];
  env = Librepl.new_env dict
}

let utf8 s = Glib.Convert.convert s "UTF-8" "ISO-8859-1"

let eval env str =
  let open Env in
  let open Formatter in
  try
    match Parser.parse str with
    | Ok expr -> begin
        let op = Librepl.op_of_expr env expr in
        let e = {env with op} in
        let env', out = Eval.eval env expr in
        env', unlines @@ format_wordset e out
      end
    | Error m -> env, format_error m
  with
  | x -> env, format_exception x

let make_cell_view ~column ~title ~opts =
  let renderer = GTree.cell_renderer_text opts in
  let col = GTree.view_column ~title () in
  col#pack renderer;
  col#set_cell_data_func renderer
    (fun model row ->
       let str = model#get ~row ~column in
       renderer#set_properties [ `TEXT (utf8 str) ]);
  col

class history_widget ~model ~output ?packing ?show () =
  let frame = GBin.frame ~border_width:3 ~shadow_type:`IN ?packing () in
  let scrolled_win = GBin.scrolled_window ~packing:frame#add
      ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ()
  in
  let cols = new GTree.column_list in
  let val_col = cols#add Gobject.Data.string in
  let list_model = GTree.list_store cols in
  let make_view ~column = make_cell_view ~column ~title:""
      ~opts: [ `XALIGN 0.; `YPAD 1 ]
  in
  let val_col_view = make_view val_col in
  let view = GTree.view ~model:list_model ~packing:scrolled_win#add () in
  object(self)
    inherit GObj.widget_full frame#as_widget

    initializer
      self#update;
      ignore @@ view#append_column val_col_view;
      ignore @@ view#connect#after#row_activated ~callback: begin
        fun path vcol ->
          let it : Gtk.tree_iter = list_model#get_iter path in
          let v = list_model#get ~row:it ~column:val_col in
          let env', out = eval !model.env v in
          model := {!model with env = env'};
          output#set_text out
      end

    method update =
      list_model#clear ();
      List.iter ~f:(fun v ->
          let row = list_model#append () in
          list_model#set ~row ~column:val_col v;
        )
        (List.rev !model.history)
  end

class input_widget ~model ~history ~output ?packing ?show () =
  let frame = GBin.frame ~border_width:3 ~shadow_type:`IN ?packing () in
  let input = GText.view ~packing:frame#add ~editable:true () in
  object(self)
    inherit GObj.widget_full frame#as_widget

    initializer
      input#set_wrap_mode `CHAR;
      ignore @@ input#event#connect#key_press
        ~callback:(fun ev -> self#handle_key_press ev)

    method handle_key_press ev =
      let key = GdkEvent.Key.keyval ev in
      if key = GdkKeysyms._Return then
        let t = input#buffer#get_text () in
        let env', out = eval !model.env t in
        output#set_text out;
        model := {history = t :: !model.history; env = env'};
        history#update;
        input#buffer#set_text "";
        true
      else
        false
  end

class output_widget ?packing ?show () =
  let frame = GBin.frame ~border_width:3 ~shadow_type:`IN ?packing () in
  let scrolled_win = GBin.scrolled_window ~packing:frame#add
      ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ()
  in
  let output = GText.view ~packing:scrolled_win#add ~editable:false () in
  object(self)
    inherit GObj.widget_full frame#as_widget

    method set_text text =
      output#buffer#set_text text
  end


let () =
  let _locale = GMain.init ~setlocale:true () in
  let w = GWindow.window () in
  ignore @@ w#connect#destroy ~callback:GMain.quit;

  let vbox = GPack.vbox ~packing:w#add () in
  let hbox = GPack.hbox ~packing:vbox#add () in
  let vb1 = GPack.vbox ~packing:hbox#add () in
  let vb2 = GPack.vbox ~packing:hbox#add () in

  let dict = Trie.load_from_text_file "csw19.lower" in
  let model = ref (new_model dict) in
  let output = new output_widget ~packing:vb2#add () in
  let history = new history_widget ~packing:vb1#add ~model ~output () in
  let _input = new input_widget ~model ~history ~output ~packing:(vb1#pack ~expand:false) () in

  let quit = GButton.button ~label:"Quit" ~packing:vbox#pack () in
  ignore @@ quit#connect#clicked ~callback:GMain.quit;

  w#show ();
  GMain.main ()
