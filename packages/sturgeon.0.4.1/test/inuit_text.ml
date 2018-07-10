open Sturgeon
open Session
open Inuit.Cursor
open Inuit_widget

let () =
  Sturgeon_recipes_command.text_command @@ fun ~args:_ shell ->
  let k = Stui.create_cursor shell ~name:"print-server" in
  text k "Hi, how are you doing?\n";
  let counter = ref 0 in
  link k ">>>>> 0 <<<<<"
    (fun k' ->
       clear k';
       incr counter;
       text k' (string_of_int !counter));
  text k "\n";
  let _ = Check.make k in
  text k " Check me\n";
  text k "Edit me: ";
  let k' = ref null in
  let _ = Edit.make k ~on_change:(fun t ->
      let str = Edit.state t in
      clear !k';
      text !k' "UPPERCASED: ";
      text !k' (String.uppercase_ascii str);
    ) in
  text k "\n";
  k' := sub k;
  text k "\n";
  text k "Adjust me: ";
  let _ = Slider.make k in
  ()
