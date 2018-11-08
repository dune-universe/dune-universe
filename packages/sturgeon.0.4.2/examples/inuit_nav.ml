open Sturgeon
open Inuit.Cursor
open Inuit_widget

let () =
  Sturgeon_recipes_command.text_command @@ fun ~args shell ->
  Session.cancel args;
  let k = Stui.create_cursor shell ~name:"nav-server" in
  let nav =
    Nav.make "Épiménide" @@ fun {Nav. title = _; body; nav} ->
    text body "Je mens.\n\n";
    link body "- C'est vrai."
      (fun _ -> Nav.push nav "C'est vrai !" @@
        fun {Nav. body; _} -> text body "C'est faux.");
    text body "\n";
    link body "- C'est faux."
      (fun _ -> Nav.push nav "C'est faux !" @@
        fun {Nav. body; _} -> text body "C'est vrai.");
    text body "\n"
  in
  Nav.render nav k
