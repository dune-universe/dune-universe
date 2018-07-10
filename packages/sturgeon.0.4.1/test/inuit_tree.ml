open Sturgeon
open Session
open Inuit.Cursor
open Inuit_widget

let rec children prefix t =
  for i = 0 to 9 do
    let label = prefix ^ string_of_int i in
    text
      (Tree.add t ~children:(children label))
      label
  done

let () =
  Sturgeon_recipes_command.text_command
  @@ fun ~args shell ->
  let cursor = Stui.create_cursor shell ~name:"tree-server" in
  let nav =
    Nav.make "0 to 9" @@ fun {Nav. body; nav} ->
    text body "\n";
    children "/" (Tree.make body)
  in
  Nav.render nav cursor
